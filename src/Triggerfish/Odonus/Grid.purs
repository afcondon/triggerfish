-- | The Odonus grid + four-head bank, Hainbach dress. The 16 cells are shown
-- | as parameter-major small multiples — a NOTE field of value knobs, then
-- | GATE / SKIP / GLIDE / LENGTH fields — over a bank of four playheads; each
-- | head carries a René-style access **pattern** shown as a small-multiple
-- | thumbnail beside its direction / speed / interval knobs, and a 16-switch
-- | head-activation matrix cuts between playhead combinations. Aesthetic:
-- | Swiss rigor × vintage-lab materiality (see BRIEF.md).
module Triggerfish.Odonus.Grid (component) where

import Prelude

import Data.Array (any, deleteAt, elem, filter, find, head, length, mapWithIndex, null, range, updateAt, (!!))
import Data.Foldable (foldl, for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String.Common (joinWith)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Timer (setInterval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Subscription as HS
import Triggerfish.Odonus.Model as M
import Triggerfish.Odonus.Marbles as Marbles
import Triggerfish.Odonus.Gen as Gen
import Triggerfish.Ui.Pointer as Pointer
import Binnacle as Binnacle
import Binnacle.Clock as Clock
import Binnacle.Midi as Midi
import Binnacle.Scheduler as Scheduler
import Binnacle.Transport as Transport
import Reef.Protocol (encodeOdonus)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent as ME
import Triggerfish.Odonus.Grid.Types
  ( Action(..), KnobTarget(..), SourceTag(..), State, applyTarget, genDefaultAmt, genDefaultRate, genKinds, genLabel
  , marblesPadId, setAmt, setRate, targetRange, toggleGen )
import Triggerfish.Odonus.Grid.Widgets (clampI, style)
import Triggerfish.Odonus.View.Scope (scopePanel)
import Triggerfish.Odonus.View.Key (quantizerPanel)
import Triggerfish.Odonus.View.Playheads (playheadsPanel)
import Triggerfish.Odonus.View.Grid (gridPanel)
import Triggerfish.Odonus.Patch (capturePatch, loadText, patchText, recallText)
import Triggerfish.Odonus.Store as Store
import Triggerfish.Odonus.Lepidoptera (parsePatch, printPatch)
import Triggerfish.SourceQuery (Query(..))
import Triggerfish.Odonus.View.Generate (generatePanel)
import Triggerfish.Odonus.View.Scenes (scenesPanel, sceneName)

component :: forall i o m. MonadAff m => H.Component Query i o m
component =
  H.mkComponent
    { initialState: \_ ->
        { odo: M.defaultOdonus, running: false, master: false, dragging: Nothing, dragSub: Nothing
        , notes: [], binnacle: Nothing, nowMicros: 0.0
        , midiOut: Nothing, midiName: "…", clockTempo: 120.0, clockLocked: false
        , clockBeat: 0.0, clockBar: 0, anchorCount: 0
        , scenes: [], sceneNameInput: "", chain: false, sceneIx: 0, sceneBarAnchor: 0, barsPerScene: 4
        , stepDiv: 1, headNote: [ Nothing, Nothing, Nothing, Nothing ]
        , swing: 0.0, velHumanize: 12
        , gen: map (\k -> { kind: k, on: false, rate: genDefaultRate k, amt: genDefaultAmt k }) genKinds
        , genSpread: 0.5, genBias: 0.5, genSeed: Marbles.seedFrom 1
        -- SOURCE folds away by default: the dedicated TIDAL tab is the
        -- one-stop view of the whole setup; Odonus's own eDSL pane is for
        -- when you want to inspect just this module.
        , collapsed: [ "SOURCE" ], lastTap: "", lastTapMicros: 0.0
        , voiceChords: [], follow: Nothing, source: SScale }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction, handleQuery = handleQuery, initialize = Just Initialize }
    }

-- | Answer the shell: the current eDSL (TIDAL tab), or adopt the rack's shared
-- | free-run baseline so all modules share a downbeat with no rig.
handleQuery :: forall o m a. MonadAff m => Query a -> H.HalogenM State Action () o m (Maybe a)
handleQuery = case _ of
  AskSource reply -> do
    s <- H.get
    pure (Just (reply (patchText s)))
  SyncFree startMicros tempo next -> do
    s <- H.get
    for_ s.binnacle \bin ->
      liftEffect (Clock.setFreeBaseline (Binnacle.clock bin) { startMicros, tempo })
    pure (Just next)
  -- The Vetula bridge: drive the chord quantiser from Vetula's progression.
  FeedChords pcs next -> do
    H.modify_ \s -> s { odo = M.setChordFeed pcs s.odo }
    pure (Just next)
  -- The LIVE Vetula→Odonus follow bridge: store the latest poll of Odonus-bound
  -- voice chords, then re-derive the followed chord (a no-op overlay if nothing
  -- is followed or the followed voice has gone away).
  FeedVoiceChords vcs next -> do
    H.modify_ \s ->
      let
        s1 = s { voiceChords = vcs }
        -- When Vetula is the chosen source, keep a valid follow as voices come and
        -- go (adopt the first if none is picked or the picked one vanished), so a
        -- voice bound while Odonus waits is followed automatically.
        s2 = if s1.source == SVetula then s1 { follow = keepOrFirst s1.follow vcs } else s1
      in
        recomputeFollow s2
    pure (Just next)
  -- The shell's master transport. Silence held notes if we were sounding (armed)
  -- and master is now stopping us.
  SetMaster m next -> do
    st <- H.get
    let wasSounding = st.master && st.running
        nowSounding = m && st.running
    when (wasSounding && not nowSounding) $ liftEffect $ silenceHeld st.midiOut st.headNote
    H.modify_ \s -> s
      { master = m
      , headNote = if wasSounding && not nowSounding then map (const Nothing) s.headNote else s.headNote }
    pure (Just next)
  -- A5 library manager: Odonus's saved SCENES are its named presets. LoadEntry
  -- cold-loads a scene (hard playhead reset); import adds a scene (parsePatch
  -- self-guards on `odonusPatch`).
  AskLibrary reply -> do
    s <- H.get
    pure (Just (reply s.scenes))
  LoadEntry i next -> do
    H.modify_ \s -> case s.scenes !! i of
      Just sc -> loadText sc.text s
      Nothing -> s
    persistAll
    pure (Just next)
  ImportText txt reply -> case parsePatch txt of
    Just p -> do
      H.modify_ \s -> s { scenes = s.scenes <> [ { name: p.name, text: printPatch p } ] }
      persistAll
      pure (Just (reply true))
    Nothing -> pure (Just (reply false))

-- | Run the action, then persist the live patch — except for the high-frequency
-- | / non-authoring actions (the clock tick, the river frame, a knob DRAG in
-- | flight, MIDI readiness, and Initialize itself, which has just restored).
-- | DragEnd is NOT excluded, so a knob edit persists once it settles.
handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction a = do
  dispatch a
  case a of
    Frame -> pure unit
    Step _ -> pure unit
    DragMove _ -> pure unit
    MidiReady _ _ -> pure unit
    Initialize -> pure unit
    SetSceneName _ -> pure unit   -- per-keystroke; nothing authoring changed yet
    _ -> persistAll

-- | Persist the live working patch + the named scene library.
persistAll :: forall o m. MonadAff m => H.HalogenM State Action () o m Unit
persistAll = do
  s <- H.get
  liftEffect (Store.saveAll { live: patchText s, scenes: s.scenes })

dispatch :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
dispatch = case _ of
  Initialize -> do
    -- Connect to the rig. Binnacle's clock free-runs at 120 until the
    -- Link anchor arrives, then phase-locks — so Triggerfish runs solo
    -- without the rig, and joins the ensemble the moment it's up.
    bin <- liftEffect $ Binnacle.connect { url: rigUrl, tempo: 120.0 }
    -- The lookahead scheduler drives both model advance and audio.
    { emitter: stepE, listener: stepL } <- liftEffect HS.create
    _ <- H.subscribe stepE
    _ <- liftEffect $ Scheduler.startGrid (Binnacle.clock bin) gridCfg \tick ->
      HS.notify stepL (Step tick)
    -- A separate ~30fps ticker scrolls the river smoothly.
    frameE <- frameTimer
    _ <- H.subscribe frameE
    -- Web MIDI out (async permission prompt) — e.g. an IAC bus into Ableton.
    { emitter: midiE, listener: midiL } <- liftEffect HS.create
    _ <- H.subscribe midiE
    liftEffect $ Midi.requestAccess \maccess -> case maccess of
      Just access -> do
        mout <- Midi.findOutput access midiPortName
        names <- Midi.outputNames access
        let nm = case mout of
              Just _ -> midiPortName <> " ✓"
              Nothing -> "no '" <> midiPortName <> "' — ports: " <> joinWith ", " names
        HS.notify midiL (MidiReady mout nm)
      Nothing -> HS.notify midiL (MidiReady Nothing "unavailable")
    H.modify_ _ { binnacle = Just bin }
    -- Restore the saved scene library + the live working patch (each is
    -- Lepidoptera text; unparseable / absent storage falls back to defaults).
    msaved <- liftEffect Store.loadAll
    for_ msaved \sv -> do
      H.modify_ _ { scenes = sv.scenes }
      H.modify_ (loadText sv.live)
  Step tick -> do
    st <- H.get
    -- Global step divider: the scheduler ticks on a fine 1/16 grid; advance the
    -- model only every stepDiv ticks, so STEP LENGTH sets what a 1× head plays.
    when (st.master && st.running && tick.index `mod` st.stepDiv == 0) do
      let
        -- The randomisation matrix fires BEFORE the heads read, so any mutated
        -- value is what plays this step. Each source drifts one notch at a time.
        g = Gen.runGen
              { gen: st.gen, spread: st.genSpread, bias: st.genBias
              , odo: st.odo, seed: st.genSeed }
        -- The chord progression advances on its own clock, before the heads read,
        -- so the new chord is what this step's notes quantize to.
        o1 = if g.odo.chord.on then M.tickChord g.odo else g.odo
        -- Heads the generator just silenced (HEADS source) get a note-off below,
        -- so a glide note can't stick on a voice that's now muted.
        muteOf o h = maybe true _.mute (o.heads !! h)
        newlyMuted = filter (\h -> not (muteOf st.odo h) && muteOf g.odo h) (range 0 3)
        r = M.stepEmit o1
        msPerBeat = 60000.0 / max 30.0 st.clockTempo
        -- One model step in ms (a 1× head's note spacing at this STEP LENGTH).
        stepMs = (0.25 * toNumber st.stepDiv) * msPerBeat
        modelStep = tick.index / st.stepDiv
        -- Swing: lag the off-beat (odd) model steps by a fraction of a step, so
        -- the grid breathes instead of being metronomic. Applied to the audible
        -- onset (and the scope), not the model advance.
        swingMs = if modelStep `mod` 2 == 1 then st.swing * stepMs else 0.0
        emitDelay = tick.delayMs + swingMs
        -- Accent the beat (every 4th model step) so it isn't dead-flat.
        accent = if modelStep `mod` 4 == 0 then 14 else 0
        -- A non-glide note's length scales with this head's note-spacing (so it
        -- breathes with the tempo / step length) rather than a fixed blip.
        gateMsFor f =
          let spd = maybe 1.0 M.speedOf (r.odo.heads !! f.headIdx)
          in stepMs / max 1.0 spd * (toNumber r.odo.gatePct / 100.0) * toNumber f.dur
        prevOf h = join (st.headNote !! h)
        -- Velocity: the cell's own base + beat accent + seeded humanise
        -- (±velHumanize). Drawn from the same PRNG as the generators, threaded on
        -- after Gen, so per-cell VEL authoring sets the contour the groove rides.
        velStep acc f =
          let { u, seed } = Marbles.nextRand acc.seed
              hum = round ((u - 0.5) * 2.0 * toNumber st.velHumanize)
              v = clampI 1 127 (f.vel + accent + hum)
          in { items: acc.items <> [ { f, v } ], seed }
        velied = foldl velStep { items: [], seed: g.seed } r.fired
        firedV = velied.items
      -- Silence any voice the generator muted this step.
      for_ st.midiOut \out -> liftEffect $ for_ newlyMuted \h -> case join (st.headNote !! h) of
        Just n -> Midi.noteOffAt out { channel: h, note: n, delayMs: 0.0 }
        Nothing -> pure unit
      -- Emit MIDI with per-head legato: glide cells HOLD until the next note
      -- (tie if same pitch, portamento-slide if different); non-glide cells are
      -- gated notes whose length scales with tempo.
      for_ st.midiOut \out -> liftEffect $ for_ firedV \fv ->
        emitNote out emitDelay (gateMsFor fv.f) fv.v (prevOf fv.f.headIdx) fv.f
      let
        -- A glide note stays held (its pitch); a gated note auto-ends.
        nextNote f = if f.glide then Just f.pitch else Nothing
        newHeadNote = foldl
          (\arr f -> fromMaybe arr (updateAt f.headIdx (nextNote f) arr))
          st.headNote r.fired
        -- Clear the held-note slots of voices the generator just muted.
        clearedHeadNote = foldl (\arr h -> fromMaybe arr (updateAt h Nothing arr)) newHeadNote newlyMuted
        fresh = map (\f -> { pitch: f.pitch, headIdx: f.headIdx
                           , fireUnixMicros: tick.fireUnixMicros + swingMs * 1000.0 }) r.fired
      H.modify_ \s -> s
        { odo = r.odo, notes = fresh <> s.notes, headNote = clearedHeadNote
        , genSeed = velied.seed }
  Frame -> do
    st <- H.get
    case st.binnacle of
      Just bin -> do
        now <- liftEffect $ Clock.unixMicrosNow (Binnacle.clock bin)
        r <- liftEffect $ Clock.read (Binnacle.clock bin)
        H.modify_ \s ->
          let
            base = s
              { nowMicros = now
              , clockTempo = r.tempo
              , clockLocked = r.locked
              , clockBeat = r.beat
              , clockBar = r.bar
              , anchorCount = r.anchorCount
              , notes = filter (\n -> (now - n.fireUnixMicros) < windowMicros) s.notes
              }
            -- Chain mode: advance to the next scene once barsPerScene bars have
            -- elapsed, carrying playhead phase across the swap.
            advance = s.chain && not (null s.scenes)
              && (r.bar - s.sceneBarAnchor) >= s.barsPerScene
          in
            if advance then
              let ni = (s.sceneIx + 1) `mod` length s.scenes
              in case s.scenes !! ni of
                Just sc -> (recallText sc.text base) { sceneIx = ni, sceneBarAnchor = r.bar }
                Nothing -> base
            else base
      Nothing -> pure unit
  MidiReady mout nm -> H.modify_ _ { midiOut = mout, midiName = nm }
  -- The run button is now a sticky ARM toggle. Odonus sounds only when armed AND
  -- the shell's master is playing; when that combination goes false (disarm while
  -- playing, or master stop) we note-off every held note so nothing sticks on.
  ToggleRun -> do
    st <- H.get
    let wasSounding = st.master && st.running
        nowSounding = st.master && not st.running
    when (wasSounding && not nowSounding) $ liftEffect $ silenceHeld st.midiOut st.headNote
    H.modify_ \s -> s
      { running = not s.running
      , headNote = if wasSounding && not nowSounding then map (const Nothing) s.headNote else s.headNote }
  ToggleGlide i -> H.modify_ \s -> s { odo = M.toggleGlide i s.odo }
  ToggleGate i -> H.modify_ \s -> s { odo = M.toggleGate i s.odo }
  ToggleSkip i -> H.modify_ \s -> s { odo = M.toggleSkip i s.odo }
  SetAllNotes v -> H.modify_ \s -> s { odo = M.setAllNotes v s.odo }
  SeedMelody -> H.modify_ \s ->
    let r = Gen.seedMelody (M.harmonyPCs s.odo) s.odo s.genSeed
    in s { odo = r.odo, genSeed = r.seed }
  ToggleHeadMute h -> do
    st <- H.get
    -- Muting a head that's holding a note → kill it (it won't emit again to
    -- end itself), and clear its held-note slot.
    let willMute = maybe false (\hd -> not hd.mute) (st.odo.heads !! h)
    when willMute $ for_ st.midiOut \out -> case join (st.headNote !! h) of
      Just n -> liftEffect $ Midi.noteOffAt out { channel: h, note: n, delayMs: 0.0 }
      Nothing -> pure unit
    H.modify_ \s -> s
      { odo = M.toggleHeadMute h s.odo
      , headNote = fromMaybe s.headNote (updateAt h Nothing s.headNote) }
  SetHeadMask mask -> do
    st <- H.get
    -- Heads this combination silences (held now, muted next) get a note-off,
    -- and their held-note slots clear, so glide notes never stick.
    let nextOdo = M.setHeadMask mask st.odo
        nowMuted h = maybe true _.mute (nextOdo.heads !! h)
    forWithIndex_ st.headNote \h mn -> case mn of
      Just n | nowMuted h -> for_ st.midiOut \out ->
        liftEffect $ Midi.noteOffAt out { channel: h, note: n, delayMs: 0.0 }
      _ -> pure unit
    H.modify_ \s -> s
      { odo = nextOdo
      , headNote = mapWithIndex (\h mn -> if nowMuted h then Nothing else mn) s.headNote }
  CyclePattern h -> H.modify_ \s -> s { odo = M.cyclePattern h s.odo }
  SetHeadDir h d -> H.modify_ \s -> s { odo = M.setHeadDir h d s.odo }
  UnifyHeads -> H.modify_ \s -> s { odo = M.unifyHeads s.odo }
  PhaseShift d -> H.modify_ \s -> s { odo = M.nudgeOffsets d s.odo }
  CycleScaleType dir -> H.modify_ \s -> s { odo = M.cycleScaleType dir s.odo }
  ToggleDist -> H.modify_ \s -> s { odo = M.toggleDistribution s.odo }
  ToggleChord -> H.modify_ \s ->
    if tapBounced "chord" s then s else (markTap "chord" s) { odo = M.toggleChord s.odo }
  ChordRoll -> H.modify_ \s ->
    let r = Gen.rollChords M.numChordTable s.genSeed
    in s { odo = M.setChordPicks r.picks s.odo, genSeed = r.seed }
  -- The KEY pane's pitch-source radio — an explicit `source` intent. Scale =
  -- overlay off; Chord = the internal McMullen progression (overlay on); Vetula =
  -- follow a voice. Selecting Vetula always sticks (even with no voice yet): it
  -- adopts the first bound voice if available, else stays selected-but-inactive
  -- (recomputeFollow leaves the overlay off; the sub-section shows it waiting).
  SetSource SScale -> H.modify_ \s ->
    s { source = SScale, follow = Nothing, odo = s.odo { chord = s.odo.chord { on = false } } }
  SetSource SChord -> H.modify_ \s ->
    s { source = SChord, follow = Nothing
      , odo = s.odo { chord = s.odo.chord { on = true, feed = [], ix = 0, phase = 0 } } }
  SetSource SVetula -> H.modify_ \s ->
    recomputeFollow (s { source = SVetula, follow = keepOrFirst s.follow s.voiceChords })
  -- Pick a voice to follow, or "free" (Nothing = stay on Vetula but unfollowed →
  -- inactive). recomputeFollow turns the overlay on/off accordingly.
  SetFollow mfid -> H.modify_ \s -> recomputeFollow (s { follow = mfid })
  SetRoot pc -> H.modify_ \s -> s { odo = M.setRoot pc s.odo }
  SetOctave n -> H.modify_ \s -> s { odo = M.setOctaveShift n s.odo }
  SetDegShift n -> H.modify_ \s -> s { odo = M.setDegShift n s.odo }
  ToggleScaleNote pc -> H.modify_ \s -> s { odo = M.toggleScaleNote pc s.odo }
  -- Capture the WHOLE current setup under the typed name (or an auto-name), as
  -- its Lepidoptera text — the named, recallable preset. (persistAll runs in the
  -- handleAction wrapper.)
  CaptureScene -> H.modify_ \s ->
    let nm = if s.sceneNameInput == "" then sceneName s else s.sceneNameInput
    in s { scenes = s.scenes <> [ { name: nm, text: printPatch ((capturePatch s) { name = nm }) } ]
         , sceneNameInput = "" }
  SetSceneName n -> H.modify_ _ { sceneNameInput = n }
  RecallScene i -> H.modify_ \s -> case s.scenes !! i of
    Just sc -> (recallText sc.text s) { sceneIx = i }
    Nothing -> s
  DeleteScene i -> H.modify_ \s -> s { scenes = fromMaybe s.scenes (deleteAt i s.scenes) }
  ToggleChain -> H.modify_ \s -> s { chain = not s.chain, sceneBarAnchor = s.clockBar }
  BumpBars d -> H.modify_ \s -> s { barsPerScene = clampI 1 32 (s.barsPerScene + d) }
  SetStepDiv d -> H.modify_ \s -> s { stepDiv = d }
  KnobDown target startVal -> do
    sid <- setupDrag
    H.modify_ _ { dragging = Just { target, startY: 0, startVal }, dragSub = Just sid }
  DragMove clientY -> do
    st <- H.get
    case st.dragging of
      Just drag
        | drag.startY == 0 ->
            H.modify_ _ { dragging = Just drag { startY = clientY } }
        | otherwise -> do
            let
              -- cell.note is a discrete index now; its range is span × set
              -- cardinality, derived from the live odo rather than the static table.
              r = case drag.target of
                    CellNote _ -> { lo: 0, hi: M.cellIndexMax st.odo }
                    _ -> targetRange drag.target
              delta = round (toNumber (drag.startY - clientY) * toNumber (r.hi - r.lo) / 140.0)
              newVal = clampI r.lo r.hi (drag.startVal + delta)
            case drag.target of
              GenRate kind -> H.modify_ \s -> s { gen = setRate kind newVal s.gen }
              GenAmt kind -> H.modify_ \s -> s { gen = setAmt kind newVal s.gen }
              SwingAmt -> H.modify_ \s -> s { swing = toNumber newVal / 100.0 }
              VelHuman -> H.modify_ \s -> s { velHumanize = newVal }
              _ -> H.modify_ \s -> s { odo = applyTarget drag.target newVal s.odo }
      _ -> pure unit
  DragEnd -> do
    st <- H.get
    case st.dragSub of
      Just sid -> H.unsubscribe sid
      Nothing -> pure unit
    H.modify_ _ { dragging = Nothing, dragSub = Nothing }
  ToggleGen kind -> H.modify_ \s ->
    -- Same double-dispatch guard as the panels: a flip-toggle would cancel
    -- itself if the 30fps re-render replays the click, so debounce per source.
    let k = "g:" <> genLabel kind
    in if tapBounced k s then s else (markTap k s) { gen = toggleGen kind s.gen }
  MarblesPad cx cy btns ->
    -- Wired to mousedown + mousemove; act only while the button is held.
    -- X = BIAS (peak's horizontal position in the histogram, low→high notes);
    -- Y = SPREAD, inverted so up = wider.
    when (btns == 1) do
      { x, y } <- liftEffect $ Pointer.padNorm marblesPadId cx cy
      H.modify_ \s -> s { genBias = x, genSpread = 1.0 - y }
  MarblesRoll -> H.modify_ \s ->
    let g = Gen.rollAllNotes s.genSpread s.genBias s.odo s.genSeed
    in s { odo = g.odo, genSeed = g.seed }
  -- The header always collapses, the tab always expands. Each is idempotent
  -- AND debounced per-label: a single click double-dispatches (one direct, one
  -- via the eval queue) with a re-render between, so the 2nd event lands on the
  -- swapped element and would otherwise undo the 1st. `panelBounced` drops a
  -- same-label toggle within 120ms (the doubled events are near-instant;
  -- deliberate re-clicks are slower).
  CollapsePanel label -> H.modify_ \s ->
    if tapBounced label s then s
    else (markTap label s)
      { collapsed = if elem label s.collapsed then s.collapsed else s.collapsed <> [ label ] }
  ExpandPanel label -> H.modify_ \s ->
    if tapBounced label s then s
    else (markTap label s) { collapsed = filter (_ /= label) s.collapsed }
  PushToRig -> do
    -- Serialize the whole Odonus record with the shared reef codec and push it
    -- over the already-open rig WebSocket (Binnacle's socket). The BEAM decodes
    -- it with the SAME codec (Reef.Protocol) and runs it on the reef engine —
    -- the frontend->wire->engine path that proves "one definition, two runtimes".
    st <- H.get
    for_ st.binnacle \bin ->
      liftEffect $ Transport.send (Binnacle.socket bin) ("reef-odonus " <> encodeOdonus st.odo)
  HushRig -> do
    -- Stop the reef voice (and everything else) on the rig via the existing
    -- hush verb, over the same socket the push used.
    st <- H.get
    for_ st.binnacle \bin ->
      liftEffect $ Transport.send (Binnacle.socket bin) "hush"

-- | True if this target was just toggled (< 120ms ago) — the second of a
-- | double-dispatched click. nowMicros advances via the Frame loop. Shared by
-- | the accordion panels and the GENERATE source LEDs.
tapBounced :: String -> State -> Boolean
tapBounced k s = k == s.lastTap && (s.nowMicros - s.lastTapMicros) < 120000.0

markTap :: String -> State -> State
markTap k s = s { lastTap = k, lastTapMicros = s.nowMicros }

-- | Re-derive the chord overlay from the follow selection + last poll. A followed
-- | voice's chord becomes a one-element feed (overlay on; a vanished voice clears
-- | it). With no follow, the overlay is owned by the chosen source: Vetula
-- | selected-but-unfollowed is INACTIVE (overlay off — it's waiting for a voice);
-- | Scale (off) / Chord (on) keep theirs, so the 100ms poll can't clobber them.
recomputeFollow :: State -> State
recomputeFollow s = case s.follow of
  Just fid -> s { odo = M.followChord (_.pcs <$> find (\vc -> vc.id == fid) s.voiceChords) s.odo }
  Nothing -> case s.source of
    SVetula -> s { odo = s.odo { chord = s.odo.chord { on = false } } }
    _ -> s

-- | Keep the current followed voice if it still exists, else adopt the first
-- | bound voice (or none) — used to auto-track a voice for the Vetula source.
keepOrFirst :: Maybe Int -> Array { id :: Int, pcs :: Array Int } -> Maybe Int
keepOrFirst cur vcs = case cur of
  Just fid | any (\vc -> vc.id == fid) vcs -> Just fid
  _ -> map _.id (head vcs)

-- | The rig WebSocket (purerl-tidal). Binnacle subscribes to the Link
-- | anchor here and relays gates/CV to es9-daemon.
rigUrl :: String
rigUrl = "ws://127.0.0.1:3012/ws"

-- | One Odonus step = a 16th note; schedule ~120ms ahead, poll at 25ms.
gridCfg :: Scheduler.GridConfig
gridCfg = { stepBeats: 0.25, lookaheadMs: 120.0, tickMs: 25 }

-- | The per-head legato state machine for one emitted note. `prev` is the note
-- | currently held on this head's channel (from a previous glide), if any.
-- |   glide + same pitch  → tie: leave the held note ringing (no retrigger)
-- |   glide + diff pitch  → slide: porta-on, note-on new, note-off old (overlap)
-- |   glide + nothing held → start a held note (no auto-off)
-- |   no glide             → gated note: end any held note, then a roll of
-- |                          `ratchet` retriggers filling `gateMs` (1 = a single
-- |                          hit; the old behaviour). Glide and ratchet don't mix
-- |                          (a slide is a single sustained event).
emitNote :: Midi.MidiOut -> Number -> Number -> Int -> Maybe Int -> M.Fired -> Effect Unit
emitNote out delayMs gateMs vel prev f =
  let h = f.headIdx
      p = f.pitch
      portaOn = do
        Midi.sendCC out { channel: h, controller: 65, value: 127 }
        Midi.sendCC out { channel: h, controller: 5, value: 40 }
      portaOff = Midi.sendCC out { channel: h, controller: 65, value: 0 }
      -- Subdivide the gate window into `ratchet` evenly-spaced hits; each hit
      -- sustains 85% of its slot so the retriggers stay articulate.
      rat = if f.ratchet < 1 then 1 else f.ratchet
      ratchetNote =
        if rat <= 1 then Midi.scheduleNote out { channel: h, note: p, velocity: vel, delayMs, durMs: gateMs }
        else
          let sub = gateMs / toNumber rat
          in for_ (range 0 (rat - 1)) \k ->
               Midi.scheduleNote out
                 { channel: h, note: p, velocity: vel
                 , delayMs: delayMs + toNumber k * sub, durMs: sub * 0.85 }
  in case prev, f.glide of
    Just q, true | q == p -> pure unit                         -- tie
    Just q, true -> do                                          -- slide
      portaOn
      Midi.noteOnAt out { channel: h, note: p, velocity: vel, delayMs }
      Midi.noteOffAt out { channel: h, note: q, delayMs: delayMs + 60.0 }
    Just q, false -> do                                         -- gated, end held
      Midi.noteOffAt out { channel: h, note: q, delayMs }
      portaOff
      ratchetNote
    Nothing, true -> do                                         -- start held
      portaOff
      Midi.noteOnAt out { channel: h, note: p, velocity: vel, delayMs }
    Nothing, false -> do                                        -- gated
      portaOff
      ratchetNote

-- | Note-off every held note (e.g. on Stop) and clear the held-note table.
silenceHeld :: Maybe Midi.MidiOut -> Array (Maybe Int) -> Effect Unit
silenceHeld mout held = for_ mout \out ->
  forWithIndex_ held \h mn -> case mn of
    Just n -> Midi.noteOffAt out { channel: h, note: n, delayMs: 0.0 }
    Nothing -> pure unit

-- | MIDI output port (substring match). On macOS enable the IAC Driver in
-- | Audio MIDI Setup and receive this bus in Ableton; each head sends on
-- | its own channel (I→1 … IV→4). (The es9 modular path lives in
-- | Binnacle.Output for when the rig is patched.)
midiPortName :: String
midiPortName = "IAC"

-- | Drop monitor notes once they've scrolled off the left edge (~8s).
windowMicros :: Number
windowMicros = 8000000.0

-- | ~30fps UI ticker that scrolls the scope + refreshes the clock readout.
-- | Driven by `setInterval` (a browser timer) rather than a forked Aff
-- | `forever` loop — the latter can silently fail to run for the component's
-- | life (the symptom: audio tracked Link via the scheduler's own
-- | setInterval while this loop was dead, freezing the whole UI). This is the
-- | same robust mechanism Scheduler.startGrid uses.
frameTimer :: forall m. MonadAff m => m (HS.Emitter Action)
frameTimer = liftEffect do
  { emitter, listener } <- HS.create
  _ <- setInterval 33 (HS.notify listener Frame)
  pure emitter

setupDrag :: forall o m. MonadAff m => H.HalogenM State Action () o m H.SubscriptionId
setupDrag =
  H.subscribe $ HS.makeEmitter \emit -> do
    moveFn <- eventListener \e -> case ME.fromEvent e of
      Just me -> emit (DragMove (ME.clientY me))
      Nothing -> pure unit
    upFn <- eventListener \_ -> emit DragEnd
    target <- Window.toEventTarget <$> window
    addEventListener (EventType "mousemove") moveFn false target
    addEventListener (EventType "mouseup") upFn false target
    pure do
      removeEventListener (EventType "mousemove") moveFn false target
      removeEventListener (EventType "mouseup") upFn false target

-- ---------------------------------------------------------------------------
-- render
-- ---------------------------------------------------------------------------

-- | Fullscreen, no margins: a right→left signal chain of full-height panels,
-- | mirroring the scope's leftward note-flow. GRID ← PLAYHEADS ← QUANTIZE ←
-- | SCOPE. The grid authors integers, the playheads shift them in degree-space,
-- | the quantizer collapses degrees to pitches, the scope shows them flowing out
-- | the left. (The eDSL SOURCE pane was retired — the patch's source now lives
-- | only on the shell's Tidal page; `AskSource`/`patchText` still answer it.)
render :: forall m. State -> H.ComponentHTML Action () m
render s =
  HH.div
    -- The whole surface is non-selectable: knob drags and toggle/matrix clicks
    -- never start a text selection.
    [ style $ "position:fixed;top:var(--tf-bar);left:0;right:0;bottom:0;display:flex;align-items:stretch;overflow-x:auto;overflow-y:hidden;"
        <> "user-select:none;-webkit-user-select:none;"
        <> "background:#b7b1a0;font-family:Georgia,serif" ]
    [ scopePanel s
    , quantizerPanel s
    , playheadsPanel s
    , gridPanel s
    , generatePanel s
    , scenesPanel s
    ]
