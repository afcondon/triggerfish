-- | Triggerfish.Balistes — a virtual Mutable Instruments Grids, dressed in the
-- | Hainbach/Braun panel. Three things on one surface:
-- |
-- |   • the STYLE PAD — the iconic X/Y cursor roaming the 5×5 node grid; drag it
-- |     and the kit morphs by bilinear interpolation between the four nearest
-- |     drum maps. The hardware's one knob-pair, made spatial.
-- |   • the PATTERN map — a live 3×32 heatmap of the interpolated landscape (the
-- |     plan's insight: the interpolation *is* a visualization), with the density
-- |     threshold rendered as which cells light up, and the playhead sweeping it.
-- |   • DENSITY + RANDOMNESS knobs and a transport.
-- |
-- | The brain is `Balistes.Model`/`Engine`, a faithful port of the MIT firmware
-- | (and of the BEAM `balistes_voice`), so this app is also the design surface
-- | for that module — the same role Odonus plays for `odonus_engine`. Output is
-- | one MIDI channel (BD/SD/HH = 36/38/42) to an IAC bus into Ableton, clocked
-- | by Binnacle (free-run → Link-lock), exactly like Odonus.
module Triggerfish.Balistes.Component (component, Output(..)) where

import Prelude

import Data.Array (deleteAt, filter, findIndex, length, mapWithIndex, modifyAt, null, range, (!!))
import Data.Foldable (any, foldl, for_)
import Data.Int (floor, round, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.String.Common (joinWith)
import Data.String (contains) as String
import Data.String.Pattern (Pattern(..)) as String
import Data.String.CodeUnits (take)
import Effect (Effect)
import Data.Either (Either(..))
import Effect.Aff (attempt)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Timer (setInterval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Binnacle as Binnacle
import Binnacle.Clock as Clock
import Binnacle.Midi as Midi
import Binnacle.Scheduler as Scheduler
import Binnacle.Transport as Transport
import Reef.Balistes.Protocol (encodeBalSim, encodeBTagged, encodeFixed, encodeTrigKit)
import Reef.Balistes.Input as RBI
import Reef.Balistes.Fixed as RF
import Reef.Balistes.Trig as Trig
import Triggerfish.Balistes.Model as M
import Triggerfish.Balistes.Pattern as P
import Triggerfish.Balistes.Types
  ( KnobTarget(..), targetRange, applyKnob, Active(..)
  , NoteRef(..), DragKind(..), State, Action(..), activePattern, selectedPattern, rigUrl, gridCfg
  , midiPortName, drumChannel, cycleSteps, editVel, flashWindow
  , padId, eqTrigName, jackNoteOf )
import Triggerfish.Balistes.TriSnapshot (TriSnapshot(..), printTri, parseTri)
import Triggerfish.Glyph as G
import Triggerfish.GlyphView (faIcon)
import Triggerfish.Preset (Preset, indexOfContent, presetAlias)
import Triggerfish.Balistes.Widgets (armBtn, instColor)
import Triggerfish.Balistes.View.Trig (routeStrip, trigJacks)
import Triggerfish.Balistes.View.Fixed (cellStrip, fixedSvg, patternChips)
import Triggerfish.Balistes.View.Grids (heatSvg, knobStack, padSvg)
import Triggerfish.Macro (Form(..), parseLane)
import Triggerfish.Balistes.Source as Source
import Triggerfish.Balistes.Store as Store
import Triggerfish.Balistes.Remote as Remote
import Triggerfish.Balistes.Lepidoptera (printPattern, parsePattern)
import Triggerfish.SourceQuery (Query(..))
import Triggerfish.Transport (Sounding(..))
import Triggerfish.Tidal.Lane as Lane
import Reef.Balistes.Sim as Sim
import Triggerfish.Ui.Pointer as Pointer
import Triggerfish.Odonus.Grid.Widgets (clampI, engrave, style)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent as ME

-- | The upward message to the shell: Balistes' identity-chip view (or `Nothing`
-- | when nothing is parked), for the six-machine status board. Raised from the
-- | Frame loop only when the view changes (see `chipViewOf`).
data Output
  = IdentityChanged (Maybe G.ChipView)
  -- The arrangement lane was edited here. The shell owns and persists it, so we
  -- report rather than store: it lands in `macroLanes` and is mirrored straight
  -- back down, which is also what keeps the rack-wide TIDAL page in step.
  | LaneEdited String

component :: forall i m. MonadAff m => H.Component Query i Output m
component =
  H.mkComponent
    { initialState: \_ ->
        { bal: M.defaultBalistes
        , sounding: Silent, playStep: 0, nextModelStep: 0, pending: [], flash: []
        , binnacle: Nothing, midiOut: Nothing, midiName: "…"
        , clockTempo: 120.0, clockLocked: false, clockBeat: 0.0, clockBar: 0, anchorCount: 0
        , nowMicros: 0.0, dragging: Nothing, dragSub: Nothing
        , presets: []
        , identity: Nothing, lastChip: Nothing
        , active: AGrids, library: P.bundledPatterns, editing: false, presetsOpen: false, fixedSel: 0, lane: "", laneReadout: "", selected: Nothing
        , scratchFixed: Nothing
        , trig: M.defaultTrig, publishMsg: Nothing }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction, handleQuery = handleQuery, initialize = Just Initialize }
    }

-- | Answer the shell: the source (TIDAL tab) — the reflective header (X/Y,
-- | densities, groove, ratchets, tapped pads) over the editable lane/routing
-- | doc — or adopt the rack's shared free-run baseline.
handleQuery :: forall m a. MonadAff m => Query a -> H.HalogenM State Action () Output m (Maybe a)
handleQuery = case _ of
  AskSource reply -> do
    s <- H.get
    pure (Just (reply (Source.headerText s.bal)))
  PutSource _ next -> pure (Just next)   -- shell never rewrites Balistes's kit
  -- The lane mirror. Guarded on inequality so the round-trip of our OWN edit is a
  -- no-op: without it, every push would rewrite the field the user is typing in
  -- and the caret would jump to the end on each tick.
  PutLane t r next -> do
    H.modify_ \s -> s { lane = if t == s.lane then s.lane else t, laneReadout = r }
    pure (Just next)
  -- No stage axis yet, so URL routing to this machine stops at the machine
  -- segment (`#balistes`). When it grows one, parse the segments here.
  SetStagePath _ next -> pure (Just next)
  AskClock reply -> do
    s <- H.get
    pure (Just (reply { tempo: s.clockTempo, locked: s.clockLocked }))
  SyncFree startMicros tempo next -> do
    s <- H.get
    for_ s.binnacle \bin ->
      liftEffect (Clock.setFreeBaseline (Binnacle.clock bin) { startMicros, tempo })
    pure (Just next)
  FeedChords _ next -> pure (Just next)   -- a drum machine; no chord quantiser
  FeedVoiceChords _ next -> pure (Just next)   -- ditto
  -- The ONE transport query (control-surface MISU refactor). The shell pushes this
  -- machine's derived `Sounding`; drum hits are one-shots so there's nothing to
  -- note-off — we only act on the rig edges: entering Rig hands off (re-issuing Rig
  -- re-hands-off), leaving Rig stops the voice. Local emission gates on `== Local`.
  SetSounding s next -> do
    st <- H.get
    when (st.sounding == Rig && s /= Rig) $
      for_ st.binnacle \bin -> liftEffect $ Transport.send (Binnacle.socket bin) "balistes-stop"
    H.modify_ _ { sounding = s }
    when (s == Rig) (handleAction PushBalistes)
    pure (Just next)
  AskSounding reply -> do
    s <- H.get
    pure (Just (reply s.sounding))
  -- A5 library manager: the fixed-rhythm library, each as its balistesPattern eDSL.
  -- (Grids is the live generative member, not a saved entry.)
  AskLibrary reply -> do
    s <- H.get
    pure (Just (reply (map (\p -> { name: p.name, text: printPattern p }) s.library)))
  LoadEntry i next -> do
    H.modify_ _ { active = AFixed i }
    pure (Just next)
  -- parsePattern is strict (only `balistesPattern` text), so it self-guards.
  ImportText txt reply -> case parsePattern txt of
    Just p -> do
      H.modify_ \s -> s { library = s.library <> [ p ], active = AFixed (length s.library) }
      persistLib
      pure (Just (reply true))
    Nothing -> pure (Just (reply false))
  -- No pitch quantiser — the rig's harmonic context doesn't apply to Balistes.
  SetContextPitchSet _ _ next -> pure (Just next)
  -- The shell's CAPTURE hotkey: bank the current playing-state and park identity
  -- on it (the chip shows the freshly-minted glyph, held). See captureNow.
  Capture next -> do
    captureNow
    pure (Just next)
  -- The status-board chip's recall menu: report each preset as its glyph alias (the
  -- shell reconstructs the coloured glyph via glyphFromAlias, faithful because
  -- colour follows the icon name), and recall a chosen preset.
  AskBank reply -> do
    s <- H.get
    pure (Just (reply (mapWithIndex (\i p -> { slot: i, alias: presetAlias p, name: fromMaybe "" p.name, starred: p.starred }) s.presets)))
  RecallSlot i next -> do
    recallPreset i
    pure (Just next)
  StarSlot i next -> do
    H.modify_ \s -> s { presets = fromMaybe s.presets (modifyAt i (\p -> p { starred = not p.starred }) s.presets) }
    persist
    pure (Just next)
  DeleteSlot i next -> do
    H.modify_ (deletePresetAt i)
    persist
    pure (Just next)

-- | Bank the current playing-state as a preset — the CAPTURE hotkey / button. DEDUPS
-- | by content (identical state ⇒ identical glyph): if it's already banked, just
-- | re-park `identity` on it; otherwise append an anonymous preset. Either way the
-- | chip shows the glyph held, and we persist. No-op only if the active brain has
-- | nothing to capture (an empty RYTM tab).
captureNow :: forall m. MonadAff m => H.HalogenM State Action () Output m Unit
captureNow = do
  s <- H.get
  case printTri <$> captureTri s of
    Nothing -> pure unit
    Just text -> do
      case indexOfContent text s.presets of
        Just _ -> H.modify_ _ { identity = Just text }
        Nothing -> H.modify_ \st -> st
          { presets = st.presets <> [ { content: text, name: Nothing, starred: false } ]
          , identity = Just text
          }
      persist

-- ---------------------------------------------------------------------------
-- handleAction
-- ---------------------------------------------------------------------------

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    -- Same rig handshake as Odonus: Binnacle free-runs at 120 until the Link
    -- anchor arrives, then phase-locks, so Balistes plays solo or in ensemble.
    bin <- liftEffect $ Binnacle.connect { url: rigUrl, tempo: 120.0 }
    { emitter: stepE, listener: stepL } <- liftEffect HS.create
    _ <- H.subscribe stepE
    _ <- liftEffect $ Scheduler.startGrid (Binnacle.clock bin) gridCfg \tick ->
      HS.notify stepL (Step tick)
    frameE <- frameTimer
    _ <- H.subscribe frameE
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
    -- restore the saved artefact: the rhythm library AND the ARRANGE rail (bank +
    -- Falls back to the bundled patterns / empty bank.
    msaved <- liftEffect Store.load
    for_ msaved \sv -> H.modify_ _
      { library = sv.library
      , presets = sv.presets
      }
    H.modify_ _ { binnacle = Just bin }
    -- Merge the shared Amphora library in the BACKGROUND. Forked deliberately: the
    -- fetch times out at ~30s when the store is unreachable, and awaiting it here
    -- kept Balistes' Initialize (hence the whole component) from completing — so the
    -- shell's queries (SetSounding, the CAPTURE hotkey) blocked until the timeout.
    -- Offline → keep the saved/bundled library; the merge lands if/when the DB answers.
    void $ H.fork do
      dbResult <- liftAff (attempt Remote.fetchLibrary)
      case dbResult of
        Right dbPats | not (null dbPats) ->
          H.modify_ \s -> s { library = mergeByName s.library dbPats }
        _ -> pure unit

  Step tick -> do
    st <- H.get
    -- Atlantis keeps the schedulers RUNNING and mutes only the emit — see
    -- Triggerfish.Transport: "the frontend is muted but keeps its schedulers
    -- running (lockstep animation)". Gating the whole handler on Local stopped
    -- the model dead in Atlantis, so the playhead froze and `bal` never
    -- advanced alongside the BEAM voice it is supposed to co-simulate.
    let audible = st.sounding == Local
    case st.active of
      -- A fixed rhythm: derive the step from the tick (no internal navigator),
      -- then emit each used lane's hit verbatim at its kit note + velocity. Reads
      -- `activePattern` so an ephemeral recalled snapshot (scratchFixed) plays too.
      AFixed _ -> case activePattern st of
        Nothing -> pure unit
        Just pat -> do
          let stepMs = 0.25 * 60000.0 / max 30.0 st.clockTempo
          when audible $ for_ st.midiOut \out -> liftEffect $
            -- the SHARED fixed-rhythm render (Reef.Balistes.Fixed.renderFixed) — the
            -- exact code the BEAM voice runs, keyed off the same absolute step, so a
            -- pushed fixed rhythm plays in lockstep. The frontend projects its rich
            -- pattern onto the wire-flat reef pattern (fixedOf).
            for_ (RF.renderFixed (fixedOf pat) tick.index) \e ->
              emitHit out drumChannel stepMs
                (max 0.0 (tick.delayMs + toNumber e.pushMs))
                e.note e.durMs e.velocity e.ratchet
          H.modify_ _ { playStep = tick.index `mod` pat.steps }
      AGrids -> do
        let
          -- Lockstep input-drain (deferred-on-both): apply any tick-tagged inputs
          -- whose step has arrived BEFORE ticking — the same order, and the same
          -- shared reef applyBInput, the BEAM voice uses, so a deferred gesture
          -- (Reset, …) lands on the SAME model step on both runtimes. `<=` self-heals
          -- inputs that were buffered while stopped. (The sequence advance that used
          -- to live here is now `advanceSeq`, run at the top of Step for all brains.)
          dueInputs = filter (\p -> p.step <= tick.index) st.pending
          keepInputs = filter (\p -> p.step > tick.index) st.pending
          bal0 = foldl (\b p -> RBI.applyBInput p.input b) st.bal dueInputs
          playedStep = bal0.step
          r = M.tick bal0
          stepMs = 0.25 * 60000.0 / max 30.0 st.clockTempo
        when audible $ for_ st.midiOut \out -> liftEffect $
          -- the three Grids voices (step-quantised, firmware-faithful), resolved by
          -- the SHARED render decision (Reef.Balistes.Sim.renderStep) — the exact
          -- code the BEAM balistes voice runs. A firing HH that clears the OPEN
          -- boundary rings as an open hat and chokes its closed self; ratchet roll
          -- + per-voice Dilla push come back on each event. The runtime only
          -- schedules the result — front and rig can't diverge on the decision.
          for_ (Sim.renderStep bal0 playedStep r.fired) \e ->
            emitHit out drumChannel stepMs
              (max 0.0 (tick.delayMs + toNumber e.pushMs))
              e.note e.durMs e.velocity e.ratchet
        let
          gridsFlash = map (\t -> { inst: t.inst, accent: t.accent, fireUnixMicros: tick.fireUnixMicros }) r.fired
        H.modify_ \s -> s
          { bal = r.bal, playStep = playedStep
          -- r.bal is the state that plays NEXT, at absolute step tick.index + 1;
          -- PushBalistes stamps the handoff with this for phase alignment.
          , nextModelStep = tick.index + 1
          , pending = keepInputs
          , flash = gridsFlash <> s.flash }
      -- POLYTRIG: resolve the rack to onset-fractions per jack (own source ∪ route
      -- atoms addressed to its name), then let the SHARED reef renderer slice out the
      -- onsets that fall in THIS step's window and their fractional sub-step time —
      -- the EXACT code reef_balistes_voice runs off the pushed kit, so browser
      -- (ch 10) and rig co-simulate byte-for-byte. One Tidal cycle == cycleSteps grid
      -- steps (one bar). Local emits; Rig follows the pushed kit.
      ASelene -> do
        let
          step = tick.index `mod` cycleSteps
          stepMs = 0.25 * 60000.0 / max 30.0 st.clockTempo
          fires = Trig.renderTrigStep (resolveTrigKit st.trig) tick.index cycleSteps
        when audible $ for_ st.midiOut \out -> liftEffect $
          for_ fires \f ->
            Midi.scheduleNote out
              { channel: drumChannel, note: f.note, velocity: Trig.trigVelocity
              , delayMs: tick.delayMs + f.frac * stepMs, durMs: Trig.trigGateMs }
        H.modify_ _ { playStep = step }

  Frame -> do
    st <- H.get
    case st.binnacle of
      Just bin -> do
        now <- liftEffect $ Clock.unixMicrosNow (Binnacle.clock bin)
        r <- liftEffect $ Clock.read (Binnacle.clock bin)
        H.modify_ \s -> s
          { nowMicros = now
          , clockTempo = r.tempo
          , clockLocked = r.locked
          , clockBeat = r.beat
          , clockBar = r.bar
          , anchorCount = r.anchorCount
          , flash = filter (\f -> (now - f.fireUnixMicros) < flashWindow) s.flash
          }
      Nothing -> pure unit
    -- Report the identity chip up to the shell's status board, but only when it
    -- actually changed (this fires ~30×/s) — capture/recall/divergence all land here.
    s2 <- H.get
    let cv = chipViewOf s2
    when (cv /= s2.lastChip) do
      H.modify_ _ { lastChip = cv }
      H.raise (IdentityChanged cv)

  MidiReady mout nm -> H.modify_ _ { midiOut = mout, midiName = nm }

  -- Reset shifts the model step (jump to 0), so it's DEFERRED-ON-BOTH: enqueued +
  -- broadcast tagged for a near-future step, applied by the drain here and by the
  -- voice on the rig at the SAME absolute step — no pattern offset. (When stopped it
  -- queues and applies on the next play, the drain's `<=` self-heal.)
  ResetPat -> enqueueBInput RBI.BReset
  -- Dice only reseeds the perturbation RNG (no step change), so it converges: the rig
  -- reseeds from the same RNG state at the tagged step. Rare edge = a 32-step resample
  -- landing in the ~2-step apply window (self-heals at the next boundary).
  Dice -> do
    H.modify_ \s -> s { bal = M.reseed s.bal }
    broadcastBInput RBI.BReseed

  -- The pad's own SVG mousemove fires whenever the cursor crosses it with a
  -- button held — including mid-knob-drag. Guard on `dragging`: a knob drag owns
  -- the pointer, so the pad ignores moves until that drag ends.
  PadAt cx cy btns -> do
    st <- H.get
    when (btns == 1 && isNothing st.dragging) do
      { x, y } <- liftEffect $ Pointer.padNorm padId cx cy
      H.modify_ \s ->
        let
          b1 = M.setY (round ((1.0 - y) * 255.0)) (M.setX (round (x * 255.0)) s.bal)
          moved = b1.x /= s.bal.x || b1.y /= s.bal.y
        in
          s { bal = if moved then M.clearRatchets b1 else b1 }

  -- Pad pointer-up: the X/Y were applied live during the drag; broadcast the settled
  -- values so the rig lands on the same cursor. (Two absolute setters, one tag.)
  PadRelease -> do
    st <- H.get
    broadcastBInput (RBI.BSetX st.bal.x)
    broadcastBInput (RBI.BSetY st.bal.y)

  StartDrag kind startVal -> do
    sid <- setupDrag
    H.modify_ _ { dragging = Just { kind, startY: 0, startVal }, dragSub = Just sid }
  DragMove clientY -> do
    st <- H.get
    case st.dragging of
      Just d
        | d.startY == 0 -> H.modify_ _ { dragging = Just d { startY = clientY } }
        | otherwise -> do
            let dist = d.startY - clientY
            case d.kind of
              DKnob target ->
                let r = targetRange target
                    delta = round (toNumber dist * toNumber (r.hi - r.lo) / 200.0)
                    newVal = clampI r.lo r.hi (d.startVal + delta)
                in H.modify_ \s -> s { bal = applyKnob target newVal s.bal }
              DCell inst step ->
                let newVal = clampI 1 8 (d.startVal + round (toNumber dist / 22.0))
                in H.modify_ \s -> s { bal = M.setRatchetAt inst step newVal s.bal }
              DNote ref ->
                let newVal = clampI 0 127 (d.startVal + round (toNumber dist / 7.0))
                in H.modify_ \s -> case ref of
                     NGrids lane -> s { bal = M.setNote lane newVal s.bal }
                     NFixed i lane -> s { library = fromMaybe s.library (modifyAt i (P.setNoteAt lane newVal) s.library) }
      Nothing -> pure unit
  DragEnd -> do
    st <- H.get
    for_ st.dragSub H.unsubscribe
    -- Live knob sync: broadcast the SETTLED gesture to the rig as a tick-tagged
    -- BInput. reef_balistes_voice applies it (via the shared reef applyBInput) on the
    -- tagged model step, so the rig follows the edit. Absolute idempotent setters, so
    -- replaying the settled value lands the rig exactly where the drag settled.
    when (st.sounding == Rig) $ for_ (st.dragging >>= \d -> dragToBInput d.kind st.bal) \input ->
      for_ st.binnacle \bin ->
        liftEffect $ Transport.send (Binnacle.socket bin) (balInputMsg st input)
    H.modify_ _ { dragging = Nothing, dragSub = Nothing }
    persistLib   -- a note drag (NFixed) may have edited the library

  BankNow -> captureNow
  -- Bank the named brain, whether or not it is the sounding one — the glyph you
  -- click lives in that brain's own band, so it must bank what THAT band shows.
  BankBrain target -> do
    s <- H.get
    case printTri <$> triOf s target of
      Nothing -> pure unit
      Just text -> do
        when (isNothing (indexOfContent text s.presets)) $
          H.modify_ \st -> st { presets = st.presets <> [ { content: text, name: Nothing, starred: false } ] }
        -- `identity` is the parked identity of the SOUNDING brain, so only move it
        -- when we banked that one; banking a ghosted band must not claim the chip.
        when (sameBrain s.active target) (H.modify_ _ { identity = Just text })
        persist

  SetLane t -> do
    H.modify_ _ { lane = t }
    H.raise (LaneEdited t)
  -- Click-to-assemble: append a pattern's name to the lane. Names with spaces are
  -- quoted, which is what the macro-tidal grammar wants ("quoted names" — Amphora
  -- labels routinely contain spaces, and `lo house 110` is three atoms unquoted).
  InsertLaneToken name -> do
    st <- H.get
    let tok = if String.contains (String.Pattern " ") name then "\"" <> name <> "\"" else name
        joined = (if st.lane == "" then "" else st.lane <> " ") <> tok
    H.modify_ _ { lane = joined }
    H.raise (LaneEdited joined)

  OpenPresets -> H.modify_ _ { presetsOpen = true }
  ClosePresets -> H.modify_ _ { presetsOpen = false }
  -- Recall closes the modal: you picked a thing, you want to see it land on the
  -- bands. Star and delete leave it open — those are curation, done in batches.
  RecallPreset i -> do
    recallPreset i
    H.modify_ _ { presetsOpen = false }
  StarPreset i ->
    H.modify_ \s -> s { presets = fromMaybe s.presets (modifyAt i (\p -> p { starred = not p.starred }) s.presets) }
  DeletePreset i -> H.modify_ (deletePresetAt i)

  DillaPreset -> H.modify_ \s -> s { bal = M.dillaPush s.bal }
  FlatGroove -> H.modify_ \s -> s { bal = M.flatPush s.bal }
  -- switching pattern just changes which branch the next Step takes; hits are
  -- one-shot, so nothing to silence.
  -- switching pattern changes which branch the next Step takes; once pushed, make the
  -- rig follow the selection too (a fixed pattern swaps in place; Grids re-hands-off).
  SelectPattern a -> do
    -- a deliberate tab / library selection clears any ephemeral recalled snapshot,
    -- returning the RYTM tab to its library index.
    -- Picking the RYTM brain also fixes the SELECTION, so `fixedSel` and the
    -- output agree whenever RYTM is live. Choosing another brain leaves
    -- `fixedSel` alone — the RYTM band keeps showing (and editing) its rhythm
    -- while ghosted.
    H.modify_ \s -> s
      { active = a, scratchFixed = Nothing, publishMsg = Nothing
      , fixedSel = case a of
          AFixed i -> i
          _ -> s.fixedSel }
    st <- H.get
    when (st.sounding == Rig) case a of
      AFixed _ -> repushFixed
      AGrids -> pushHandoff st
      ASelene -> pushTrig   -- push the resolved POLYTRIG kit to the rig voice
  ToggleEdit -> H.modify_ \s -> s { editing = not s.editing }
  -- click selects a cell for the NOTE inspector, creating a hit at the default
  -- velocity if the cell was empty; shift-click clears it.
  -- Edits are disabled on an ephemeral recalled snapshot (scratchFixed) — a
  -- frozen artefact plays read-only; the library is never mutated behind it.
  CellClick lane step shift -> do
    H.modify_ \s ->
      if isJust s.scratchFixed then s
      else if shift then s
        { library = modLibAt s.fixedSel (P.modifyCell lane step (const P.emptyCell)) s.library
        , selected = if s.selected == Just { lane, step } then Nothing else s.selected }
      else s
        { library = modLibAt s.fixedSel (\p -> if P.firesAt p lane step then p else P.modifyCell lane step (const (P.hitCell editVel)) p) s.library
        , selected = Just { lane, step } }
    persistLib
  SetCellVel d -> do
    H.modify_ (modSelectedCell \c -> c { vel = clampI 1 127 (c.vel + d) })
    persistLib
  SetCellProb d -> do
    H.modify_ (modSelectedCell \c -> c { prob = clampI 0 100 (c.prob + d) })
    persistLib
  SetCellRatchet d -> do
    H.modify_ (modSelectedCell \c -> c { ratchet = clampI 1 8 (c.ratchet + d) })
    persistLib
  CycleCellCond -> do
    H.modify_ (modSelectedCell \c -> c { cond = P.cycleCond c.cond })
    persistLib
  ClearSelected -> do
    H.modify_ \s -> case s.selected of
      Just { lane, step } | isNothing s.scratchFixed ->
        s { library = modLibAt s.fixedSel (P.modifyCell lane step (const P.emptyCell)) s.library, selected = Nothing }
      _ -> s
    persistLib
  -- a fresh empty rhythm, selected and opened in EDIT so all 16 lanes show.
  NewPattern -> do
    H.modify_ \s ->
      let n = length s.library
          p = P.emptyPattern ("pattern " <> show (n + 1)) 32
      in s { library = s.library <> [ p ], active = AFixed n, fixedSel = n, editing = true, selected = Nothing }
    persistLib
  SetPatternName name -> do
    H.modify_ \s ->
      if isJust s.scratchFixed then s
      else s { library = modLibAt s.fixedSel (_ { name = name }) s.library }
    persistLib
  -- Write-back to Amphora: publish the active fixed rhythm to the store (content
  -- + label + balistes-grid favourite), so a pattern built in the app persists
  -- and round-trips on next load. Content-addressed, so re-publishing an
  -- unchanged pattern is a no-op dedup.
  PublishActive -> do
    st <- H.get
    -- Publishes the SELECTED rhythm, not the sounding one: the RYTM band is
    -- editable while ghosted, so publishing must follow what you were editing.
    case selectedPattern st of
      Just pat -> do
        H.modify_ _ { publishMsg = Just "publishing…" }
        res <- liftAff (attempt (Remote.publishPattern pat))
        H.modify_ _ { publishMsg = Just case res of
          Right hash -> "✓ published · " <> take 8 hash
          Left _ -> "✗ publish failed (store offline?)" }
      Nothing -> pure unit
  PushBalistes -> do
    -- Lockstep HANDOFF: project the frontend Balistes state to a BalSim (the shared
    -- serializable subset), encode with the reef codec, and push it phase-aligned to
    -- the rig. reef_balistes_voice decodes with the SAME codec (decodeBalSim) and runs
    -- the SAME stepBal + renderStep, holding the pushed state until absolute step
    -- nextModelStep so the browser (ch 10) and the rig (ch 11) play it on the same
    -- step — no handoff flam. The Balistes grid is fixed 1/16 → stepBeats 0.25.
    st <- H.get
    case st.active of
      -- Fixed rhythm: push the whole pattern (stateless, no phase-hold needed).
      AFixed _ -> when (st.sounding == Rig) $ for_ (activePattern st) \pat ->
        for_ st.binnacle \bin ->
          liftEffect $ Transport.send (Binnacle.socket bin)
            ("balistes-fixed " <> encodeFixed (fixedOf pat))
      -- Grids: the phase-aligned BalSim handoff.
      AGrids -> pushHandoff st
      -- POLYTRIG: push the whole resolved kit (stateless, no phase-hold needed —
      -- both runtimes read the same Link step, the fixed-rhythm discipline).
      ASelene -> pushTrig
  -- POLYTRIG editor — state edits; re-push the resolved kit so live jack/route
  -- edits reach the rig voice in place (a no-op in Local/Silent). Browser-only
  -- persistence: the rack isn't saved to localStorage (unlike the fixed library).
  SetJackSource i src -> do
    H.modify_ \s -> s { trig = M.setJackSource i src s.trig }
    pushTrig
  SetJackName i nm -> do
    H.modify_ \s -> s { trig = M.setJackName i nm s.trig }
    pushTrig
  SetJackNote i d -> do
    H.modify_ \s -> s { trig = M.setJackNote i (jackNoteOf s.trig i + d) s.trig }
    pushTrig
  SetRoute i src -> do
    H.modify_ \s -> s { trig = M.setRoute i src s.trig }
    pushTrig
  AddRoute -> do
    H.modify_ \s -> s { trig = M.addRoute s.trig }
    pushTrig
  RemoveRoute i -> do
    H.modify_ \s -> s { trig = M.removeRoute i s.trig }
    pushTrig
  NoOp -> pure unit

-- | Project the frontend Balistes record onto the shared `BalSim` — the lockstep
-- | subset (engine state + render overlay). Snapshots/sequence stay frontend-only.
balSimOf :: M.Balistes -> Sim.BalSim
balSimOf b =
  { x: b.x, y: b.y, densBd: b.densBd, densSd: b.densSd, densHh: b.densHh
  , randomness: b.randomness, step: b.step, perts: b.perts, rng: b.rng
  , notes: b.notes, open: b.open, push: b.push, ratchet: b.ratchet }

-- | Steps to defer a synced gesture: tagged for soundingStep + this. It must clear
-- | the rig voice's 200ms scheduling lookahead (~1.6 steps @120bpm) BY A MARGIN, plus
-- | the frontend's own ~120ms lookahead and clock-read staleness — otherwise the rig
-- | has already committed the tagged step and applies the gesture a step LATE. That's
-- | inaudible for idempotent setters (a knob one step late looks the same), but a
-- | step-SHIFTING gesture (Reset) offsets the pattern permanently. 4 steps (~500ms
-- | @120bpm) clears both lookaheads with room; safe to ~180bpm.
inputBufferSteps :: Int
inputBufferSteps = 4

-- | The current sounding model step from the shared Link beat (Balistes grid is
-- | fixed 1/16 → 0.25 beats/step). Matches reef_balistes_voice's trunc(beat/0.25).
soundingStep :: State -> Int
soundingStep s = floor (s.clockBeat / 0.25)

-- | Format a tick-tagged BInput for the wire, tagged a few steps ahead so the rig
-- | applies it on the same model step the frontend is heading toward.
balInputMsg :: State -> RBI.BInput -> String
balInputMsg s input =
  "balistes-input " <> encodeBTagged { tick: soundingStep s + inputBufferSteps, input }

-- | Broadcast a settled gesture to the rig as a tick-tagged BInput (no-op if no rig
-- | is connected). The frontend has already applied it locally; the rig applies it
-- | (via the shared applyBInput) on the tagged step and converges.
broadcastBInput :: forall o m. MonadAff m => RBI.BInput -> H.HalogenM State Action () o m Unit
broadcastBInput input = do
  st <- H.get
  -- Rig-send only in ATLANTIS (onRig = not audible); SOLO is silent to the rig.
  when (st.sounding == Rig) $ for_ st.binnacle \bin ->
    liftEffect $ Transport.send (Binnacle.socket bin) (balInputMsg st input)

-- | Project the frontend's rich FixedPattern onto the wire-flat reef pattern: drop
-- | the name/kit metadata and flatten each cell's TrigCond to condX/condY (CAlways →
-- | 0). The frontend and the rig then share reef's renderFixed off this exact data.
fixedOf :: P.FixedPattern -> RF.FixedPattern
fixedOf p =
  { steps: p.steps
  , notes: p.notes
  , grid: map (map cellOf) p.grid
  }
  where
  cellOf c =
    let cond = case c.cond of
                 P.CAlways -> { x: 0, y: 0 }
                 P.CEvery x y -> { x, y }
    in { vel: c.vel, prob: c.prob, ratchet: c.ratchet, condX: cond.x, condY: cond.y }

-- | Push the phase-aligned handoff: the whole BalSim stamped with nextModelStep, so
-- | the rig holds it until that step. Used by the Push button and by snapshot recall
-- | (a whole-kit jump, which the handoff is the natural fit for).
pushHandoff :: forall o m. MonadAff m => State -> H.HalogenM State Action () o m Unit
pushHandoff st =
  -- ATLANTIS only. Called by SyncToRig (the entering-ATLANTIS handoff, where audible
  -- is already false) and by snapshot recall (a no-op in SOLO — no rig leak).
  when (st.sounding == Rig) $ for_ st.binnacle \bin ->
    liftEffect $ Transport.send (Binnacle.socket bin)
      ("balistes-sim-at " <> show st.nextModelStep <> " 0.25 " <> encodeBalSim (balSimOf st.bal))

-- | Capture the CURRENTLY ACTIVE brain's playing-state into a `TriSnapshot`, so one
-- | bank sequences Grids / Rytm / Tidal intermingled. Stores the whole artefact
-- | (not a reference), so a snapshot survives library edits and can be pushed to the
-- | rig verbatim. `Nothing` only if a RYTM tab has no pattern in view.
captureTri :: State -> Maybe TriSnapshot
captureTri s = triOf s s.active

-- | A named brain's snapshot, INDEPENDENT of which one is sounding. Every band
-- | shows its own live glyph, so all three must be hashable at once.
-- |
-- | Only possible since the `activePattern`/`selectedPattern` split: RYTM's state
-- | used to be readable solely when RYTM was the live brain (`AFixed i` carried
-- | both "sounding" and "selected"), so a ghosted RYTM band had nothing to hash.
triOf :: State -> Active -> Maybe TriSnapshot
triOf s = case _ of
  AGrids -> Just (TSGrids (M.captureSnapshot s.bal))
  AFixed _ -> TSFixed <$> selectedPattern s
  ASelene -> Just (TSTrig s.trig)

-- | Restore a `TriSnapshot`: switch the active tab to its brain, restore that
-- | brain's state, and — when rig-authoritative — push the matching handoff so the
-- | rig follows. The rig side re-modes in place on any of balistes-sim-at / -fixed /
-- | -trig, so a mid-sequence Grids→Tidal→Rytm march is just three pushes, no gap.
-- | `TSFixed` restores EPHEMERALLY (scratchFixed), never touching the library. Does
-- | NOT set `identity` — the caller (`recallPreset`) parks it on the preset's text.
recallSnap :: forall o m. MonadAff m => TriSnapshot -> H.HalogenM State Action () o m Unit
recallSnap = case _ of
  TSGrids gsnap -> do
    H.modify_ \s -> s { active = AGrids, scratchFixed = Nothing, bal = M.applySnapshot gsnap s.bal }
    H.get >>= pushHandoff
  TSFixed pat -> do
    -- Highlight the library chip that matches the snapshot BY NAME (the identity the
    -- user reasons about — "funk 100"), so the switcher agrees with what's playing.
    -- Playback still comes from `scratchFixed` (the frozen artefact). No match → 0.
    st <- H.get
    let idx = fromMaybe 0 (findIndex (\p -> p.name == pat.name) st.library)
    H.modify_ _ { active = AFixed idx, scratchFixed = Just pat }
    st2 <- H.get
    when (st2.sounding == Rig) $ for_ st2.binnacle \bin ->
      liftEffect $ Transport.send (Binnacle.socket bin) ("balistes-fixed " <> encodeFixed (fixedOf pat))
  TSTrig rack -> do
    H.modify_ _ { active = ASelene, scratchFixed = Nothing, trig = rack }
    pushTrig

-- | Recall preset `i`: parse its content to a `TriSnapshot`, restore it, and park
-- | the identity chip on the preset's text (glyph SOLID; ghosts on divergence).
recallPreset :: forall o m. MonadAff m => Int -> H.HalogenM State Action () o m Unit
recallPreset i = do
  st <- H.get
  case st.presets !! i of
    Nothing -> pure unit
    Just p -> case parseTri p.content of
      Nothing -> pure unit
      Just snap -> do
        recallSnap snap
        H.modify_ _ { identity = Just p.content }

-- | Resolve a POLYTRIG bank to the wire-flat `Trig.TrigKit` the rig runs: each jack
-- | becomes its MIDI note + the onset fractions it fires at over one cycle (its own
-- | source pattern ∪ the route atoms addressed to its name). The mini-notation parse
-- | happens HERE (reef has no Tidal parser); the shared `renderTrigStep` then slices
-- | these onsets into steps identically on both runtimes. Concatenation order (own
-- | then routed, no dedup) matches the frontend's own playback exactly.
resolveTrigKit :: M.TrigBank -> Trig.TrigKit
resolveTrigKit tb =
  map (\jack -> { note: jack.note, onsets: Lane.onsetsOf jack.source <> routeOns jack.name }) tb.jacks
  where
  routeOns nm = map _.at (filter (\e -> eqTrigName e.name nm) (tb.routes >>= Lane.namedOnsetsOf))

-- | Push the resolved POLYTRIG kit to the rig voice (`balistes-trig <json>`). Like the
-- | fixed-rhythm push, no phase-hold: a rack is a pure function of the absolute step,
-- | so the rig snaps to the current Link step and agrees. A no-op unless rig-authoritative.
pushTrig :: forall o m. MonadAff m => H.HalogenM State Action () o m Unit
pushTrig = do
  st <- H.get
  when (st.sounding == Rig) $ for_ st.binnacle \bin ->
    liftEffect $ Transport.send (Binnacle.socket bin)
      ("balistes-trig " <> encodeTrigKit (resolveTrigKit st.trig))

-- | Deferred-on-both: enqueue a gesture locally AND broadcast it, both tagged for the
-- | same near-future step. The Step-loop drain applies it here, the voice applies it
-- | on the rig — both on the SAME model step. Needed for gestures that shift the step
-- | (Reset), where broadcast-on-settle would offset the pattern.
enqueueBInput :: forall o m. MonadAff m => RBI.BInput -> H.HalogenM State Action () o m Unit
enqueueBInput input = do
  st <- H.get
  let tag = soundingStep st + inputBufferSteps
  -- Local always applies (SOLO plays it); rig-send only in ATLANTIS.
  H.modify_ \s -> s { pending = s.pending <> [ { step: tag, input } ] }
  when (st.sounding == Rig) $ for_ st.binnacle \bin ->
    liftEffect $ Transport.send (Binnacle.socket bin) (balInputMsg st input)

-- | Map a settled drag to the BInput that reproduces it on the rig. The four knob
-- | kinds, grid notes and ratchets sync; a fixed-rhythm note edit (NFixed) is
-- | frontend-only (not part of the shared Grids engine), so it doesn't.
dragToBInput :: DragKind -> M.Balistes -> Maybe RBI.BInput
dragToBInput kind b = case kind of
  DKnob (KDens i) -> Just (RBI.BSetDensity i (M.densityOf i b))
  DKnob KRand -> Just (RBI.BSetRandomness b.randomness)
  DKnob (KPush i) -> Just (RBI.BSetPush i (M.pushOf i b))
  DKnob KOpen -> Just (RBI.BSetOpen (M.openOf b))
  DCell inst step -> Just (RBI.BSetRatchet inst step (M.ratchetAt b inst step))
  DNote (NGrids lane) -> Just (RBI.BSetNote lane (M.noteOf lane b))
  DNote (NFixed _ _) -> Nothing

-- | Delete preset `i` from the bank.
deletePresetAt :: Int -> State -> State
deletePresetAt i s = s { presets = fromMaybe s.presets (deleteAt i s.presets) }

-- | Project component `State` onto the persisted artefact (library + preset bank).
savedOf :: State -> Store.Saved
savedOf s = { library: s.library, presets: s.presets }

-- | Save the whole artefact to localStorage (library + bank + sequence). Called
-- | after any bank / sequence edit; `persistLib` layers the rig re-push on top.
persist :: forall o m. MonadAff m => H.HalogenM State Action () o m Unit
persist = do
  s <- H.get
  liftEffect (Store.save (savedOf s))

-- | Save after a library edit, then re-push the active pattern to the rig.
persistLib :: forall o m. MonadAff m => H.HalogenM State Action () o m Unit
persistLib = persist *> repushFixed

-- | After a fixed-rhythm edit, re-push the active pattern to the rig so live cell /
-- | velocity / condition edits reach it. The voice swaps the pattern IN PLACE
-- | (set_pattern — no restart). Guarded on `pushed` so an edit never silently starts
-- | the rig; a no-op in Grids mode or with no rig connected.
repushFixed :: forall o m. MonadAff m => H.HalogenM State Action () o m Unit
repushFixed = do
  st <- H.get
  when (st.sounding == Rig) case st.active of
    AFixed _ -> for_ (activePattern st) \pat ->
      for_ st.binnacle \bin ->
        liftEffect $ Transport.send (Binnacle.socket bin) ("balistes-fixed " <> encodeFixed (fixedOf pat))
    AGrids -> pure unit
    ASelene -> pure unit

-- | Emit one already-resolved hit: schedule `note` at `delay0` for `durMs`, or —
-- | when the cell is ratcheted (n > 1) — explode it into n evenly-spaced
-- | retriggers at flat velocity. Push/ratchet/open are resolved by the caller.
emitHit
  :: Midi.MidiOut -> Int -> Number -> Number -> Int -> Number -> Int -> Int -> Effect Unit
emitHit out channel stepMs delay0 note durMs velocity n =
  if n <= 1 then
    Midi.scheduleNote out
      { channel, note, velocity, delayMs: delay0, durMs }
  else
    let sub = stepMs / toNumber n
    in for_ (range 0 (n - 1)) \k ->
         Midi.scheduleNote out
           { channel, note, velocity
           , delayMs: delay0 + toNumber k * sub, durMs: sub * 0.9 }

-- | Apply a function to library pattern `i` (no-op if out of range).
modLibAt :: Int -> (P.FixedPattern -> P.FixedPattern) -> Array P.FixedPattern -> Array P.FixedPattern
modLibAt i f lib = fromMaybe lib (modifyAt i f lib)

-- | Union two libraries by pattern name: keep everything in `current`, append
-- | any `incoming` whose name isn't already present. Used to fold the Amphora
-- | `balistes-grid` patterns in over the locally-saved library without
-- | clobbering the user's own edits.
mergeByName :: Array P.FixedPattern -> Array P.FixedPattern -> Array P.FixedPattern
mergeByName current incoming =
  current <> filter (\p -> not (any (\q -> q.name == p.name) current)) incoming

-- | Apply a function to the selected cell of the active fixed rhythm.
modSelectedCell :: (P.Cell -> P.Cell) -> State -> State
modSelectedCell f s = case s.selected of
  Just { lane, step } | isNothing s.scratchFixed ->
    s { library = modLibAt s.fixedSel (P.modifyCell lane step f) s.library }
  _ -> s

-- ---------------------------------------------------------------------------
-- Timers / drag plumbing (mirrors Odonus)
-- ---------------------------------------------------------------------------

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

-- | **The three brains, all on screen at once** (AC, 2026-08-06).
-- |
-- | Balistes runs three drum brains — GRIDS (the MI-Grids morph engine), RYTM
-- | (user rhythms) and TIDAL (the POLYTRIG jack rack) — and exactly ONE of them
-- | sounds: `Step` fires `case st.active of` to ch10, and `repushFixed` hands off
-- | the same way. `active` is an OUTPUT selector.
-- |
-- | It used to be presented as a tab bar, which is a navigation control. That
-- | hid the mutual exclusivity behind a metaphor that denies it — three tabs read
-- | as three separate tools you visit, not one output you choose. And it cost
-- | three full-screen layouts to show three things that each used a fraction of
-- | the width.
-- |
-- | So: one surface, three horizontal BANDS, the two that aren't sounding
-- | GHOSTED. The ghost is the honest rendering of `active` — you can see all
-- | three states at once and which one is live, and switching is a click on the
-- | band itself rather than a trip through a tab.
-- |
-- | Ghosted bands stay FULLY INTERACTIVE (no `pointer-events:none`): editing a
-- | pattern before you switch to it is exactly the move you want mid-set. The
-- | dimming says "not sounding", not "not available".
-- |
-- | What paid for the space: the 196px BALISTES transport column (its readouts
-- | are now inline in the nav, and TEMPO duplicated the shell's BPM), the three
-- | per-brain help-text columns (NOTE, ROUTES, and the transport's prose — all
-- | static), and the 14-chip rhythm library wall (now the preset modal).
render :: forall m. State -> H.ComponentHTML Action () m
render s =
  HH.div
    [ style $ "position:fixed;top:var(--tf-bar);left:0;right:0;bottom:0;display:flex;flex-direction:column;"
        <> "user-select:none;-webkit-user-select:none;background:#b7b1a0;font-family:Georgia,serif" ]
    [ navBar s
    -- Bands left, ASSEMBLE right. Un-stretching the bands to ~2/3 costs them
    -- nothing — the RYTM grid's aspect is ~7.4:1, so a narrower band makes it
    -- SHORTER, not cramped — and buys a full-height column for putting the
    -- patterns together.
    , HH.div
        [ style "flex:1 1 auto;min-height:0;display:flex;align-items:stretch" ]
        [ HH.div
            [ style "flex:1 1 auto;min-width:0;overflow-y:auto;display:flex;flex-direction:column;gap:10px;padding:10px 14px 14px" ]
            [ mutableBand s, gridsBand s, tidalBand s ]
        , assemblePanel s
        ]
    , presetModal s
    ]

-- | **ASSEMBLE** — Balistes' view onto its own macro-tidal arrangement lane.
-- |
-- | Deliberately NOT a new chaining mechanism. The rack already has an
-- | arrangement language (docs/DESIGN-macro-tidal.md): one lane per machine,
-- | `step := form (# verb arg)*`, with `~` rests, `<a b c>` per-cycle
-- | alternation, quoted names and transform stacks. It runs on the shell's macro
-- | clock and the doc names "Balistes beats" as a form type. A private Balistes
-- | chainer would have been a second, weaker sequencer that didn't compose with
-- | the other machines' lanes.
-- |
-- | What this adds is REACH: the lane was only editable on the rack-wide TIDAL
-- | page, so assembling beats meant leaving the beats. Here the pattern names are
-- | click-to-insert next to the grid that makes them.
assemblePanel :: forall m. State -> H.ComponentHTML Action () m
assemblePanel s =
  HH.div
    [ style $ "flex:0 0 340px;min-width:0;height:100%;box-sizing:border-box;overflow-y:auto;"
        <> "background:linear-gradient(#dcd8c9,#cfcabb);border-left:1px solid #b3ae9c;padding:14px 14px 16px" ]
    [ HH.div [ style $ engrave <> ";font-size:12px;letter-spacing:0.16em;color:#3f3c33;border-bottom:1px solid #00000018;padding-bottom:6px;margin-bottom:10px" ]
        [ HH.text "ASSEMBLE" ]
    , HH.textarea
        [ HP.value s.lane
        , HE.onValueInput SetLane
        , HP.placeholder "\"lo house 110\" <\"trap 140\" ~> # dilla"
        , style $ "width:100%;box-sizing:border-box;height:78px;resize:vertical;padding:7px 9px;"
            <> "border:1px solid #a8a392;border-radius:6px;background:#f3f1e8;"
            <> "font-family:'SF Mono',Menlo,monospace;font-size:11px;line-height:1.5;color:#1c1a12" ]
    , laneScore s
    , HH.div [ style $ engrave <> ";font-size:8px;opacity:0.6;margin:7px 0 12px;line-height:1.6" ]
        [ HH.text ("NOW: " <> (if s.laneReadout == "" then "—" else s.laneReadout)) ]
    , HH.div [ style $ engrave <> ";font-size:9px;color:#8a8676;border-bottom:1px solid #00000014;padding-bottom:4px;margin-bottom:8px" ]
        [ HH.text "SNAPSHOTS" ]
    , HH.div [ style "display:flex;gap:5px;flex-wrap:wrap;margin-bottom:12px" ]
        (if null s.presets
           then [ HH.span [ style $ engrave <> ";font-size:8px;opacity:0.5" ] [ HH.text "NOTHING BANKED YET" ] ]
           else map presetToken s.presets)
    , HH.div [ style $ engrave <> ";font-size:9px;color:#8a8676;border-bottom:1px solid #00000014;padding-bottom:4px;margin-bottom:8px" ]
        [ HH.text "STRUCTURE" ]
    , HH.div [ style "display:flex;gap:5px;flex-wrap:wrap" ]
        (map laneToken [ "~", "<", ">", "# dilla", "# flat" ])
    , HH.div [ style $ engrave <> ";font-size:8px;opacity:0.5;margin-top:12px;line-height:1.7" ]
        [ HH.text "STEPS DIVIDE THE MACRO-CYCLE. ~ RESTS. <a b> TAKES A DIFFERENT ONE EACH CYCLE. ATOMS ARE BANKED SNAPSHOTS — BANK A STATE (◆ TOP LEFT) TO MAKE IT SEQUENCEABLE. RUN IT FROM THE TIDAL PAGE." ]
    ]

-- | The lane rendered as a SCORE — each step's alias drawn as its glyph pair
-- | rather than its three-word name.
-- |
-- | This is the answer to "I want to write `<0 2> 1`" without paying for it in
-- | stability (AC, 2026-08-06). Slot numbers would be terse but renumber on every
-- | delete, silently changing what a saved arrangement plays; glyph aliases are
-- | content-derived, so identical state always yields the identical name. Keeping
-- | aliases as the SOURCE and drawing them as glyphs gets the glanceable score
-- | without the hazard — the text stays verbose, the reading does not.
laneScore :: forall m. State -> H.ComponentHTML Action () m
laneScore s =
  let steps = parseLane s.lane
  in if null steps then HH.text ""
     else HH.div
       [ style "display:flex;align-items:center;gap:9px;flex-wrap:wrap;margin-top:9px;padding:7px 9px;border-radius:6px;background:#00000008" ]
       (map stepGlyphs steps)
  where
  stepGlyphs step = case step.form of
    FRest -> HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:12px;color:#a09a88" ] [ HH.text "~" ]
    FName n -> aliasGlyphs n
    -- an alternation reads as its members inside angle brackets, so the shape of
    -- `<a b> c` survives into the score.
    FAlt inner ->
      HH.span [ style "display:inline-flex;align-items:center;gap:5px" ]
        ( [ bracket "⟨" ] <> map aliasGlyphs inner <> [ bracket "⟩" ] )
  bracket t = HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:12px;color:#a09a88" ] [ HH.text t ]
  -- An atom only resolves if it matches a BANKED snapshot's alias (`recallAlias`
  -- matches the bank, nothing else). Anything else — a rhythm's name, a typo, a
  -- snapshot since deleted — is drawn as its raw text with a `?`, because the
  -- alternative is what bit us: a token that silently renders as blank glyphs and
  -- is silently skipped at play time.
  aliasGlyphs a =
    if a == "~" then bracket "~"
    else if any (\p -> presetAlias p == a) s.presets then
      let g = G.glyphFromAlias a
      in HH.span [ HP.title a, style "display:inline-flex;align-items:center;gap:2px" ]
           [ faIcon g.first, faIcon g.second ]
    else
      HH.span
        [ HP.title (a <> " — not a banked snapshot, so this step will not resolve")
        , style "font-family:'SF Mono',Menlo,monospace;font-size:10px;color:#a8562f;white-space:nowrap" ]
        [ HH.text (a <> " ?") ]

-- One banked snapshot as a click-to-insert glyph pair. Inserting writes the ALIAS
-- (quoted) into the lane — the stable, content-derived name — while what you click
-- and read is the glyph.
presetToken :: forall m. Preset -> H.ComponentHTML Action () m
presetToken p =
  let alias = presetAlias p
      g = G.glyphFromAlias alias
  in HH.button
      [ HE.onClick \_ -> InsertLaneToken alias
      , HP.title (fromMaybe alias p.name <> " — append to the lane")
      , style $ "display:flex;align-items:center;gap:3px;padding:3px 8px;border:1px solid #a8a392;"
          <> "border-radius:5px;cursor:pointer;background:#f3f1e8" ]
      [ faIcon g.first, faIcon g.second ]

-- One click-to-insert token.
laneToken :: forall m. String -> H.ComponentHTML Action () m
laneToken name =
  HH.button
    [ HE.onClick \_ -> InsertLaneToken name
    , HP.title ("append to the lane: " <> name)
    , style $ "padding:3px 9px;border:1px solid #a8a392;border-radius:5px;cursor:pointer;"
        <> "font-family:'SF Mono',Menlo,monospace;font-size:10px;color:#3f3c33;background:#f3f1e8" ]
    [ HH.text name ]

-- The secondary nav, same shape as Vetula's and Odonus's: controls hard left
-- under the shell's transport, status right. No stage tabs — Balistes has one
-- surface, and what used to be its tabs is now the ghosting on the bands.
navBar :: forall m. State -> H.ComponentHTML Action () m
navBar s =
  HH.div
    [ style $ "flex:0 0 auto;display:flex;align-items:center;gap:10px;padding:5px 14px;"
        <> "background:linear-gradient(#cdc7b6,#c4bead);border-bottom:1px solid #00000014" ]
    ( [ navBtn "RESET" ResetPat
      , navBtn "DICE" Dice
      , navBtn "presets…" OpenPresets
      , HH.div [ style "flex:1 1 auto;min-width:8px" ] []
      , lampRow s
      , navDivider
      , navReadout (show (round s.clockTempo) <> " bpm" <> (if s.clockLocked then " ⛓" else ""))
      , navReadout ("bar " <> show s.clockBar <> " · " <> pad2 (s.playStep + 1) <> "/32")
      , navReadout ("ch " <> show (drumChannel + 1))
      , navReadout s.midiName
      ] )

-- | **One live glyph per BAND, three visible at all times** (AC, 2026-08-07).
-- |
-- | Each band shows the content glyph of ITS OWN brain — recomputed every render
-- | from `triOf`, so identical state always shows the identical pair. Position
-- | carries the machine: a badge letter would say the same thing in less legible
-- | ink, and you learn `🚚☂️ = Grids` by having banked it in the Grids band.
-- |
-- | Gold = this exact content is not banked; grey = it is. Click banks it, so the
-- | glyph is the name the thing WILL have, shown before you commit to it — the
-- | gesture is recognition rather than naming. Works on a ghosted band: building
-- | the next beat while the current one plays is the point of showing all three.
bandGlyph :: forall m. State -> Active -> H.ComponentHTML Action () m
bandGlyph s target = case printTri <$> triOf s target of
  Nothing -> HH.text ""
  Just text ->
    let g = G.glyphOf text
        banked = isJust (indexOfContent text s.presets)
    in HH.button
        [ HE.onClick \_ -> BankBrain target
        , HP.title (if banked then "this exact state is banked under this glyph"
                              else "unsaved — click to bank this state under its glyph")
        , style $ "display:flex;align-items:center;gap:5px;padding:2px 8px;border-radius:5px;cursor:pointer;"
            <> "font-family:Georgia,serif;font-size:10px;"
            <> (if banked then "border:1px solid #00000018;background:#00000006;color:#8a8676"
                          else "border:1px solid #c9a23a;background:#fbf3df;color:#7a5c00") ]
        [ HH.span [ style "display:inline-flex;align-items:center;gap:3px" ]
            [ faIcon g.first, faIcon g.second ]
        , HH.text (if banked then "banked" else "bank")
        ]

navBtn :: forall m. String -> Action -> H.ComponentHTML Action () m
navBtn label act =
  HH.button
    [ HE.onClick \_ -> act
    , style $ "flex:0 0 auto;padding:3px 12px;border:1px solid #00000022;border-radius:6px;cursor:pointer;"
        <> "font-family:Georgia,serif;font-size:11px;color:#3f3c33;background:#efece1" ]
    [ HH.text label ]

navDivider :: forall m. H.ComponentHTML Action () m
navDivider = HH.div [ style "width:1px;height:18px;background:#00000018" ] []

navReadout :: forall m. String -> H.ComponentHTML Action () m
navReadout txt =
  HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:9px;color:#5a5648;white-space:nowrap" ]
    [ HH.text txt ]

-- ---------------------------------------------------------------------------
-- The bands
-- ---------------------------------------------------------------------------

-- One brain's band: a header (name · live dot · that brain's own inline
-- controls) over its body, ghosted when it isn't the sounding brain. Clicking
-- the header makes it live.
band
  :: forall m
   . State
  -> Active
  -> String
  -> Array (H.ComponentHTML Action () m)
  -> H.ComponentHTML Action () m
  -> H.ComponentHTML Action () m
band s target label extras body =
  let live = sameBrain s.active target
  in HH.div
      [ style $ "flex:0 0 auto;border-radius:9px;border:1px solid " <> (if live then "#00000026" else "#00000012")
          <> ";background:" <> (if live then "linear-gradient(#dcd8c9,#d2cdbe)" else "#00000008")
          <> ";padding:8px 12px 10px;transition:opacity 120ms ease;"
          <> (if live then "" else "opacity:0.45") ]
      [ HH.div [ style "display:flex;align-items:center;gap:12px;margin-bottom:7px;flex-wrap:wrap" ]
          ( [ HH.button
                [ HE.onClick \_ -> SelectPattern target
                , HP.title (if live then "sounding on ch10" else "make this the sounding brain")
                , style $ "display:flex;align-items:center;gap:7px;border:none;background:none;cursor:pointer;padding:0;"
                    <> engrave <> ";font-size:12px;letter-spacing:0.14em;color:#3f3c33" ]
                [ HH.span
                    [ style $ "width:8px;height:8px;border-radius:50%;"
                        <> (if live then "background:#b8975a;box-shadow:0 0 6px #b8975a" else "background:#00000022") ]
                    []
                , HH.text label
                ]
            , bandGlyph s target
            ] <> extras )
      , body
      ]

-- `Active` carries the RYTM index, so a plain `==` would say a band isn't live
-- merely because a different rhythm is selected. Compare the BRAIN.
sameBrain :: Active -> Active -> Boolean
sameBrain a b = case a, b of
  AGrids, AGrids -> true
  AFixed _, AFixed _ -> true
  ASelene, ASelene -> true
  _, _ -> false

mutableBand :: forall m. State -> H.ComponentHTML Action () m
mutableBand s =
  band s AGrids "GRIDS" [ xyReadout ]
    ( HH.div [ style "display:flex;align-items:flex-start;gap:16px" ]
        [ HH.div [ style "flex:0 0 200px;height:200px" ] [ padSvg s ]
        , knobStack s
        , HH.div [ style "flex:1 1 auto;min-width:0" ] [ heatSvg s ]
        ] )
  where
  xyReadout =
    HH.span [ style $ engrave <> ";font-size:8px;opacity:0.6" ]
      [ HH.text ("X " <> show s.bal.x <> " · Y " <> show s.bal.y) ]

gridsBand :: forall m. State -> H.ComponentHTML Action () m
gridsBand s =
  band s (AFixed s.fixedSel) "RYTM" extras
    ( case selectedPattern s of
        Nothing -> HH.div [ style $ engrave <> ";font-size:9px;opacity:0.5" ] [ HH.text "NO RHYTHM SELECTED — OPEN PRESETS." ]
        Just pat -> HH.div [ style "width:100%" ] [ fixedSvg s s.fixedSel pat ] )
  where
  extras =
    [ HH.input
        [ HP.value (maybe "" _.name (selectedPattern s))
        , HE.onValueInput SetPatternName
        , style $ "padding:3px 8px;border:1px solid #a8a392;border-radius:5px;background:#f3f1e8;"
            <> "font-family:Georgia,serif;font-size:12px;color:#1c1a12;width:130px" ]
    , armBtn (if s.editing then "● EDITING" else "EDIT") s.editing ToggleEdit
    , armBtn "PUBLISH ⚱" false PublishActive
    , case s.publishMsg of
        Just msg -> HH.span [ style $ engrave <> ";font-size:8px;color:#2f6a4a" ] [ HH.text msg ]
        Nothing -> HH.text ""
    , cellStrip s
    ]

tidalBand :: forall m. State -> H.ComponentHTML Action () m
tidalBand s =
  band s ASelene "TIDAL" [ routeStrip s ] (trigJacks s)

-- ---------------------------------------------------------------------------
-- The preset modal
-- ---------------------------------------------------------------------------

-- | Two collections, deliberately kept apart (they are NOT the same thing and
-- | merging them would lose a distinction the rest of the rack relies on):
-- |
-- |   * **RHYTHMS** — `library :: Array FixedPattern`, the named user rhythms
-- |     that the RYTM brain plays. Amphora-merged, shared across the rig.
-- |   * **SNAPSHOTS** — `presets :: Array Preset`, whole-machine captures with
-- |     content-derived 2-glyph aliases (identical state ⇒ identical glyph).
-- |     These carry WHICH BRAIN was live, so recalling one can change the band
-- |     you're on. Previously reachable only from the shell's status-board chip
-- |     menu — i.e. not from inside Balistes at all.
presetModal :: forall m. State -> H.ComponentHTML Action () m
presetModal s =
  if not s.presetsOpen then HH.text ""
  else
    -- Backdrop and panel are SIBLINGS, not nested: a click-outside-to-close
    -- backdrop wrapped around the panel would need the panel to stop propagation,
    -- which needs an Effect-carrying no-op action. Siblings get the same
    -- behaviour with no plumbing — the panel simply isn't inside the catcher.
    HH.div [ style "position:fixed;inset:0;z-index:60" ]
      [ HH.div
          [ style "position:absolute;inset:0;background:#00000055"
          , HE.onClick \_ -> ClosePresets ]
          []
      , HH.div
          [ style $ "position:absolute;top:50%;left:50%;transform:translate(-50%,-50%);"
              <> "width:660px;max-width:92vw;max-height:82vh;overflow-y:auto;border-radius:10px;"
              <> "background:linear-gradient(#f6f2e8,#efe9db);border:1px solid #a8a392;"
              <> "box-shadow:0 14px 48px #00000044;padding:20px 24px" ]
          [ HH.div [ style "display:flex;align-items:baseline;justify-content:space-between;margin-bottom:14px" ]
              [ HH.span [ style $ engrave <> ";font-size:13px;letter-spacing:0.16em;color:#3f3c33" ] [ HH.text "PRESETS" ]
              , HH.button
                  [ HE.onClick \_ -> ClosePresets
                  , style "border:none;background:none;color:#8a8676;font-size:16px;cursor:pointer;line-height:1" ]
                  [ HH.text "✕" ]
              ]
          , modalSection "RHYTHMS" "named user rhythms — what the RYTM brain plays" (patternChips s)
          , modalSection "SNAPSHOTS" "whole-machine captures, glyphed by content — recalling one may change the live brain" (snapshotChips s)
          ]
      ]

modalSection :: forall m. String -> String -> H.ComponentHTML Action () m -> H.ComponentHTML Action () m
modalSection label blurb body =
  HH.div [ style "margin-bottom:18px" ]
    [ HH.div [ style $ engrave <> ";font-size:9px;color:#8a8676;border-bottom:1px solid #00000014;padding-bottom:4px;margin-bottom:9px" ]
        [ HH.text label ]
    , HH.div [ style $ engrave <> ";font-size:8px;opacity:0.6;margin-bottom:9px" ] [ HH.text blurb ]
    , body
    ]

-- The banked snapshots as their glyph pairs + names. Recall / star / delete are
-- the same three verbs the shell's chip menu offers, now available in-machine.
snapshotChips :: forall m. State -> H.ComponentHTML Action () m
snapshotChips s =
  if null s.presets
    then HH.div [ style $ engrave <> ";font-size:9px;opacity:0.5" ]
           [ HH.text "NOTHING BANKED YET — THE `c` HOTKEY CAPTURES THE LIVE STATE." ]
    else HH.div [ style "display:flex;flex-direction:column;gap:4px" ]
           (mapWithIndex snapshotRow s.presets)

snapshotRow :: forall m. Int -> Preset -> H.ComponentHTML Action () m
snapshotRow i p =
  let g = G.glyphFromAlias (presetAlias p)
  in HH.div
      [ style "display:flex;align-items:center;gap:9px;padding:4px 8px;border-radius:6px;background:#00000008" ]
      [ HH.span
          [ HE.onClick \_ -> StarPreset i
          , HP.title (if p.starred then "unstar" else "star (go-to)")
          , style $ "cursor:pointer;font-size:12px;color:" <> (if p.starred then "#c9a23a" else "#c2beb0") ]
          [ HH.text (if p.starred then "★" else "☆") ]
      , HH.span
          [ HE.onClick \_ -> RecallPreset i
          , style "display:flex;align-items:center;gap:8px;cursor:pointer;flex:1 1 auto" ]
          [ HH.span [ style "display:inline-flex;align-items:center;gap:3px" ] [ faIcon g.first, faIcon g.second ]
          , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:10px;color:#4a463b" ]
              [ HH.text (fromMaybe (presetAlias p) p.name) ]
          ]
      , HH.span
          [ HE.onClick \_ -> DeletePreset i
          , HP.title "delete"
          , style "cursor:pointer;color:#b0a898;font-size:11px" ]
          [ HH.text "✕" ]
      ]

-- Three pilot lamps that glow on a recent hit (bright = accented).
lampRow :: forall m. State -> H.ComponentHTML Action () m
lampRow s =
  HH.div [ style "display:flex;gap:8px;align-items:center" ]
    (map lamp [ 0, 1, 2 ])
  where
  lamp inst =
    let
      hits = filter (\f -> f.inst == inst && (s.nowMicros - f.fireUnixMicros) < flashWindow) s.flash
      on = not (null hits)
      accent = any _.accent hits
      col = instColor inst
      fill = if on then col else "#8c887a"
      glow = if on then ";box-shadow:0 0 8px " <> col <> (if accent then "" else "aa") else ""
    in
      HH.div [ style $ "width:11px;height:11px;border-radius:50%;border:1px solid #00000033;background:" <> fill <> glow ] []

pad2 :: Int -> String
pad2 n = if n < 10 then "0" <> show n else show n

-- The identity-chip view Balistes reports to the shell's six-machine status board:
-- the glyph of the parked identity + whether the live state has diverged from it
-- (per docs/DESIGN-scene-modal.md). `Nothing` when nothing is parked (empty). The
-- glyph is content-hashed from the parked snapshot's canonical text; divergence is
-- just "the live capture no longer equals the parked identity".
chipViewOf :: State -> Maybe G.ChipView
chipViewOf s = case s.identity of
  Nothing -> Nothing
  Just text -> Just { glyph: G.glyphOf text, diverged: (printTri <$> captureTri s) /= Just text }





