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
module Triggerfish.Balistes.Component (component) where

import Prelude

import Data.Array (filter, length, modifyAt, null, range, replicate, updateAt, (!!))
import Data.Foldable (any, foldl, for_)
import Data.Int (floor, round, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.String.Common (joinWith)
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
  , NoteRef(..), DragKind(..), State, Action(..), activePattern, rigUrl, gridCfg
  , stepsPerBar, midiPortName, drumChannel, cycleSteps, editVel, flashWindow
  , padId, eqTrigName, jackNoteOf )
import Triggerfish.Balistes.TriSnapshot (TriSnapshot(..))
import Triggerfish.Balistes.Widgets (flatBtn, instColor, panel, readout)
import Triggerfish.Balistes.View.Trig (trigBody, trigInfoPanel)
import Triggerfish.Balistes.View.Fixed (fixedBody, inspectorPanel, patternSwitcher)
import Triggerfish.Balistes.View.Grids (controlsPanel, gridsBody)
import Triggerfish.Balistes.Snapshot (snapshotRail)
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

component :: forall i o m. MonadAff m => H.Component Query i o m
component =
  H.mkComponent
    { initialState: \_ ->
        { bal: M.defaultBalistes
        , sounding: Silent, playStep: 0, nextModelStep: 0, pending: [], flash: []
        , binnacle: Nothing, midiOut: Nothing, midiName: "…"
        , clockTempo: 120.0, clockLocked: false, clockBeat: 0.0, clockBar: 0, anchorCount: 0
        , nowMicros: 0.0, dragging: Nothing, dragSub: Nothing
        , capArm: false, seqArm: false, seqEnabled: false, seqPos: 0, seqStartBar: 0
        , snapshots: replicate M.snapshotCount Nothing, sequence: [], seqBars: 1
        , active: AGrids, library: P.bundledPatterns, editing: false, selected: Nothing
        , scratchFixed: Nothing
        , trig: M.defaultTrig, publishMsg: Nothing }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction, handleQuery = handleQuery, initialize = Just Initialize }
    }

-- | Answer the shell: the source (TIDAL tab) — the reflective header (X/Y,
-- | densities, groove, ratchets, tapped pads) over the editable lane/routing
-- | doc — or adopt the rack's shared free-run baseline.
handleQuery :: forall o m a. MonadAff m => Query a -> H.HalogenM State Action () o m (Maybe a)
handleQuery = case _ of
  AskSource reply -> do
    s <- H.get
    pure (Just (reply (Source.headerText s.bal)))
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

-- ---------------------------------------------------------------------------
-- handleAction
-- ---------------------------------------------------------------------------

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
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
    -- restore the saved rhythm library (falls back to the bundled patterns).
    mlib <- liftEffect Store.loadLibrary
    for_ mlib \lib -> H.modify_ _ { library = lib }
    -- source the shared library from Amphora (the store of record): merge in any
    -- DB pattern not already present by name. Offline → keep saved/bundled.
    dbResult <- liftAff (attempt Remote.fetchLibrary)
    case dbResult of
      Right dbPats | not (null dbPats) ->
        H.modify_ \s -> s { library = mergeByName s.library dbPats }
      _ -> pure unit
    H.modify_ _ { binnacle = Just bin }

  Step tick -> do
    -- Mode-agnostic sequence advance FIRST: if a bar boundary elapsed, recall the
    -- next slot's TriSnapshot — which may switch the active brain — then the
    -- per-mode emit below runs on the (possibly just-switched) brain. This is what
    -- lets a Mutable→Tidal→Grids march play intermingled (#182/#199).
    advanceSeq tick
    st <- H.get
    when (st.sounding == Local) case st.active of
      -- A fixed rhythm: derive the step from the tick (no internal navigator),
      -- then emit each used lane's hit verbatim at its kit note + velocity. Reads
      -- `activePattern` so an ephemeral recalled snapshot (scratchFixed) plays too.
      AFixed _ -> case activePattern st of
        Nothing -> pure unit
        Just pat -> do
          let stepMs = 0.25 * 60000.0 / max 30.0 st.clockTempo
          for_ st.midiOut \out -> liftEffect $
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
        for_ st.midiOut \out -> liftEffect $
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
        for_ st.midiOut \out -> liftEffect $
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

  DillaPreset -> H.modify_ \s -> s { bal = M.dillaPush s.bal }
  FlatGroove -> H.modify_ \s -> s { bal = M.flatPush s.bal }
  -- the two arms are mutually exclusive.
  ToggleCap -> H.modify_ \s -> s { capArm = not s.capArm, seqArm = false }
  ToggleSeqBuild -> H.modify_ \s -> s { seqArm = not s.seqArm, capArm = false }
  -- shift → clear; capArm → CAPTURE the active brain's state into the slot (and
  -- disarm); seqArm → append to the path; otherwise RECALL (switch tab + restore +
  -- rig push). The bank now holds a `TriSnapshot` of whichever brain was active.
  SlotClick i shift -> do
    pre <- H.get
    if shift then
      H.modify_ \s -> s { snapshots = fromMaybe s.snapshots (updateAt i Nothing s.snapshots) }
    else if pre.capArm then
      H.modify_ \s -> s { snapshots = fromMaybe s.snapshots (updateAt i (captureTri s) s.snapshots), capArm = false }
    else if pre.seqArm then
      H.modify_ \s -> s { sequence = s.sequence <> [ i ] }
    else
      recallTri i
  -- enabling: seed seqPos at the end and force an immediate advance to step 0
  -- (the big-negative sentinel makes the first Step's bar gap exceed seqBars).
  ToggleSeq -> H.modify_ \s ->
    if s.seqEnabled then s { seqEnabled = false }
    else s { seqEnabled = true, seqPos = max 0 (length s.sequence - 1), seqStartBar = -100000 }
  SeqBarsDelta d -> H.modify_ \s -> s { seqBars = clampI 1 16 (s.seqBars + d) }
  ClearSeq -> H.modify_ \s -> s { sequence = [], seqEnabled = false, seqPos = 0 }
  -- switching pattern just changes which branch the next Step takes; hits are
  -- one-shot, so nothing to silence.
  -- switching pattern changes which branch the next Step takes; once pushed, make the
  -- rig follow the selection too (a fixed pattern swaps in place; Grids re-hands-off).
  SelectPattern a -> do
    -- a deliberate tab / library selection clears any ephemeral recalled snapshot,
    -- returning the GRIDS tab to its library index.
    H.modify_ _ { active = a, scratchFixed = Nothing, publishMsg = Nothing }
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
    H.modify_ \s -> case s.active of
      AFixed i | isNothing s.scratchFixed ->
        if shift then s
          { library = modLibAt i (P.modifyCell lane step (const P.emptyCell)) s.library
          , selected = if s.selected == Just { lane, step } then Nothing else s.selected }
        else s
          { library = modLibAt i (\p -> if P.firesAt p lane step then p else P.modifyCell lane step (const (P.hitCell editVel)) p) s.library
          , selected = Just { lane, step } }
      _ -> s
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
    H.modify_ \s -> case s.active, s.selected of
      AFixed i, Just { lane, step } | isNothing s.scratchFixed ->
        s { library = modLibAt i (P.modifyCell lane step (const P.emptyCell)) s.library, selected = Nothing }
      _, _ -> s
    persistLib
  -- a fresh empty rhythm, selected and opened in EDIT so all 16 lanes show.
  NewPattern -> do
    H.modify_ \s ->
      let n = length s.library
          p = P.emptyPattern ("pattern " <> show (n + 1)) 32
      in s { library = s.library <> [ p ], active = AFixed n, editing = true, selected = Nothing }
    persistLib
  SetPatternName name -> do
    H.modify_ \s -> case s.active of
      AFixed i | isNothing s.scratchFixed -> s { library = modLibAt i (_ { name = name }) s.library }
      _ -> s
    persistLib
  -- Write-back to Amphora: publish the active fixed rhythm to the store (content
  -- + label + balistes-grid favourite), so a pattern built in the app persists
  -- and round-trips on next load. Content-addressed, so re-publishing an
  -- unchanged pattern is a no-op dedup.
  PublishActive -> do
    st <- H.get
    case st.active of
      AFixed _ -> case activePattern st of
        Just pat -> do
          H.modify_ _ { publishMsg = Just "publishing…" }
          res <- liftAff (attempt (Remote.publishPattern pat))
          H.modify_ _ { publishMsg = Just case res of
            Right hash -> "✓ published · " <> take 8 hash
            Left _ -> "✗ publish failed (store offline?)" }
        Nothing -> pure unit
      _ -> H.modify_ _ { publishMsg = Just "select a GRIDS rhythm first" }
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
-- | bank sequences Mutable / Grids / Tidal intermingled. Stores the whole artefact
-- | (not a reference), so a snapshot survives library edits and can be pushed to the
-- | rig verbatim. `Nothing` only if a GRIDS tab has no pattern in view.
captureTri :: State -> Maybe TriSnapshot
captureTri s = case s.active of
  AGrids -> Just (TSGrids (M.captureSnapshot s.bal))
  AFixed _ -> TSFixed <$> activePattern s
  ASelene -> Just (TSTrig s.trig)

-- | Recall slot `i`: switch the active tab to the snapshot's brain, restore that
-- | brain's state, and — when rig-authoritative — push the matching handoff so the
-- | rig follows. The rig side re-modes in place on any of balistes-sim-at / -fixed /
-- | -trig, so a mid-sequence Mutable→Tidal→Grids march is just three pushes, no gap.
-- | `TSFixed` restores EPHEMERALLY (scratchFixed), never touching the library.
recallTri :: forall o m. MonadAff m => Int -> H.HalogenM State Action () o m Unit
recallTri i = do
  st <- H.get
  case join (st.snapshots !! i) of
    Nothing -> pure unit
    Just (TSGrids gsnap) -> do
      H.modify_ \s -> s { active = AGrids, scratchFixed = Nothing, bal = M.applySnapshot gsnap s.bal }
      H.get >>= pushHandoff
    Just (TSFixed pat) -> do
      H.modify_ _ { active = AFixed 0, scratchFixed = Just pat }
      st2 <- H.get
      when (st2.sounding == Rig) $ for_ st2.binnacle \bin ->
        liftEffect $ Transport.send (Binnacle.socket bin) ("balistes-fixed " <> encodeFixed (fixedOf pat))
    Just (TSTrig rack) -> do
      H.modify_ _ { active = ASelene, scratchFixed = Nothing, trig = rack }
      pushTrig

-- | Mode-agnostic sequence advance, run at the top of every Step: if a bar boundary
-- | elapsed while the sequence is playing, step the path and recall the next slot's
-- | TriSnapshot (which may switch the visible brain). Runs whenever the transport
-- | sounds (Local or Rig) so the rig follows the arrangement too.
advanceSeq :: forall o m. MonadAff m => Scheduler.Tick -> H.HalogenM State Action () o m Unit
advanceSeq tick = do
  st <- H.get
  let
    bar = tick.index / stepsPerBar
    seqLen = length st.sequence
    advancing = st.sounding /= Silent && st.seqEnabled && seqLen > 0 && (bar - st.seqStartBar) >= st.seqBars
  when advancing do
    let nextPos = (st.seqPos + 1) `mod` seqLen
    H.modify_ _ { seqPos = nextPos, seqStartBar = bar }
    case st.sequence !! nextPos of
      Just slot -> recallTri slot
      Nothing -> pure unit

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

-- | Save the current rhythm library to localStorage (after any edit).
persistLib :: forall o m. MonadAff m => H.HalogenM State Action () o m Unit
persistLib = do
  lib <- H.gets _.library
  liftEffect (Store.saveLibrary lib)
  repushFixed

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
modSelectedCell f s = case s.active, s.selected of
  AFixed i, Just { lane, step } | isNothing s.scratchFixed ->
    s { library = modLibAt i (P.modifyCell lane step f) s.library }
  _, _ -> s

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

render :: forall m. State -> H.ComponentHTML Action () m
render s =
  HH.div
    [ style $ "position:fixed;top:var(--tf-bar);left:0;right:0;bottom:0;display:flex;flex-direction:column;"
        <> "user-select:none;-webkit-user-select:none;background:#b7b1a0;font-family:Georgia,serif" ]
    -- One of three drum-brains at a time, chosen by the tab bar. Each tab is
    -- self-contained: its own CONTROL column (middle) + its own PATTERN surface
    -- (right). Three drum models — MUTABLE (the MI-Grids morph engine, AGrids),
    -- GRIDS (user rhythms, AFixed), TIDAL (the relocated POLYTRIG rack, ASelene).
    -- All → ch10.
    [ tabBar s
    , HH.div
        [ style "flex:1 1 auto;min-height:0;display:flex;align-items:stretch;overflow-x:auto;overflow-y:hidden" ]
        ( [ transportPanel s ]
            <> (case s.active of
                  AGrids -> [ controlsPanel s ]
                  AFixed _ -> [ inspectorPanel s ]
                  ASelene -> [ trigInfoPanel s ])
            <> [ patternPanel s ]
            -- the macro-tidal ARRANGE rail — the persistent snapshot bank +
            -- sequence, present in every tab so the three brains sequence together.
            <> [ snapshotRail s ] )
    ]

-- The drum-brain tab bar. The active tab is a projection of `active`'s constructor;
-- clicking a tab swaps the brain (GRIDS remembers the last-selected rhythm, or
-- falls to the first). All three brains output on ch 10.
tabBar :: forall m. State -> H.ComponentHTML Action () m
tabBar s =
  HH.div
    [ style $ "flex:0 0 auto;display:flex;gap:2px;padding:0 14px;background:#cfcabb;"
        <> "border-bottom:1px solid #b3ae9c;box-shadow:0 1px 3px #00000010" ]
    -- Display names (the constructors keep their build-time identifiers):
    -- MUTABLE = the MI-Grids morph engine (AGrids), GRIDS = user rhythms (AFixed),
    -- TIDAL = the POLYTRIG jack rack (ASelene).
    [ tabBtn "MUTABLE" (isGrids s.active) (Just (SelectPattern AGrids))
    , tabBtn "GRIDS" (isFixed s.active) (Just (SelectPattern (AFixed (fixedIx s.active))))
    , tabBtn "TIDAL" (isSelene s.active) (Just (SelectPattern ASelene))
    ]
  where
  isGrids = case _ of AGrids -> true
                      _ -> false
  isFixed = case _ of AFixed _ -> true
                      _ -> false
  isSelene = case _ of ASelene -> true
                       _ -> false
  fixedIx = case _ of AFixed i -> i
                      _ -> 0

tabBtn :: forall m. String -> Boolean -> Maybe Action -> H.ComponentHTML Action () m
tabBtn label active mact =
  HH.button
    ( [ style $ "padding:9px 20px;border:none;background:none;border-bottom:3px solid "
          <> (if active then "#b8975a" else "transparent") <> ";"
          <> "font-family:Georgia,serif;font-size:12px;letter-spacing:0.1em;"
          <> "cursor:" <> (maybe "default" (const "pointer") mact) <> ";"
          <> (if active then "color:#1c1a12"
              else maybe "color:#9a9484" (const "color:#3f3c33") mact) ]
        <> maybe [] (\act -> [ HE.onClick \_ -> act ]) mact )
    [ HH.text (label <> maybe "  ·soon" (const "") mact) ]

-- ---------------------------------------------------------------------------
-- Transport panel
-- ---------------------------------------------------------------------------

transportPanel :: forall m. State -> H.ComponentHTML Action () m
transportPanel s =
  let
    chLine = case s.active of
      AGrids -> show (drumChannel + 1) <> "  ·  "
        <> joinWith " / " (map (\l -> show (M.noteOf l s.bal)) [ 0, 1, 2 ])
      AFixed _ -> show (drumChannel + 1) <> "  ·  "
        <> case activePattern s of
             Just p -> show (length (P.usedLanes p)) <> " voices"
             Nothing -> "—"
      ASelene -> show (drumChannel + 1) <> "  ·  "
        <> show (length s.trig.jacks) <> " jacks"
    helpText = case s.active of
      AGrids -> "DRAG THE STYLE PAD TO MORPH THE KIT BETWEEN THE 25 NODES. DENSITY SETS HOW MANY HITS; RANDOMNESS NUDGES OFF-GRID EACH PATTERN."
      AFixed _ -> "A FIXED STARTER RHYTHM IS PLAYING. PICK ANOTHER FROM THE BANK, OR THE MUTABLE TAB FOR THE LIVE MORPH ENGINE."
      ASelene -> "POLYTRIG: EIGHT NAMED JACKS, EACH WITH ITS OWN MINI-NOTATION PATTERN, PLUS LANE-SPANNING ROUTES (\"bd sn cp sn\") THAT FIRE JACKS BY NAME. ALL → CH 10."
  in
  panel "BALISTES" "flex:0 0 196px"
    [ HH.div [ style "display:flex;flex-direction:column;gap:12px;margin-top:4px" ]
        -- ARM now lives on the tab dot in the top switcher; RESET / DICE stay.
        [ HH.div [ style "display:flex;gap:8px" ]
            [ flatBtn "RESET" ResetPat
            , flatBtn "DICE" Dice
            ]
        -- Control-surface Phase 2/refinement: no per-pane push OR hush — ATLANTIS
        -- hands off automatically; the global "Hush rig" lives in the top nav.
        , lampRow s
        , readout "TEMPO" (show (round s.clockTempo) <> " bpm" <> (if s.clockLocked then " ⛓" else " ·"))
        , readout "BAR" (show s.clockBar <> "  ·  step " <> pad2 (s.playStep + 1) <> "/32")
        , readout "MIDI" s.midiName
        , readout "CH" chLine
        , HH.div [ style $ engrave <> ";font-size:8px;opacity:0.6;margin-top:6px;line-height:1.5" ]
            [ HH.text helpText ]
        ]
    ]

-- Three pilot lamps that glow on a recent hit (bright = accented).
lampRow :: forall m. State -> H.ComponentHTML Action () m
lampRow s =
  HH.div [ style "display:flex;gap:10px;justify-content:center;margin:6px 0" ]
    (map lamp [ 0, 1, 2 ])
  where
  lamp inst =
    let
      hits = filter (\f -> f.inst == inst && (s.nowMicros - f.fireUnixMicros) < flashWindow) s.flash
      on = not (null hits)
      accent = any _.accent hits
      col = instColor inst
      fill = if on then col else "#8c887a"
      glow = if on then ";box-shadow:0 0 9px " <> col <> (if accent then "" else "aa") else ""
    in
      HH.div [ style "display:flex;flex-direction:column;align-items:center;gap:3px" ]
        [ HH.div [ style $ "width:18px;height:18px;border-radius:50%;border:1px solid #00000033;background:" <> fill <> glow ] []
        , HH.span [ style $ engrave <> ";font-size:8px" ] [ HH.text (M.instName inst) ]
        ]

pad2 :: Int -> String
pad2 n = if n < 10 then "0" <> show n else show n

patternPanel :: forall m. State -> H.ComponentHTML Action () m
patternPanel s =
  panel "PATTERN" "flex:1 1 480px;min-width:380px"
    ( (case s.active of
         AGrids -> []
         AFixed _ -> [ patternSwitcher s ]
         ASelene -> [])
        <> [ case s.active of
               AGrids -> gridsBody s
               AFixed i -> case activePattern s of
                 Just pat -> fixedBody s i pat
                 Nothing -> HH.text "—"
               ASelene -> trigBody s ] )



