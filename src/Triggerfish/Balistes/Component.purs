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

import Data.Array (concatMap, filter, length, mapWithIndex, modifyAt, null, range, (!!))
import Data.Foldable (any, foldl, for_, sum)
import Data.Int (floor, round, toNumber)
import Data.Int.Bits (shr)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.String.Common (joinWith)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Timer (setInterval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Unsafe.Coerce (unsafeCoerce)
import Binnacle as Binnacle
import Binnacle.Clock as Clock
import Binnacle.Midi as Midi
import Binnacle.Scheduler as Scheduler
import Binnacle.Transport as Transport
import Reef.Balistes.Protocol (encodeBalSim, encodeBTagged, encodeFixed)
import Reef.Balistes.Input as RBI
import Reef.Balistes.Fixed as RF
import Triggerfish.Balistes.Model as M
import Triggerfish.Balistes.Pattern as P
import Triggerfish.Balistes.Source as Source
import Triggerfish.Balistes.Store as Store
import Triggerfish.Balistes.Lepidoptera (printPattern, parsePattern)
import Triggerfish.SourceQuery (Query(..))
import Reef.Balistes.Tables as T
import Reef.Balistes.Sim as Sim
import Triggerfish.Ui.Knob (knob)
import Triggerfish.Ui.Pointer as Pointer
import Triggerfish.Odonus.Grid.Widgets (clampI, engrave, style, svgAttr, svgEl)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent as ME

-- ---------------------------------------------------------------------------
-- State / Actions
-- ---------------------------------------------------------------------------

-- | A recent hit, kept just long enough to flash the transport pilot lamps.
type Flash = { inst :: Int, accent :: Boolean, fireUnixMicros :: Number }

data KnobTarget = KDens Int | KRand | KPush Int | KOpen

-- | What the panel is currently playing. Grids is the special, generative,
-- | mutatable pattern (it owns the CONTROL column); `AFixed i` is a literal
-- | rhythm from the library (`library !! i`), played verbatim.
data Active = AGrids | AFixed Int

derive instance eqActive :: Eq Active

-- | Which MIDI note a note-drag edits: a Grids lane (0..3) or a fixed-pattern
-- | lane (`NFixed patternIx lane`).
data NoteRef = NGrids Int | NFixed Int Int

-- | A document-tracked drag turns a knob, subdivides a Grids cell into ratchets
-- | (`DCell lane step`), or nudges a lane's MIDI note (`DNote`). One plumbing.
data DragKind = DKnob KnobTarget | DCell Int Int | DNote NoteRef

type Drag = { kind :: DragKind, startY :: Int, startVal :: Int }

-- | The value range each knob spans (so the drag scales correctly per target).
targetRange :: KnobTarget -> { lo :: Int, hi :: Int }
targetRange = case _ of
  KDens _ -> { lo: 0, hi: 255 }
  KRand -> { lo: 0, hi: 255 }
  KPush _ -> { lo: -50, hi: 50 }
  KOpen -> { lo: 0, hi: 255 }

type State =
  { bal :: M.Balistes
  , running :: Boolean        -- the ARM/cue flag (sticky); sounds only when master too
  , master :: Boolean         -- the shell's master transport (pushed via SetMaster)
  , playStep :: Int
  -- the ABSOLUTE model step the current `bal` will next be played from (Grids
  -- mode). PushBalistes stamps the handoff with this so the rig holds the pushed
  -- state until the same step — the Odonus #57 phase-alignment, for Balistes.
  , nextModelStep :: Int
  -- tick-tagged gestures awaiting their model step (deferred-on-both lockstep):
  -- applied in the Step loop when step <= tick.index, on both runtimes.
  , pending :: Array { step :: Int, input :: RBI.BInput }
  -- has the user pushed to the rig this session? Gates the auto-re-push of fixed
  -- edits so editing/selecting a pattern doesn't silently START the rig voice.
  , pushed :: Boolean
  , flash :: Array Flash
  , binnacle :: Maybe Binnacle.Binnacle
  , midiOut :: Maybe Midi.MidiOut
  , midiName :: String
  , clockTempo :: Number
  , clockLocked :: Boolean
  , clockBeat :: Number
  , clockBar :: Int
  , anchorCount :: Int
  , nowMicros :: Number
  , dragging :: Maybe Drag
  , dragSub :: Maybe H.SubscriptionId
  -- snapshot-bank arming: capArm → a slot click STORES; seqArm → a slot click
  -- APPENDS to the sequence; neither → recall. Mutually exclusive.
  , capArm :: Boolean
  , seqArm :: Boolean
  -- sequence playback: enabled, the current step, and the absolute bar the step
  -- began on (a big-negative sentinel forces an immediate advance on enable).
  , seqEnabled :: Boolean
  , seqPos :: Int
  , seqStartBar :: Int
  -- the pattern family: which one is playing, and the fixed-rhythm library.
  , active :: Active
  , library :: Array P.FixedPattern
  -- EDIT mode for a fixed rhythm: reveal all 16 lanes (greyed where empty) so
  -- you can add voices; cells are click-to-toggle either way.
  , editing :: Boolean
  -- the cell the NOTE inspector is editing (lane, step) on the active rhythm.
  , selected :: Maybe { lane :: Int, step :: Int }
  }

data Action
  = Initialize
  | Step Scheduler.Tick
  | Frame
  | MidiReady (Maybe Midi.MidiOut) String
  | ToggleRun
  | ResetPat
  | Dice
  | PadAt Int Int Int          -- clientX clientY buttons
  | PadRelease                 -- pad pointer-up: broadcast the settled X/Y to the rig
  | StartDrag DragKind Int     -- kind, startVal
  | DragMove Int
  | DragEnd
  | DillaPreset
  | FlatGroove
  | ToggleCap                  -- arm/disarm capture-on-slot-click
  | ToggleSeqBuild             -- arm/disarm append-to-sequence-on-slot-click
  | SlotClick Int Boolean      -- slot i; shift = clear; else store/append/recall by arm
  | ToggleSeq                  -- play/stop the snapshot sequence
  | SeqBarsDelta Int           -- nudge bars-per-step
  | ClearSeq
  | SelectPattern Active       -- switch the playing pattern (Grids / a rhythm)
  | ToggleEdit                 -- reveal all 16 lanes on the active fixed rhythm
  | CellClick Int Int Boolean  -- select a cell (lane, step); shift = clear
  | SetCellVel Int             -- nudge the selected cell's velocity
  | SetCellProb Int            -- nudge its probability
  | SetCellRatchet Int         -- nudge its ratchet count
  | CycleCellCond              -- step its trig condition
  | ClearSelected              -- clear the selected cell + deselect
  | NewPattern                 -- append a fresh empty rhythm + select it
  | SetPatternName String      -- rename the active rhythm
  | PushBalistes               -- lockstep handoff: push BalSim to the rig (ch 11)
  | HushBalistes               -- silence the rig
  | NoOp

component :: forall i o m. MonadAff m => H.Component Query i o m
component =
  H.mkComponent
    { initialState: \_ ->
        { bal: M.defaultBalistes
        , running: false, master: false, playStep: 0, nextModelStep: 0, pending: [], pushed: false, flash: []
        , binnacle: Nothing, midiOut: Nothing, midiName: "…"
        , clockTempo: 120.0, clockLocked: false, clockBeat: 0.0, clockBar: 0, anchorCount: 0
        , nowMicros: 0.0, dragging: Nothing, dragSub: Nothing
        , capArm: false, seqArm: false, seqEnabled: false, seqPos: 0, seqStartBar: 0
        , active: AGrids, library: P.bundledPatterns, editing: false, selected: Nothing }
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
  SetMaster m next -> do
    H.modify_ _ { master = m }
    pure (Just next)
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
    H.modify_ _ { binnacle = Just bin }

  Step tick -> do
    st <- H.get
    when (st.master && st.running) case st.active of
      -- A fixed rhythm: derive the step from the tick (no internal navigator),
      -- then emit each used lane's hit verbatim at its kit note + velocity.
      AFixed i -> case st.library !! i of
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
          -- a bar is 16 sixteenth-steps. If the sequence is running and this step
          -- begins a step boundary (seqBars bars elapsed), advance the path and
          -- recall its snapshot BEFORE ticking, so the kit morphs at the boundary.
          bar = tick.index / stepsPerBar
          seqLen = length st.bal.sequence
          advancing = st.seqEnabled && seqLen > 0 && (bar - st.seqStartBar) >= st.bal.seqBars
          nextPos = if advancing then (st.seqPos + 1) `mod` seqLen else st.seqPos
          nextStartBar = if advancing then bar else st.seqStartBar
          bal0raw =
            if advancing then case M.seqStepAt st.bal nextPos of
              Just slot -> M.recallSnapshot slot st.bal
              Nothing -> st.bal
            else st.bal
          -- Lockstep input-drain (deferred-on-both): apply any tick-tagged inputs
          -- whose step has arrived BEFORE ticking — the same order, and the same
          -- shared reef applyBInput, the BEAM voice uses, so a deferred gesture
          -- (Reset, …) lands on the SAME model step on both runtimes. `<=` self-heals
          -- inputs that were buffered while stopped.
          dueInputs = filter (\p -> p.step <= tick.index) st.pending
          keepInputs = filter (\p -> p.step > tick.index) st.pending
          bal0 = foldl (\b p -> RBI.applyBInput p.input b) bal0raw dueInputs
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
          { bal = r.bal, playStep = playedStep, seqPos = nextPos, seqStartBar = nextStartBar
          -- r.bal is the state that plays NEXT, at absolute step tick.index + 1;
          -- PushBalistes stamps the handoff with this for phase alignment.
          , nextModelStep = tick.index + 1
          , pending = keepInputs
          , flash = gridsFlash <> s.flash }

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

  -- The RUN button is now a sticky ARM toggle; Balistes sounds only when armed
  -- AND the shell's master is playing. Drum hits are scheduled one-shots, so
  -- stopping just gates the next Step — nothing to silence.
  ToggleRun -> H.modify_ \s -> s { running = not s.running }
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
    for_ (st.dragging >>= \d -> dragToBInput d.kind st.bal) \input ->
      for_ st.binnacle \bin ->
        liftEffect $ Transport.send (Binnacle.socket bin) (balInputMsg st input)
    H.modify_ _ { dragging = Nothing, dragSub = Nothing }
    persistLib   -- a note drag (NFixed) may have edited the library

  DillaPreset -> H.modify_ \s -> s { bal = M.dillaPush s.bal }
  FlatGroove -> H.modify_ \s -> s { bal = M.flatPush s.bal }
  -- the two arms are mutually exclusive.
  ToggleCap -> H.modify_ \s -> s { capArm = not s.capArm, seqArm = false }
  ToggleSeqBuild -> H.modify_ \s -> s { seqArm = not s.seqArm, capArm = false }
  -- shift → clear; capArm → store (and disarm); seqArm → append to the path;
  -- otherwise recall whatever's there (instant jump).
  SlotClick i shift -> do
    pre <- H.get
    H.modify_ \s ->
      if shift then s { bal = M.clearSnapshot i s.bal }
      else if s.capArm then s { bal = M.storeSnapshot i s.bal, capArm = false }
      else if s.seqArm then s { bal = M.appendSeq i s.bal }
      else s { bal = M.recallSnapshot i s.bal }
    -- a plain RECALL is a whole-kit jump; re-push the phase-aligned handoff so the
    -- rig lands on the recalled state (the handoff is the natural fit for a big jump).
    when (not shift && not pre.capArm && not pre.seqArm) do
      st <- H.get
      pushHandoff st
  -- enabling: seed seqPos at the end and force an immediate advance to step 0
  -- (the big-negative sentinel makes the first Step's bar gap exceed seqBars).
  ToggleSeq -> H.modify_ \s ->
    if s.seqEnabled then s { seqEnabled = false }
    else s { seqEnabled = true, seqPos = max 0 (length s.bal.sequence - 1), seqStartBar = -100000 }
  SeqBarsDelta d -> H.modify_ \s -> s { bal = M.setSeqBars (s.bal.seqBars + d) s.bal }
  ClearSeq -> H.modify_ \s -> s { bal = M.clearSeq s.bal, seqEnabled = false, seqPos = 0 }
  -- switching pattern just changes which branch the next Step takes; hits are
  -- one-shot, so nothing to silence.
  -- switching pattern changes which branch the next Step takes; once pushed, make the
  -- rig follow the selection too (a fixed pattern swaps in place; Grids re-hands-off).
  SelectPattern a -> do
    H.modify_ _ { active = a }
    st <- H.get
    when st.pushed case a of
      AFixed _ -> repushFixed
      AGrids -> pushHandoff st
  ToggleEdit -> H.modify_ \s -> s { editing = not s.editing }
  -- click selects a cell for the NOTE inspector, creating a hit at the default
  -- velocity if the cell was empty; shift-click clears it.
  CellClick lane step shift -> do
    H.modify_ \s -> case s.active of
      AGrids -> s
      AFixed i ->
        if shift then s
          { library = modLibAt i (P.modifyCell lane step (const P.emptyCell)) s.library
          , selected = if s.selected == Just { lane, step } then Nothing else s.selected }
        else s
          { library = modLibAt i (\p -> if P.firesAt p lane step then p else P.modifyCell lane step (const (P.hitCell editVel)) p) s.library
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
    H.modify_ \s -> case s.active, s.selected of
      AFixed i, Just { lane, step } ->
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
      AFixed i -> s { library = modLibAt i (_ { name = name }) s.library }
      AGrids -> s
    persistLib
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
      AFixed i -> for_ (st.library !! i) \pat ->
        for_ st.binnacle \bin ->
          liftEffect $ Transport.send (Binnacle.socket bin)
            ("balistes-fixed " <> encodeFixed (fixedOf pat))
      -- Grids: the phase-aligned BalSim handoff.
      AGrids -> pushHandoff st
    H.modify_ _ { pushed = true }
  HushBalistes -> do
    st <- H.get
    for_ st.binnacle \bin ->
      liftEffect $ Transport.send (Binnacle.socket bin) "hush"
    H.modify_ _ { pushed = false }
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
  for_ st.binnacle \bin ->
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
  for_ st.binnacle \bin ->
    liftEffect $ Transport.send (Binnacle.socket bin)
      ("balistes-sim-at " <> show st.nextModelStep <> " 0.25 " <> encodeBalSim (balSimOf st.bal))

-- | Deferred-on-both: enqueue a gesture locally AND broadcast it, both tagged for the
-- | same near-future step. The Step-loop drain applies it here, the voice applies it
-- | on the rig — both on the SAME model step. Needed for gestures that shift the step
-- | (Reset), where broadcast-on-settle would offset the pattern.
enqueueBInput :: forall o m. MonadAff m => RBI.BInput -> H.HalogenM State Action () o m Unit
enqueueBInput input = do
  st <- H.get
  let tag = soundingStep st + inputBufferSteps
  H.modify_ \s -> s { pending = s.pending <> [ { step: tag, input } ] }
  for_ st.binnacle \bin ->
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
  when st.pushed case st.active of
    AFixed i -> for_ (st.library !! i) \pat ->
      for_ st.binnacle \bin ->
        liftEffect $ Transport.send (Binnacle.socket bin) ("balistes-fixed " <> encodeFixed (fixedOf pat))
    AGrids -> pure unit

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

-- | Apply a function to the selected cell of the active fixed rhythm.
modSelectedCell :: (P.Cell -> P.Cell) -> State -> State
modSelectedCell f s = case s.active, s.selected of
  AFixed i, Just { lane, step } -> s { library = modLibAt i (P.modifyCell lane step f) s.library }
  _, _ -> s

applyKnob :: KnobTarget -> Int -> M.Balistes -> M.Balistes
applyKnob (KDens i) v = M.setDensity i v
applyKnob KRand v = M.setRandomness v
applyKnob (KPush i) v = M.setPush i v
applyKnob KOpen v = M.setOpen v

knobValue :: KnobTarget -> M.Balistes -> Int
knobValue (KDens i) b = M.densityOf i b
knobValue KRand b = b.randomness
knobValue (KPush i) b = M.pushOf i b
knobValue KOpen b = M.openOf b

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

rigUrl :: String
rigUrl = "ws://127.0.0.1:3012/ws"

-- | One Grids step = a 16th note (32 steps = two bars). Same lookahead as Odonus.
gridCfg :: Scheduler.GridConfig
gridCfg = { stepBeats: 0.25, lookaheadMs: 120.0, tickMs: 25 }

-- | Sixteenth-note steps per 4/4 bar — the unit the snapshot sequence counts in.
stepsPerBar :: Int
stepsPerBar = 16

midiPortName :: String
midiPortName = "IAC"

-- | GM drum channel (MIDI ch 10) — the Grids device (BD/SD/HH).
drumChannel :: Int
drumChannel = 9

-- | Velocity a freshly-clicked fixed-rhythm cell lands at (a firm hit).
editVel :: Int
editVel = 98



-- | The open hat's teal — distinct from HH steel-blue, so opening cells read as
-- | a different voice in the heatmap.
ohColor :: String
ohColor = "#2f8a8a"

padId :: String
padId = "balistes-pad"

-- | Keep a hit around ~0.4s — long enough for the pilot lamps to glow.
flashWindow :: Number
flashWindow = 400000.0

instColor :: Int -> String
instColor = case _ of
  0 -> "#b04a2f"   -- BD, amber-red
  1 -> "#5f7d3f"   -- SD, green
  _ -> "#3f6f8a"   -- HH, steel-blue

-- | Per-lane colour for a fixed rhythm's 16-lane kit, grouped by voice family
-- | (kick/snare warm, hats cool, toms brown, cymbals gold, perc violet).
laneColor :: Int -> String
laneColor = case _ of
  0 -> "#b04a2f"   -- BD
  1 -> "#5f7d3f"   -- SD
  2 -> "#a86a2f"   -- CP
  3 -> "#8a6a4a"   -- RS
  4 -> "#3f6f8a"   -- CH
  5 -> "#4f7f9a"   -- PH
  6 -> "#2f8a8a"   -- OH
  7 -> "#7a5a3a"   -- LT
  8 -> "#8a6a44"   -- MT
  9 -> "#9a7a4a"   -- HT
  10 -> "#9a7d3a"  -- RD
  11 -> "#aa8d4a"  -- RB
  12 -> "#b58a3a"  -- CR
  13 -> "#6a5f8a"  -- CW
  14 -> "#7a6f9a"  -- TB
  _ -> "#8a7faa"   -- SH

-- | Gate length per fixed-rhythm lane: hats/cymbals ring, drums blip.

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
    [ style $ "position:fixed;top:var(--tf-bar);left:0;right:0;bottom:0;display:flex;align-items:stretch;overflow-x:auto;overflow-y:hidden;"
        <> "user-select:none;-webkit-user-select:none;background:#b7b1a0;font-family:Georgia,serif" ]
    -- The middle column is Grids' CONTROL chrome (X/Y morph + knobs); for a
    -- fixed rhythm it becomes the NOTE inspector for the selected cell.
    ( [ transportPanel s ]
        <> (case s.active of
              AGrids -> [ controlsPanel s ]
              AFixed _ -> [ inspectorPanel s ])
        <> [ patternPanel s ] )

-- A pale Hainbach panel (header + body). Scrolls vertically if its content is
-- taller than the viewport (the consolidated CONTROL panel can be).
panel :: forall m. String -> String -> Array (H.ComponentHTML Action () m) -> H.ComponentHTML Action () m
panel label widthCss body =
  HH.div
    [ style $ widthCss <> ";height:calc(100vh - var(--tf-bar));box-sizing:border-box;overflow-y:auto;overflow-x:hidden;"
        <> "background:linear-gradient(#dcd8c9,#cfcabb);border-left:1px solid #b3ae9c;"
        <> "padding:18px 16px;display:flex;flex-direction:column" ]
    ( [ HH.div
          [ style $ engrave <> ";font-size:14px;letter-spacing:0.16em;color:#3f3c33;"
              <> "margin-bottom:14px;border-bottom:1px solid #00000018;padding-bottom:6px" ]
          [ HH.text label ]
      ] <> body )

-- ---------------------------------------------------------------------------
-- Transport panel
-- ---------------------------------------------------------------------------

transportPanel :: forall m. State -> H.ComponentHTML Action () m
transportPanel s =
  let
    chLine = case s.active of
      AGrids -> show (drumChannel + 1) <> "  ·  "
        <> joinWith " / " (map (\l -> show (M.noteOf l s.bal)) [ 0, 1, 2 ])
      AFixed i -> show (drumChannel + 1) <> "  ·  "
        <> case s.library !! i of
             Just p -> show (length (P.usedLanes p)) <> " voices"
             Nothing -> "—"
    helpText = case s.active of
      AGrids -> "DRAG THE STYLE PAD TO MORPH THE KIT BETWEEN THE 25 NODES. DENSITY SETS HOW MANY HITS; RANDOMNESS NUDGES OFF-GRID EACH PATTERN."
      AFixed _ -> "A FIXED STARTER RHYTHM IS PLAYING. SWITCH TO ◆ GRIDS FOR THE LIVE MORPH ENGINE AND ITS CONTROLS."
  in
  panel "BALISTES" "flex:0 0 196px"
    [ HH.div [ style "display:flex;flex-direction:column;gap:12px;margin-top:4px" ]
        [ HH.button
            [ HE.onClick \_ -> ToggleRun
            , style $ "padding:12px 0;border:1px solid #a8a392;border-radius:7px;cursor:pointer;"
                <> "font-family:Georgia,serif;font-size:15px;letter-spacing:0.12em;color:#1c1a12;"
                <> "background:" <> (if s.running then "linear-gradient(#c8a86a,#b8975a)" else "linear-gradient(#efece1,#ddd9cb)") ]
            [ HH.text (if s.running then (if s.master then "❚❚ PLAYING" else "◆ CUED") else "▶ ARM") ]
        , HH.div [ style "display:flex;gap:8px" ]
            [ flatBtn "RESET" ResetPat
            , flatBtn "DICE" Dice
            ]
        , HH.div [ style "display:flex;gap:8px;padding-top:10px;border-top:1px solid #00000014" ]
            [ HH.button
                [ HE.onClick \_ -> PushBalistes
                , style $ "flex:1;padding:8px 0;border:1px solid #a8a392;border-radius:7px;cursor:pointer;"
                    <> "background:linear-gradient(#dfe7d6,#cdd9c0);font-family:Georgia,serif;font-size:12px;color:#3f4a33" ]
                [ HH.text "⇪ Push to rig (ch11)" ]
            , HH.button
                [ HE.onClick \_ -> HushBalistes
                , style $ "flex:0 0 auto;padding:8px 12px;border:1px solid #a8a392;border-radius:7px;cursor:pointer;"
                    <> "background:linear-gradient(#e7dcd6,#d9c8c0);font-family:Georgia,serif;font-size:12px;color:#4a3833" ]
                [ HH.text "✋ Hush" ]
            ]
        , lampRow s
        , readout "TEMPO" (show (round s.clockTempo) <> " bpm" <> (if s.clockLocked then " ⛓" else " ·"))
        , readout "BAR" (show s.clockBar <> "  ·  step " <> pad2 (s.playStep + 1) <> "/32")
        , readout "MIDI" s.midiName
        , readout "CH" chLine
        , HH.div [ style $ engrave <> ";font-size:8px;opacity:0.6;margin-top:6px;line-height:1.5" ]
            [ HH.text helpText ]
        ]
    ]

flatBtn :: forall m. String -> Action -> H.ComponentHTML Action () m
flatBtn label act =
  HH.button
    [ HE.onClick \_ -> act
    , style $ "flex:1;padding:8px 0;border:1px solid #a8a392;border-radius:6px;cursor:pointer;"
        <> "font-family:Georgia,serif;font-size:11px;letter-spacing:0.08em;color:#3f3c33;"
        <> "background:linear-gradient(#efece1,#ddd9cb)" ]
    [ HH.text label ]

readout :: forall m. String -> String -> H.ComponentHTML Action () m
readout label val =
  HH.div [ style "display:flex;justify-content:space-between;align-items:baseline;border-bottom:1px dotted #0000001a;padding-bottom:3px" ]
    [ HH.span [ style $ engrave <> ";font-size:9px" ] [ HH.text label ]
    , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:10px;color:#3f3c33;text-align:right" ] [ HH.text val ]
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

-- ---------------------------------------------------------------------------
-- Control panel — the STYLE pad + density/groove knobs, consolidated so the
-- SOURCE pane has room. Pad on top; two knob columns (DENSITY · PUSH) below.
-- ---------------------------------------------------------------------------

controlsPanel :: forall m. State -> H.ComponentHTML Action () m
controlsPanel s =
  panel "CONTROL" "flex:0 0 300px"
    [ HH.div [ style "display:flex;justify-content:center" ]
        [ HH.div [ style "width:252px;height:252px" ] [ padSvg s ] ]
    , HH.div [ style "display:flex;justify-content:space-between;margin:2px 6px 8px" ]
        [ HH.span [ style $ engrave <> ";font-size:9px" ] [ HH.text ("X " <> show s.bal.x) ]
        , HH.span [ style $ engrave <> ";font-size:9px" ] [ HH.text ("Y " <> show s.bal.y) ]
        ]
    , HH.div [ style "height:1px;background:#00000018;margin-bottom:10px" ] []
    -- Knobs as compact rows (BD·SD·HH across), so the freed vertical space goes
    -- to the snapshot sequencer below.
    , knobRow "DENSITY"
        [ bigKnob (KDens 0) (instColor 0) "BD" s.bal
        , bigKnob (KDens 1) (instColor 1) "SD" s.bal
        , bigKnob (KDens 2) (instColor 2) "HH" s.bal
        ]
    , knobRow "PUSH ms"
        [ bigKnob (KPush 0) (instColor 0) "BD" s.bal
        , bigKnob (KPush 1) (instColor 1) "SD" s.bal
        , bigKnob (KPush 2) (instColor 2) "HH" s.bal
        ]
    , knobRow "GROOVE"
        [ bigKnob KRand "#6a6657" "RAND" s.bal
        , bigKnob KOpen ohColor "OPEN" s.bal
        , HH.div [ style "display:flex;flex-direction:column;gap:5px;width:60px;align-self:center" ]
            [ flatBtn "DILLA" DillaPreset, flatBtn "FLAT" FlatGroove ]
        ]
    , HH.div [ style $ engrave <> ";font-size:8px;opacity:0.5;line-height:1.5;margin-top:6px" ]
        [ HH.text "OPEN turns the loudest HH hits into open hats (teal) — choke + ring. Drag any heatmap cell up/down to ratchet it." ]
    ]

-- The NOTE inspector — the per-cell editor that fills the column CONTROL
-- vacates for a fixed rhythm. Edits the selected cell's velocity / probability /
-- trig-condition / ratchet (the overlay the grid's tweak-dot flags).
inspectorPanel :: forall m. State -> H.ComponentHTML Action () m
inspectorPanel s =
  panel "NOTE" "flex:0 0 240px"
    [ case s.active, s.selected of
        AFixed i, Just sel -> case s.library !! i of
          Just pat -> cellInspector pat sel
          Nothing -> inspectorHint
        _, _ -> inspectorHint
    ]

inspectorHint :: forall m. H.ComponentHTML Action () m
inspectorHint =
  HH.div [ style $ engrave <> ";font-size:9px;opacity:0.55;line-height:1.8;margin-top:8px" ]
    [ HH.text "CLICK A CELL IN THE GRID TO INSPECT IT — VELOCITY · PROBABILITY · CONDITION · RATCHET. SHIFT-CLICK CLEARS A CELL." ]

cellInspector :: forall m. P.FixedPattern -> { lane :: Int, step :: Int } -> H.ComponentHTML Action () m
cellInspector pat sel =
  let c = P.cellAt pat sel.lane sel.step
  in HH.div [ style "display:flex;flex-direction:column;gap:13px;margin-top:6px" ]
       [ HH.div [ style "display:flex;align-items:baseline;justify-content:space-between" ]
           [ HH.span [ style $ "font-family:Georgia,serif;font-size:16px;font-weight:bold;color:" <> laneColor sel.lane ]
               [ HH.text (P.laneName sel.lane) ]
           , HH.span [ style $ engrave <> ";font-size:9px;opacity:0.6" ]
               [ HH.text ("STEP " <> show (sel.step + 1) <> " · ♪" <> show (P.noteOf pat sel.lane)) ]
           ]
       , paramRow "VELOCITY" (show c.vel) (SetCellVel (-8)) (SetCellVel 8)
       , paramRow "PROBABILITY" (show c.prob <> "%") (SetCellProb (-10)) (SetCellProb 10)
       , paramRow "RATCHET" ("×" <> show c.ratchet) (SetCellRatchet (-1)) (SetCellRatchet 1)
       , HH.div [ style "display:flex;align-items:center;justify-content:space-between;border-bottom:1px dotted #0000001a;padding-bottom:9px" ]
           [ HH.span [ style $ engrave <> ";font-size:9px" ] [ HH.text "CONDITION" ]
           , HH.button
               [ HE.onClick \_ -> CycleCellCond
               , style $ "padding:5px 14px;border:1px solid #a8a392;border-radius:5px;cursor:pointer;"
                   <> "font-family:'SF Mono',Menlo,monospace;font-size:12px;color:#3f3c33;background:linear-gradient(#efece1,#ddd9cb)" ]
               [ HH.text (condDisplay c.cond) ]
           ]
       , flatBtn "× CLEAR CELL" ClearSelected
       ]

-- The condition button's label ("ALWAYS" reads better than the "—" glyph here).
condDisplay :: P.TrigCond -> String
condDisplay P.CAlways = "ALWAYS"
condDisplay c = P.condLabel c

-- One inspector parameter row: label, − stepper, value, + stepper.
paramRow :: forall m. String -> String -> Action -> Action -> H.ComponentHTML Action () m
paramRow label val dec inc =
  HH.div [ style "display:flex;align-items:center;justify-content:space-between;border-bottom:1px dotted #0000001a;padding-bottom:9px" ]
    [ HH.span [ style $ engrave <> ";font-size:9px" ] [ HH.text label ]
    , HH.div [ style "display:flex;align-items:center;gap:9px" ]
        [ stepBtn "−" dec
        , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:13px;color:#3f3c33;width:46px;text-align:center" ] [ HH.text val ]
        , stepBtn "+" inc
        ]
    ]

-- The snapshot bank: capture the whole control point (X/Y + densities +
-- randomness + open + push) into a slot, recall it instantly. Records the
-- two-handed gestures a single mouse can't (kick up while snare down). Each
-- filled slot shows a mini X/Y dot so the bank reads as a constellation of
-- points in control space.
snapshotSection :: forall m. State -> H.ComponentHTML Action () m
snapshotSection s =
  HH.div_
    [ HH.div [ style "display:flex;align-items:center;justify-content:space-between;margin-bottom:7px" ]
        [ HH.span [ style $ engrave <> ";font-size:9px" ] [ HH.text "SNAPSHOTS" ]
        , HH.button
            [ HE.onClick \_ -> ToggleCap
            , style $ "padding:4px 12px;border:1px solid #a8a392;border-radius:5px;cursor:pointer;"
                <> "font-family:'SF Mono',Menlo,monospace;font-size:9px;letter-spacing:0.08em;"
                <> (if s.capArm then "color:#fbeae7;background:linear-gradient(#b23b28,#9a3120)"
                    else "color:#3f3c33;background:linear-gradient(#efece1,#ddd9cb)") ]
            [ HH.text (if s.capArm then "● ARMED" else "CAPTURE") ]
        ]
    , HH.div [ style "display:grid;grid-template-columns:repeat(4,1fr);gap:6px;max-width:200px" ]
        (map (snapshotSlot s) (range 0 (M.snapshotCount - 1)))
    , HH.div [ style $ engrave <> ";font-size:8px;opacity:0.5;line-height:1.5;margin-top:8px" ]
        [ HH.text (if s.capArm then "ARMED — CLICK A SLOT TO STORE THE CURRENT KIT." else "CLICK CAPTURE THEN A SLOT TO STORE · CLICK A SLOT TO RECALL · SHIFT-CLICK TO CLEAR.") ]
    ]

-- One snapshot slot: a mini X/Y pad. Filled shows the captured cursor as a dot;
-- empty is a faint outline with its index.
snapshotSlot :: forall m. State -> Int -> H.ComponentHTML Action () m
snapshotSlot s i =
  let
    msnap = M.snapshotAt s.bal i
    filled = case msnap of
      Just _ -> true
      Nothing -> false
    body = case msnap of
      Just snap ->
        [ svgEl "svg"
            [ svgAttr "viewBox" "0 0 100 100", svgAttr "width" "30", svgAttr "height" "30"
            , svgAttr "style" "display:block" ]
            [ svgEl "circle"
                [ svgAttr "cx" (show (toNumber snap.x / 255.0 * 100.0))
                , svgAttr "cy" (show ((1.0 - toNumber snap.y / 255.0) * 100.0))
                , svgAttr "r" "13", svgAttr "fill" "#1c1a12" ] []
            ]
        ]
      Nothing ->
        [ HH.span [ style $ engrave <> ";font-size:9px;opacity:0.45" ] [ HH.text (show (i + 1)) ] ]
  in
    HH.div
      [ HE.onClick \e -> SlotClick i (ME.shiftKey e)
      , style $ "width:32px;height:32px;border-radius:5px;cursor:pointer;display:flex;"
          <> "align-items:center;justify-content:center;box-sizing:border-box;"
          <> (if filled then "border:1px solid #a8a392;background:#cfcabb"
              else "border:1px dashed #b3ae9c;background:#00000006") ]
      body

-- One labelled row of knobs (the left label, then the knobs across).
knobRow :: forall m. String -> Array (H.ComponentHTML Action () m) -> H.ComponentHTML Action () m
knobRow label knobs =
  HH.div [ style "display:flex;align-items:flex-start;gap:8px;margin-bottom:4px" ]
    [ HH.div [ style $ engrave <> ";font-size:8px;opacity:0.7;width:42px;flex:0 0 auto;padding-top:6px;text-align:right" ]
        [ HH.text label ]
    , HH.div [ style "display:flex;gap:2px" ] knobs ]

padSvg :: forall m. State -> H.ComponentHTML Action () m
padSvg s =
  let
    b = s.bal
    -- node value 0..255 for grid index 0..4
    nodeVal k = toNumber k * 255.0 / 4.0
    sx v = v
    sy v = 255.0 - v
    i0 = b.x `shr` 6
    j0 = b.y `shr` 6
    bracket i j = (i == i0 || i == i0 + 1) && (j == j0 || j == j0 + 1)
    -- a node dot, radius by its overall energy
    dot i j =
      let
        nv = T.node (T.drumMapIx i j)
        energy = if null nv then 0.0 else toNumber (sum nv) / (96.0 * 255.0)
        r = 3.0 + energy * 7.0
        hot = bracket i j
        dcx = sx (nodeVal i)
        dcy = sy (nodeVal j)
      in
        svgEl "circle"
          [ svgAttr "cx" (show dcx), svgAttr "cy" (show dcy), svgAttr "r" (show r)
          , svgAttr "fill" (if hot then "#7a7460" else "#9a9583")
          , svgAttr "fill-opacity" (if hot then "0.85" else "0.4")
          ] []
    cx = sx (toNumber b.x)
    cy = sy (toNumber b.y)
  in
    svgEl "svg"
      [ svgAttr "viewBox" "-8 -8 272 272", svgAttr "width" "100%", svgAttr "height" "100%"
      , svgAttr "id" padId
      , svgMouse "mousedown" \e -> PadAt (ME.clientX e) (ME.clientY e) (ME.buttons e)
      , svgMouse "mousemove" \e -> PadAt (ME.clientX e) (ME.clientY e) (ME.buttons e)
      , svgMouse "mouseup" \_ -> PadRelease
      , svgAttr "style" "display:block;cursor:crosshair;touch-action:none"
      ]
      ( [ svgEl "rect"
            [ svgAttr "x" "-8", svgAttr "y" "-8", svgAttr "width" "272", svgAttr "height" "272"
            , svgAttr "rx" "8", svgAttr "fill" "#c4bfae", svgAttr "stroke" "#a8a392" ] []
        ]
        <> (range 0 4 `concatMap'` \i -> range 0 4 `concatMap'` \j -> [ dot i j ])
        <> crosshair cx cy
      )

-- crosshair lines + a filled cursor dot
crosshair :: forall w i. Number -> Number -> Array (HH.HTML w i)
crosshair cx cy =
  [ svgEl "line"
      [ svgAttr "x1" "0", svgAttr "y1" (show cy), svgAttr "x2" "255", svgAttr "y2" (show cy)
      , svgAttr "stroke" "#3f3c33", svgAttr "stroke-opacity" "0.25", svgAttr "stroke-width" "0.8" ] []
  , svgEl "line"
      [ svgAttr "x1" (show cx), svgAttr "y1" "0", svgAttr "x2" (show cx), svgAttr "y2" "255"
      , svgAttr "stroke" "#3f3c33", svgAttr "stroke-opacity" "0.25", svgAttr "stroke-width" "0.8" ] []
  , svgEl "circle"
      [ svgAttr "cx" (show cx), svgAttr "cy" (show cy), svgAttr "r" "7"
      , svgAttr "fill" "#1c1a12", svgAttr "stroke" "#efece1", svgAttr "stroke-width" "2" ] []
  ]

-- Flipped concatMap so the call sites read `range … `concatMap'` \i -> …`.
concatMap' :: forall a b. Array a -> (a -> Array b) -> Array b
concatMap' xs f = concatMap f xs

-- An editable MIDI-note tag in a lane gutter: drag up/down to nudge the note.
-- Shared by the Grids heatmap and the fixed grid.
noteTag :: forall m. Number -> Number -> NoteRef -> Int -> H.ComponentHTML Action () m
noteTag x y ref n =
  svgEl "text"
    [ svgAttr "x" (show x), svgAttr "y" (show y)
    , svgAttr "fill" "#3f3c33", svgAttr "fill-opacity" "0.7"
    , svgAttr "font-size" "8.5", svgAttr "font-family" "'SF Mono',Menlo,monospace"
    , svgAttr "style" "cursor:ns-resize"
    , svgMouse "mousedown" \_ -> StartDrag (DNote ref) n ]
    [ HH.text ("♪" <> show n) ]

-- A filled, rounded SVG rect — the cell primitive shared by the fixed grid.
svgRect :: forall w i. Number -> Number -> Number -> Number -> String -> Number -> HH.HTML w i
svgRect x0 y0 wid hgt c op =
  svgEl "rect"
    [ svgAttr "x" (show x0), svgAttr "y" (show y0)
    , svgAttr "width" (show wid), svgAttr "height" (show hgt), svgAttr "rx" "2"
    , svgAttr "fill" c, svgAttr "fill-opacity" (show op) ] []

-- An SVG mouse handler (the `svgEl` row is `()`, so the typed HE.onMouse* props
-- don't fit; coerce the MouseEvent decode like Ui.Knob's mousedown handler).
svgMouse :: forall r i. String -> (ME.MouseEvent -> i) -> HH.IProp r i
svgMouse name f = HE.handler (EventType name) (unsafeCoerce f)

-- ---------------------------------------------------------------------------
-- Pattern heatmap — 3 instruments × 32 steps
-- ---------------------------------------------------------------------------

patternPanel :: forall m. State -> H.ComponentHTML Action () m
patternPanel s =
  panel "PATTERN" "flex:1 1 480px;min-width:380px"
    [ patternSwitcher s
    , case s.active of
        AGrids -> gridsBody s
        AFixed i -> case s.library !! i of
          Just pat -> fixedBody s i pat
          Nothing -> HH.text "—"
    ]

-- The pattern bank: ◆ GRIDS (the live morph engine) plus each library rhythm.
-- Clicking switches what plays; the active chip is brass.
patternSwitcher :: forall m. State -> H.ComponentHTML Action () m
patternSwitcher s =
  HH.div [ style "display:flex;gap:6px;flex-wrap:wrap;margin-bottom:16px;max-width:640px" ]
    ( [ chip "◆ GRIDS" (s.active == AGrids) (SelectPattern AGrids) ]
        <> mapWithIndex (\i pat -> chip pat.name (s.active == AFixed i) (SelectPattern (AFixed i))) s.library
        <> [ newChip ] )

-- The "+ NEW" tab: appends a fresh empty rhythm (dashed to read as an action).
newChip :: forall m. H.ComponentHTML Action () m
newChip =
  HH.button
    [ HE.onClick \_ -> NewPattern
    , style $ "padding:6px 13px;border:1px dashed #a8a392;border-radius:6px;cursor:pointer;"
        <> "font-family:Georgia,serif;font-size:12px;color:#6a6657;background:#00000006" ]
    [ HH.text "+ NEW" ]

chip :: forall m. String -> Boolean -> Action -> H.ComponentHTML Action () m
chip label active act =
  HH.button
    [ HE.onClick \_ -> act
    , style $ "padding:6px 13px;border:1px solid #a8a392;border-radius:6px;cursor:pointer;"
        <> "font-family:Georgia,serif;font-size:12px;letter-spacing:0.04em;"
        <> (if active then "color:#1c1a12;background:linear-gradient(#c8a86a,#b8975a)"
            else "color:#3f3c33;background:linear-gradient(#efece1,#ddd9cb)") ]
    [ HH.text label ]

-- The Grids pattern: the live interpolation heatmap + the snapshot bank +
-- the snapshot sequence (the control-space machinery).
gridsBody :: forall m. State -> H.ComponentHTML Action () m
gridsBody s =
  HH.div_
    [ HH.div [ style "width:100%;max-width:640px;margin:0 auto" ] [ heatSvg s ]
    , HH.div [ style $ engrave <> ";font-size:8px;opacity:0.5;margin-top:10px;line-height:1.6;max-width:640px" ]
        [ HH.text "THE 3 GRIDS VOICES (BD · SD · HH). FAINT = THE INTERPOLATED LANDSCAPE THE X/Y CURSOR SELECTS; SOLID = WHAT FIRES AT THIS DENSITY. DRAG A CELL UP/DOWN TO RATCHET IT." ]
    , HH.div [ style "max-width:640px;margin:20px auto 0" ]
        [ snapshotSection s
        , HH.div [ style "height:1px;background:#00000018;margin:16px 0 12px" ] []
        , sequenceSection s
        ]
    ]

-- A fixed rhythm: the literal lane grid (folded to used lanes, or all 16 when
-- editing), click-to-toggle cells, draggable per-lane notes.
fixedBody :: forall m. State -> Int -> P.FixedPattern -> H.ComponentHTML Action () m
fixedBody s idx pat =
  HH.div_
    [ HH.div [ style "display:flex;align-items:center;gap:10px;max-width:640px;margin:0 auto 12px" ]
        [ HH.input
            [ HP.value pat.name
            , HE.onValueInput SetPatternName
            , style $ "padding:5px 9px;border:1px solid #a8a392;border-radius:5px;background:#f3f1e8;"
                <> "font-family:Georgia,serif;font-size:13px;color:#1c1a12;width:150px" ]
        , armBtn (if s.editing then "● EDITING" else "EDIT") s.editing ToggleEdit
        , HH.span [ style $ engrave <> ";font-size:8px;opacity:0.6;line-height:1.5" ]
            [ HH.text (if s.editing
                then "ALL 16 LANES — CLICK CELLS TO TOGGLE HITS · DRAG A ♪NOTE TO RETUNE A LANE."
                else "CLICK A CELL TO TOGGLE A HIT · EDIT REVEALS ALL 16 LANES TO ADD VOICES.") ]
        ]
    , HH.div [ style "width:100%;max-width:640px;margin:0 auto" ] [ fixedSvg s idx pat ]
    , HH.div [ style $ engrave <> ";font-size:8px;opacity:0.5;margin-top:10px;line-height:1.6;max-width:640px" ]
        [ HH.text ("STARTER RHYTHM · " <> show pat.steps <> " STEPS · " <> show (length (P.usedLanes pat)) <> " OF 16 LANES IN USE. A FIXED LOOP — RECALL INSTANTLY, EDIT TO TASTE. SAMPLES SWAP DOWNSTREAM.") ]
    ]

-- The fixed-rhythm step grid: one row per used lane (kit name + GM note),
-- velocity as cell intensity, playhead sweeping the steps.
fixedSvg :: forall m. State -> Int -> P.FixedPattern -> H.ComponentHTML Action () m
fixedSvg s idx pat =
  let
    -- folded to the lanes in use, or the whole 16-lane kit when editing.
    lanes = if s.editing then range 0 (P.kitSize - 1) else P.usedLanes pat
    nLanes = length lanes
    cols = pat.steps
    colW = 16.0
    rowH = 28.0
    gutter = 34.0                       -- left margin for lane name + MIDI note
    w = gutter + toNumber cols * colW
    h = toNumber nLanes * rowH
    here = s.playStep `mod` cols
    colX step = gutter + toNumber step * colW
    laneEmpty lane = not (any (P.firesAt pat lane) (range 0 (cols - 1)))
    -- visuals only (the coloured hit + a faint slot in edit mode + the tweak-dot
    -- + the selection outline).
    rowVisuals row lane =
      range 0 (cols - 1) `concatMap'` \step ->
        let c = P.cellAt pat lane step
            v = c.vel
            x = colX step
            y = toNumber row * rowH
            slot = if s.editing && v <= 0
              then [ svgEl "rect"
                       [ svgAttr "x" (show (x + 2.0)), svgAttr "y" (show (y + 2.0))
                       , svgAttr "width" (show (colW - 4.0)), svgAttr "height" (show (rowH - 5.0)), svgAttr "rx" "2"
                       , svgAttr "fill" "none", svgAttr "stroke" (laneColor lane), svgAttr "stroke-opacity" "0.16"
                       , svgAttr "stroke-width" "0.8", svgAttr "style" "pointer-events:none" ] [] ]
              else []
            hit = if v <= 0 then []
              else [ svgRect (x + 2.0) (y + 2.0) (colW - 4.0) (rowH - 5.0) (laneColor lane)
                       (0.34 + toNumber v / 127.0 * 0.62) ]
            -- a small dot marks a hit whose prob/cond/ratchet overlay was tweaked.
            dot = if v > 0 && P.cellTweaked c
              then [ svgEl "circle"
                       [ svgAttr "cx" (show (x + colW - 3.6)), svgAttr "cy" (show (y + 4.6)), svgAttr "r" "1.9"
                       , svgAttr "fill" "#1c1a12", svgAttr "fill-opacity" "0.85", svgAttr "style" "pointer-events:none" ] [] ]
              else []
            sel = if s.selected == Just { lane, step }
              then [ svgEl "rect"
                       [ svgAttr "x" (show (x + 0.5)), svgAttr "y" (show (y + 0.5))
                       , svgAttr "width" (show (colW - 1.0)), svgAttr "height" (show (rowH - 1.0)), svgAttr "rx" "3"
                       , svgAttr "fill" "none", svgAttr "stroke" "#1c1a12", svgAttr "stroke-width" "1.4"
                       , svgAttr "stroke-opacity" "0.9", svgAttr "style" "pointer-events:none" ] [] ]
              else []
        in slot <> hit <> dot <> sel
    -- a transparent click target per cell, drawn last so it always wins clicks.
    rowTargets row lane =
      range 0 (cols - 1) `concatMap'` \step ->
        let x = colX step
            y = toNumber row * rowH
        in [ svgEl "rect"
               [ svgAttr "x" (show x), svgAttr "y" (show y)
               , svgAttr "width" (show (colW - 1.0)), svgAttr "height" (show (rowH - 1.0))
               , svgAttr "fill" "rgba(0,0,0,0)", svgAttr "style" "cursor:pointer;pointer-events:all"
               , svgMouse "click" \e -> CellClick lane step (ME.shiftKey e) ] [] ]
    laneAt row = fromMaybe 0 (lanes !! row)
    cells = concatMap (\row -> rowVisuals row (laneAt row)) (range 0 (nLanes - 1))
    targets = concatMap (\row -> rowTargets row (laneAt row)) (range 0 (nLanes - 1))
    beatLines =
      range 0 (cols / 4) `concatMap'` \k ->
        let x = colX (k * 4)
        in [ svgEl "line"
               [ svgAttr "x1" (show x), svgAttr "y1" "0", svgAttr "x2" (show x), svgAttr "y2" (show h)
               , svgAttr "stroke" "#3f3c33", svgAttr "stroke-opacity" "0.18", svgAttr "stroke-width" "0.8"
               , svgAttr "style" "pointer-events:none" ] [] ]
    laneDivider row =
      svgEl "line"
        [ svgAttr "x1" (show gutter), svgAttr "y1" (show (toNumber row * rowH)), svgAttr "x2" (show w)
        , svgAttr "y2" (show (toNumber row * rowH))
        , svgAttr "stroke" "#3f3c33", svgAttr "stroke-opacity" "0.12", svgAttr "stroke-width" "0.6"
        , svgAttr "style" "pointer-events:none" ] []
    -- empty lanes (only visible while editing) are dimmed; named-and-used ones full.
    rowLabel row =
      let lane = laneAt row
          op = if laneEmpty lane then "0.4" else "0.9"
      in [ svgEl "text"
             [ svgAttr "x" "3", svgAttr "y" (show (toNumber row * rowH + 12.0))
             , svgAttr "fill" (laneColor lane), svgAttr "fill-opacity" op, svgAttr "style" "pointer-events:none"
             , svgAttr "font-size" "9", svgAttr "font-weight" "bold", svgAttr "font-family" "Georgia,serif" ]
             [ HH.text (P.laneName lane) ]
         , noteTag 3.0 (toNumber row * rowH + 23.0) (NFixed idx lane) (P.noteOf pat lane)
         ]
    playhead =
      svgEl "rect"
        [ svgAttr "x" (show (colX here)), svgAttr "y" "0"
        , svgAttr "width" (show colW), svgAttr "height" (show h)
        , svgAttr "fill" "#1c1a12", svgAttr "fill-opacity" (if s.running then "0.10" else "0.0")
        , svgAttr "stroke" "#1c1a12", svgAttr "stroke-opacity" (if s.running then "0.5" else "0.15")
        , svgAttr "stroke-width" "1", svgAttr "style" "pointer-events:none" ] []
  in
    svgEl "svg"
      [ svgAttr "viewBox" ("0 0 " <> show w <> " " <> show h)
      , svgAttr "width" "100%", svgAttr "style" "display:block;max-height:90vh" ]
      ( cells <> beatLines
          <> map laneDivider (range 1 (nLanes - 1))
          <> [ playhead ] <> concatMap rowLabel (range 0 (nLanes - 1)) <> targets )

-- The snapshot sequence: a path of slot references the playhead walks, each
-- held `seqBars` bars; advancing recalls that snapshot, morphing the kit. Build
-- it with SEQ+ (then click snapshots in order); play it with ▸.
sequenceSection :: forall m. State -> H.ComponentHTML Action () m
sequenceSection s =
  let
    seq = s.bal.sequence
    n = length seq
  in
    HH.div_
      [ HH.div [ style "display:flex;align-items:center;gap:8px;margin-bottom:8px" ]
          [ HH.span [ style $ engrave <> ";font-size:9px;flex:0 0 auto" ] [ HH.text "SEQUENCE" ]
          , armBtn (if s.seqEnabled then "❚❚ STOP" else "▸ PLAY") s.seqEnabled ToggleSeq
          , armBtn (if s.seqArm then "● BUILD" else "SEQ +") s.seqArm ToggleSeqBuild
          , HH.div [ style "display:flex;align-items:center;gap:4px;margin-left:6px" ]
              [ HH.span [ style $ engrave <> ";font-size:8px;opacity:0.6" ] [ HH.text "BARS/STEP" ]
              , stepBtn "−" (SeqBarsDelta (-1))
              , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:11px;color:#3f3c33;width:14px;text-align:center" ] [ HH.text (show s.bal.seqBars) ]
              , stepBtn "+" (SeqBarsDelta 1)
              ]
          , flatBtn "CLEAR" ClearSeq
          ]
      , if n == 0 then
          HH.div [ style $ engrave <> ";font-size:8px;opacity:0.5;line-height:1.6" ]
            [ HH.text (if s.seqArm then "ARMED — CLICK SNAPSHOTS IN ORDER TO LAY THE PATH." else "PRESS SEQ + THEN CLICK SNAPSHOTS TO LAY A PATH; ▸ PLAYS IT, MORPHING THE KIT EACH STEP.") ]
        else
          HH.div [ style "display:flex;flex-wrap:wrap;gap:5px" ]
            (map (seqCell s) (range 0 (n - 1)))
      ]

-- One step of the sequence lane: the snapshot index, highlighted on the playhead.
seqCell :: forall m. State -> Int -> H.ComponentHTML Action () m
seqCell s p =
  let
    slot = fromMaybe 0 (s.bal.sequence !! p)
    here = s.seqEnabled && p == s.seqPos
  in
    HH.div
      [ style $ "width:26px;height:26px;border-radius:5px;display:flex;align-items:center;justify-content:center;"
          <> "font-family:'SF Mono',Menlo,monospace;font-size:11px;box-sizing:border-box;"
          <> (if here then "background:#1c1a12;color:#efece1;border:1px solid #1c1a12"
              else "background:#cfcabb;color:#3f3c33;border:1px solid #a8a392") ]
      [ HH.text (show (slot + 1)) ]

-- A small square stepper button (− / +).
stepBtn :: forall m. String -> Action -> H.ComponentHTML Action () m
stepBtn label act =
  HH.button
    [ HE.onClick \_ -> act
    , style $ "width:18px;height:18px;border:1px solid #a8a392;border-radius:4px;cursor:pointer;"
        <> "font-family:'SF Mono',Menlo,monospace;font-size:11px;color:#3f3c33;background:#efece1;"
        <> "display:flex;align-items:center;justify-content:center;padding:0" ]
    [ HH.text label ]

-- A small arm/toggle button (brass when active).
armBtn :: forall m. String -> Boolean -> Action -> H.ComponentHTML Action () m
armBtn label active act =
  HH.button
    [ HE.onClick \_ -> act
    , style $ "padding:4px 11px;border:1px solid #a8a392;border-radius:5px;cursor:pointer;"
        <> "font-family:'SF Mono',Menlo,monospace;font-size:9px;letter-spacing:0.06em;"
        <> (if active then "color:#1c1a12;background:linear-gradient(#c8a86a,#b8975a)"
            else "color:#3f3c33;background:linear-gradient(#efece1,#ddd9cb)") ]
    [ HH.text label ]

heatSvg :: forall m. State -> H.ComponentHTML Action () m
heatSvg s =
  let
    b = s.bal
    cols = 32
    colW = 16.0
    rowH = 30.0
    nLanes = 3
    gutter = 34.0                       -- left margin for lane name + MIDI note
    laneY lane = toNumber lane * rowH
    colX step = gutter + toNumber step * colW
    w = gutter + toNumber cols * colW
    h = toNumber nLanes * rowH
    rectBlock x0 y0 wid hgt c op =
      svgEl "rect"
        [ svgAttr "x" (show x0), svgAttr "y" (show y0)
        , svgAttr "width" (show wid), svgAttr "height" (show hgt), svgAttr "rx" "2"
        , svgAttr "fill" c, svgAttr "fill-opacity" (show op) ] []
    accentOutline x0 y0 =
      svgEl "rect"
        [ svgAttr "x" (show (x0 + 1.0)), svgAttr "y" (show (y0 + 1.0))
        , svgAttr "width" (show (colW - 3.0)), svgAttr "height" (show (rowH - 3.0)), svgAttr "rx" "3"
        , svgAttr "fill" "none", svgAttr "stroke" "#1c1a12", svgAttr "stroke-width" "1.2"
        , svgAttr "style" "pointer-events:none" ] []
    -- ratchets as a vertical STACK of n blocks down the tall pill (readable,
    -- since the cell is taller than wide), all at the same (flat) opacity.
    stackBlocks x y c n op =
      if n <= 1 then [ rectBlock (x + 2.0) (y + 2.0) (colW - 5.0) (rowH - 5.0) c op ]
      else
        range 0 (n - 1) `concatMap'` \k ->
          let segH = (rowH - 4.0) / toNumber n
              sy = y + 2.0 + toNumber k * segH
          in [ rectBlock (x + 2.0) sy (colW - 5.0) (segH - 1.0) c op ]
    -- the small ⋮N marker shown on a ratcheted slot that isn't firing.
    ratchetHint x y c n =
      svgEl "text"
        [ svgAttr "x" (show (x + colW / 2.0)), svgAttr "y" (show (y + rowH / 2.0 + 3.0))
        , svgAttr "fill" c, svgAttr "fill-opacity" "0.6", svgAttr "font-size" "8"
        , svgAttr "text-anchor" "middle", svgAttr "font-family" "Georgia,serif"
        , svgAttr "style" "pointer-events:none" ]
        [ HH.text ("⋮" <> show n) ]
    -- a Grids lane cell: faint interpolated landscape + ratcheted hit + accent.
    gridsCell lane step =
      let
        level = M.levelAt b lane step
        fires = M.wouldFire b lane step
        accent = fires && level > 192
        opens = lane == 2 && fires && M.opensAt b step   -- an open hat here
        n = M.ratchetAt b lane step
        x = colX step
        y = laneY lane
        c = if opens then ohColor else instColor lane
        landscape = rectBlock x y (colW - 1.0) (rowH - 1.0) c (toNumber level / 255.0 * 0.32)
        segs = if not fires then [] else stackBlocks x y c n (if accent then 0.95 else 0.7)
        acc = if accent then [ accentOutline x y ] else []
        hint = if n > 1 && not fires then [ ratchetHint x y c n ] else []
      in
        [ landscape ] <> segs <> acc <> hint
    -- Grids cells have no plain action (hits come from X/Y); a vertical drag
    -- sets the beat's ratchet (up = more retriggers).
    gridsTarget lane step =
      let
        x = colX step
        y = laneY lane
      in
        svgEl "rect"
          [ svgAttr "x" (show x), svgAttr "y" (show y)
          , svgAttr "width" (show (colW - 1.0)), svgAttr "height" (show (rowH - 1.0))
          , svgAttr "fill" "rgba(0,0,0,0)", svgAttr "style" "cursor:ns-resize;pointer-events:all"
          , svgMouse "mousedown" \_ -> StartDrag (DCell lane step) (M.ratchetAt b lane step) ] []
    playhead =
      svgEl "rect"
        [ svgAttr "x" (show (colX s.playStep)), svgAttr "y" "0"
        , svgAttr "width" (show colW), svgAttr "height" (show h)
        , svgAttr "fill" "#1c1a12", svgAttr "fill-opacity" (if s.running then "0.10" else "0.0")
        , svgAttr "stroke" "#1c1a12", svgAttr "stroke-opacity" (if s.running then "0.5" else "0.15")
        , svgAttr "stroke-width" "1", svgAttr "style" "pointer-events:none" ] []
    beatLines =
      range 0 8 `concatMap'` \k ->
        let x = colX (k * 4)
        in [ svgEl "line"
               [ svgAttr "x1" (show x), svgAttr "y1" "0", svgAttr "x2" (show x), svgAttr "y2" (show h)
               , svgAttr "stroke" "#3f3c33", svgAttr "stroke-opacity" "0.18", svgAttr "stroke-width" "0.8"
               , svgAttr "style" "pointer-events:none" ] [] ]
    laneDivider lane =
      svgEl "line"
        [ svgAttr "x1" (show gutter), svgAttr "y1" (show (laneY lane)), svgAttr "x2" (show w)
        , svgAttr "y2" (show (laneY lane))
        , svgAttr "stroke" "#3f3c33", svgAttr "stroke-opacity" "0.12", svgAttr "stroke-width" "0.6"
        , svgAttr "style" "pointer-events:none" ] []
    -- the lane name (bold, coloured) + its editable MIDI note below, pulled into
    -- the gutter like the fixed grid.
    rowLabel lane =
      [ svgEl "text"
          [ svgAttr "x" "3", svgAttr "y" (show (laneY lane + 13.0))
          , svgAttr "fill" (instColor lane), svgAttr "fill-opacity" "0.9", svgAttr "style" "pointer-events:none"
          , svgAttr "font-size" "9", svgAttr "font-weight" "bold", svgAttr "font-family" "Georgia,serif" ]
          [ HH.text (M.instName lane) ]
      , noteTag 3.0 (laneY lane + 25.0) (NGrids lane) (M.noteOf lane b)
      ]
    visuals =
      range 0 2 `concatMap'` \lane -> range 0 (cols - 1) `concatMap'` \step -> gridsCell lane step
    targets =
      range 0 2 `concatMap'` \lane -> range 0 (cols - 1) `concatMap'` \step -> [ gridsTarget lane step ]
  in
    svgEl "svg"
      [ svgAttr "viewBox" ("0 0 " <> show w <> " " <> show h)
      , svgAttr "width" "100%", svgAttr "style" "display:block;max-height:90vh" ]
      ( visuals <> beatLines
          <> map laneDivider (range 1 (nLanes - 1))
          <> [ playhead ] <> concatMap rowLabel (range 0 (nLanes - 1)) <> targets )

bigKnob :: forall m. KnobTarget -> String -> String -> M.Balistes -> H.ComponentHTML Action () m
bigKnob target color label b =
  let
    v = knobValue target b
    r = targetRange target
  in
    HH.div [ style "display:flex;flex-direction:column;align-items:center;width:64px" ]
      [ HH.span [ style $ engrave <> ";font-size:9px;margin-bottom:2px" ] [ HH.text label ]
      , HH.div [ style "width:50px;height:50px" ]
          [ knob { cx: 24.0, cy: 24.0, rOuter: 20.0, rInner: 8.0, color, lo: r.lo, hi: r.hi, value: v, ticks: 0 } (StartDrag (DKnob target) v) ]
      , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:10px;color:#3f3c33;margin-top:2px" ]
          [ HH.text (show v) ]
      ]

