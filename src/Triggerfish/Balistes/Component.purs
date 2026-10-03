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

import Data.Array (concatMap, deleteAt, filter, find, length, mapWithIndex, modifyAt, null, range, sortWith, (!!))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Foldable (any, foldl, for_)
import Data.Int (floor, round, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.String (contains) as String
import Data.String.Common (trim) as String
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
import Reef.Balistes.Protocol (encodeBalSim, encodeBTagged, encodeFixed)
import Reef.Balistes.Input as RBI
import Reef.Balistes.Fixed as RF
import Triggerfish.Balistes.Model as M
import Triggerfish.Balistes.Pattern as P
import Triggerfish.Routing.Model as RM
import Triggerfish.Routing.Out as RO
import Reef.Routing as RR
import Triggerfish.Balistes.Types
  ( KnobTarget(..), targetRange, applyKnob, Active(..), ClickMode(..)
  , NoteRef(..), DragKind(..), State, Action(..), activePattern, selectedPattern, patternAt, rhythmEntries, rigUrl, gridCfg
  , editVel, flashWindow
  , padId )
import Triggerfish.Balistes.TriSnapshot (Brain(..), TriSnapshot(..), brainBadge, brainLabel, brainOf, printTri, parseTri, rhythmContent, rhythmOfContent)
import Triggerfish.Glyph as G
import Triggerfish.GlyphView (faIcons)
import Triggerfish.Preset (Preset, indexOfContent, presetAlias, presetLabel)
import Triggerfish.Balistes.Widgets (armBtn, instColor)
import Triggerfish.Balistes.View.Fixed (cellStrip, fixedSvg)
import Triggerfish.Balistes.View.Grids (heatSvg, knobStack, padSvg)
import Triggerfish.Macro (Form(..), parseLane)
import Triggerfish.Balistes.Source as Source
import Triggerfish.Balistes.Store as Store
import Triggerfish.Balistes.Remote as Remote
import Triggerfish.Balistes.Lepidoptera (printPattern, parsePattern)
import Triggerfish.SourceQuery (Query(..))
import Triggerfish.Transport (Sounding(..))
import Reef.Balistes.Sim as Sim
import Triggerfish.Ui.Pointer as Pointer
import Triggerfish.Ui.Style (engrave, style)
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
        , binnacle: Nothing, outs: [], routing: RM.defaultTable, midiName: "…"
        , clockTempo: 120.0, clockLocked: false, clockBeat: 0.0, clockBar: 0, anchorCount: 0
        , nowMicros: 0.0, dragging: Nothing, dragSub: Nothing
        , presets: []
        , identity: Nothing, lastChip: Nothing
        , active: AGrids, editing: false, presetsOpen: false, bankFilter: Nothing, clickMode: Assemble, laneEditOpen: false, fixedSel: 0, lane: "", laneReadout: "", selected: Nothing
        , scratchFixed: Nothing
        , publishMsg: Nothing }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction, handleQuery = handleQuery, initialize = Just Initialize }
    }

-- | Answer the shell: the source (TIDAL tab) — the reflective header (X/Y,
-- | densities, groove, ratchets, tapped pads) over the editable lane/routing
-- | doc — or adopt the rack's shared free-run baseline.
handleQuery :: forall m a. MonadAff m => Query a -> H.HalogenM State Action () Output m (Maybe a)
handleQuery = case _ of
  SetRouting t k -> do
    H.modify_ _ { routing = t }
    pushRouting
    pure (Just k)
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
  -- The ONE transport query (control-surface MISU refactor). The shell pushes this
  -- machine's derived `Sounding`; drum hits are one-shots so there's nothing to
  -- note-off — we only act on the rig edges: entering Rig hands off (re-issuing Rig
  -- re-hands-off), leaving Rig stops the voice. Local emission gates on `== Local`.
  SetSounding s next -> do
    st <- H.get
    when (st.sounding == Rig && s /= Rig) $
      for_ st.binnacle \bin -> liftEffect $ Transport.send (Binnacle.socket bin) "balistes-stop"
    -- Leaving Silent: the Grids model did not advance while stopped, so put it on
    -- the step the clock is at now, as if it had. Its step is the absolute step
    -- mod 32; only the perturbation draws (one per wrap) differ from a model that
    -- kept running, and a rig handoff sends the whole model, so both sides agree.
    when (st.sounding == Silent && s /= Silent) do
      now <- currentStep
      H.modify_ \t -> t { bal = t.bal { step = now `mod` 32 }, nextModelStep = now }
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
    pure (Just (reply (map (\(Tuple _ p) -> { name: p.name, text: printPattern p }) (rhythmEntries s))))
  LoadEntry i next -> do
    H.modify_ _ { active = AFixed i }
    pure (Just next)
  -- parsePattern is strict (only `balistesPattern` text), so it self-guards.
  ImportText txt reply -> case parsePattern txt of
    Just p -> do
      H.modify_ \s -> s { presets = s.presets <> [ presetOfRhythm p ], active = AFixed (length s.presets) }
      persistLib
      pure (Just (reply true))
    Nothing -> pure (Just (reply false))
  -- No pitch quantiser — the rig's harmonic context doesn't apply to Balistes.
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
        outs <- RO.openAll access
        names <- Midi.outputNames access
        -- The status line no longer names ONE port, because routing no longer
        -- has one: which ports matter is now a property of the table, so the
        -- "is anything missing" judgement is made in the view against
        -- `st.routing` (see `routingHealth`). Here we only record what exists.
        let nm = show (length names) <> " ports"
        HS.notify midiL (MidiReady outs nm)
      Nothing -> HS.notify midiL (MidiReady [] "unavailable")
    -- restore the saved artefact: the rhythm library AND the ARRANGE rail (bank +
    -- Falls back to the bundled patterns / empty bank.
    msaved <- liftEffect Store.load
    for_ msaved \sv -> H.modify_ _
      { presets = sv.presets }
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
          H.modify_ \s -> s { presets = mergeRhythmsByName s dbPats }
        _ -> pure unit

  Step tick -> do
    st <- H.get
    -- Atlantis keeps the schedulers RUNNING and mutes only the emit — see
    -- Triggerfish.Transport: "the frontend is muted but keeps its schedulers
    -- running (lockstep animation)". Gating the whole handler on Local stopped
    -- the model dead in Atlantis, so the playhead froze and `bal` never
    -- advanced alongside the BEAM voice it is supposed to co-simulate.
    --
    -- Silent is different: nothing plays anywhere, so there is nothing to
    -- co-simulate, and a step here would only re-render three bands to move a
    -- playhead nobody is listening to. Balistes does no per-step work while
    -- stopped; `SetSounding` realigns the Grids model to the clock when it starts.
    let audible = st.sounding == Local
    unless (st.sounding == Silent) case st.active of
      -- A fixed rhythm: derive the step from the tick (no internal navigator),
      -- then emit each used lane's hit verbatim at its kit note + velocity. Reads
      -- `activePattern` so an ephemeral recalled snapshot (scratchFixed) plays too.
      AFixed _ -> case activePattern st of
        Nothing -> pure unit
        Just pat -> do
          let stepMs = 0.25 * 60000.0 / max 30.0 st.clockTempo
          when audible $ liftEffect $
            -- the SHARED fixed-rhythm render (Reef.Balistes.Fixed.renderFixed) — the
            -- exact code the BEAM voice runs, keyed off the same absolute step, so a
            -- pushed fixed rhythm plays in lockstep. The frontend projects its rich
            -- pattern onto the wire-flat reef pattern (fixedOf).
            for_ (RF.renderFixed (fixedOf pat) tick.index) \e ->
              emitHit st.outs (drumsOf st) stepMs
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
        when audible $ liftEffect $
          -- the three Grids voices (step-quantised, firmware-faithful), resolved by
          -- the SHARED render decision (Reef.Balistes.Sim.renderStep) — the exact
          -- code the BEAM balistes voice runs. A firing HH that clears the OPEN
          -- boundary rings as an open hat and chokes its closed self; ratchet roll
          -- + per-voice Dilla push come back on each event. The runtime only
          -- schedules the result — front and rig can't diverge on the decision.
          for_ (Sim.renderStep bal0 playedStep r.fired) \e ->
            emitHit st.outs (drumsOf st) stepMs
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
  Frame -> do
    st <- H.get
    case st.binnacle of
      Just bin -> do
        now <- liftEffect $ Clock.unixMicrosNow (Binnacle.clock bin)
        r <- liftEffect $ Clock.read (Binnacle.clock bin)
        -- Write only when something drawn or read has moved. Every write
        -- re-renders the whole panel, and at 30 a second, stopped, that was most
        -- of this page's idle CPU. What is drawn: the rounded tempo, the lock,
        -- the bar, and the flashes (which fade against `nowMicros`); what is read:
        -- the tempo (step length). The step is read from the clock where it is
        -- needed (`currentStep`).
        let moved = r.tempo /= st.clockTempo || r.locked /= st.clockLocked
              || r.bar /= st.clockBar || r.anchorCount /= st.anchorCount
              || not (null st.flash)
        when moved $ H.modify_ \s -> s
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

  MidiReady outs nm -> do
    H.modify_ _ { outs = outs, midiName = nm }
    pushRouting

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
                    newVal = clamp r.lo r.hi (d.startVal + delta)
                in H.modify_ \s -> s { bal = applyKnob target newVal s.bal }
              DCell inst step ->
                let newVal = clamp 1 8 (d.startVal + round (toNumber dist / 22.0))
                in H.modify_ \s -> s { bal = M.setRatchetAt inst step newVal s.bal }
              DNote ref ->
                let newVal = clamp 0 127 (d.startVal + round (toNumber dist / 7.0))
                in H.modify_ \s -> case ref of
                     NGrids lane -> s { bal = M.setNote lane newVal s.bal }
                     NFixed i lane -> modRhythmAt i (P.setNoteAt lane newVal) s
      Nothing -> pure unit
  DragEnd -> do
    st <- H.get
    for_ st.dragSub H.unsubscribe
    -- Live knob sync: broadcast the SETTLED gesture to the rig as a tick-tagged
    -- BInput. reef_balistes_voice applies it (via the shared reef applyBInput) on the
    -- tagged model step, so the rig follows the edit. Absolute idempotent setters, so
    -- replaying the settled value lands the rig exactly where the drag settled.
    for_ (st.dragging >>= \d -> dragToBInput d.kind st.bal) broadcastBInput
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
  -- Naming is promotion: a blank field means "still just a capture", so it
  -- clears back to Nothing rather than storing an empty string — otherwise a
  -- cleared name would read as a named-but-nameless artefact, and `presetLabel`
  -- would show a blank row instead of falling back to the glyph alias.
  -- `persist` (not `persistLib`): the bank is not the rig's business, so there
  -- is nothing to re-push.
  RenamePreset i name -> do
    let trimmed = String.trim name
    H.modify_ \s -> s
      { presets = fromMaybe s.presets
          (modifyAt i (_ { name = if trimmed == "" then Nothing else Just trimmed }) s.presets)
      }
    persist
  -- Save a bank entry to the shared store, any brain. Reuses `publishMsg` as the
  -- status line so the store's answer appears in one place rather than growing a
  -- second reporting channel for the same operation.
  SavePreset i -> do
    st <- H.get
    case st.presets !! i of
      Nothing -> pure unit
      Just p -> do
        let brain = maybe "·" brainBadge (brainOf <$> parseTri p.content)
        H.modify_ _ { publishMsg = Just "saving…" }
        res <- liftAff (attempt (Remote.publishSnapshot p.content (presetLabel p) brain))
        H.modify_ _ { publishMsg = Just case res of
          Right hash -> "✓ saved · " <> take 8 hash
          Left _ -> "✗ save failed (store offline?)" }

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
    -- Close the modal on picking, same as RecallPreset ("you picked a thing, you
    -- want to see it land on the bands"). The two halves of one modal behaved
    -- differently: choosing a rhythm left the panel covering the band it had just
    -- changed. Harmless when the modal is not open.
    H.modify_ \s -> s
      { active = a, scratchFixed = Nothing, publishMsg = Nothing
      , presetsOpen = false
      , fixedSel = case a of
          AFixed i -> i
          _ -> s.fixedSel }
    st <- H.get
    when (st.sounding == Rig) case a of
      AFixed _ -> repushFixed
      AGrids -> pushHandoff st
  ToggleEdit -> H.modify_ \s -> s { editing = not s.editing }
  -- A click on an empty cell adds a hit at the default velocity and selects it
  -- for the NOTE inspector; on a lit cell it selects it; on the SELECTED lit
  -- cell it clears it. Shift-click clears any cell. On a recalled snapshot the
  -- first edit makes an editable copy (`thawed`) rather than doing nothing.
  CellClick lane step shift -> do
    H.modify_ \s0 ->
      let
        s = thawed s0
        here = Just { lane, step }
        lit = maybe false (\p -> P.firesAt p lane step) (selectedPattern s)
        clear = modRhythmAt s.fixedSel (P.modifyCell lane step (const P.emptyCell)) s
      in
        if shift then clear { selected = if s.selected == here then Nothing else s.selected }
        else if lit && s.selected == here then clear { selected = Nothing }
        else (modRhythmAt s.fixedSel (\p -> if P.firesAt p lane step then p else P.modifyCell lane step (const (P.hitCell editVel)) p) s)
          { selected = here }
    persistLib
  SetCellVel d -> do
    H.modify_ (modSelectedCell \c -> c { vel = clamp 1 127 (c.vel + d) })
    persistLib
  SetCellProb d -> do
    H.modify_ (modSelectedCell \c -> c { prob = clamp 0 100 (c.prob + d) })
    persistLib
  SetCellRatchet d -> do
    H.modify_ (modSelectedCell \c -> c { ratchet = clamp 1 8 (c.ratchet + d) })
    persistLib
  CycleCellCond -> do
    H.modify_ (modSelectedCell \c -> c { cond = P.cycleCond c.cond })
    persistLib
  ClearSelected -> do
    H.modify_ \s -> case s.selected of
      Just { lane, step } | isNothing s.scratchFixed ->
        (modRhythmAt s.fixedSel (P.modifyCell lane step (const P.emptyCell)) s) { selected = Nothing }
      _ -> s
    persistLib
  -- a fresh empty rhythm, selected and opened in EDIT so all 16 lanes show.
  NewPattern -> do
    H.modify_ \s ->
      let n = length s.presets
          p = P.emptyPattern ("pattern " <> show (length (rhythmEntries s) + 1)) 32
      in s { presets = s.presets <> [ presetOfRhythm p ]
           , active = AFixed n, fixedSel = n, editing = true, selected = Nothing }
    persistLib
  -- One rename path. `modRhythmAt` lifts the new name into the ENVELOPE and
  -- re-prints the content name-stripped, so the two can never disagree — and the
  -- glyph, being a fingerprint of the sound alone, does not move when you rename.
  SetPatternName name -> do
    H.modify_ \s -> let t = thawed s in modRhythmAt t.fixedSel (_ { name = name }) t
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
    -- The routing table first, so the handoff's first hit already goes where
    -- the table says.
    pushRouting
    -- Lockstep HANDOFF: project the frontend Balistes state to a BalSim (the shared
    -- serializable subset), encode with the reef codec, and push it phase-aligned to
    -- the rig. reef_balistes_voice decodes with the SAME codec (decodeBalSim) and runs
    -- the SAME stepBal + renderStep, holding the pushed state until absolute step
    -- nextModelStep so the browser and the rig, through one routing table, play it on the same
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
  -- Purely a view narrowing; nothing to persist and nothing to push. Not saved
  -- across reloads either — which brain you were reading last session is not a
  -- preference worth restoring, and a filter that survives a reload is a good way
  -- to conclude your patterns have vanished.
  SetBankFilter mb -> H.modify_ _ { bankFilter = mb }
  SetClickMode m -> H.modify_ _ { clickMode = m }
  OpenLaneEdit -> H.modify_ _ { laneEditOpen = true }
  CloseLaneEdit -> H.modify_ _ { laneEditOpen = false }
  NoOp -> pure unit

-- | Project the frontend Balistes record onto the shared `BalSim` — the lockstep
-- | subset (engine state + render overlay). Snapshots/sequence stay frontend-only.
balSimOf :: M.Balistes -> Sim.BalSim
balSimOf b =
  { x: b.x, y: b.y, densBd: b.densBd, densSd: b.densSd, densHh: b.densHh
  , randomness: b.randomness, step: b.step, perts: b.perts, rng: b.rng
  , notes: b.notes, open: b.open, push: b.push, ratchet: b.ratchet }

-- | Steps to defer a synced gesture: tagged for currentStep + this. It must clear
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
-- | Read from the clock when it is needed rather than kept in state: keeping it
-- | current meant writing state, and so re-rendering, eight times a second.
currentStep :: forall o m. MonadAff m => H.HalogenM State Action () o m Int
currentStep = do
  st <- H.get
  case st.binnacle of
    Just bin -> do
      r <- liftEffect $ Clock.read (Binnacle.clock bin)
      pure (floor (r.beat / 0.25))
    Nothing -> pure (floor (st.clockBeat / 0.25))

-- | Format a tick-tagged BInput for the wire, tagged a few steps ahead of `step`
-- | so the rig applies it on the same model step the frontend is heading toward.
balInputMsg :: Int -> RBI.BInput -> String
balInputMsg step input =
  "balistes-input " <> encodeBTagged { tick: step + inputBufferSteps, input }

-- | Broadcast a settled gesture to the rig as a tick-tagged BInput (no-op if no rig
-- | is connected). The frontend has already applied it locally; the rig applies it
-- | (via the shared applyBInput) on the tagged step and converges.
broadcastBInput :: forall o m. MonadAff m => RBI.BInput -> H.HalogenM State Action () o m Unit
broadcastBInput input = do
  st <- H.get
  now <- currentStep
  -- Rig-send only in ATLANTIS (onRig = not audible); SOLO is silent to the rig.
  when (st.sounding == Rig) $ for_ st.binnacle \bin ->
    liftEffect $ Transport.send (Binnacle.socket bin) (balInputMsg now input)

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

-- | Restore a `TriSnapshot`: switch the active tab to its brain, restore that
-- | brain's state, and — when rig-authoritative — push the matching handoff so the
-- | rig follows. The rig side re-modes in place on balistes-sim-at / -fixed, so a
-- | mid-sequence Grids→Rytm march is just two pushes, no gap.
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
    let idx = fromMaybe 0 (map fst (find (\(Tuple _ q) -> q.name == pat.name) (rhythmEntries st)))
    H.modify_ _ { active = AFixed idx, scratchFixed = Just pat }
    st2 <- H.get
    when (st2.sounding == Rig) $ for_ st2.binnacle \bin ->
      liftEffect $ Transport.send (Binnacle.socket bin) ("balistes-fixed " <> encodeFixed (fixedOf pat))
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

-- | Deferred-on-both: enqueue a gesture locally AND broadcast it, both tagged for the
-- | same near-future step. The Step-loop drain applies it here, the voice applies it
-- | on the rig — both on the SAME model step. Needed for gestures that shift the step
-- | (Reset), where broadcast-on-settle would offset the pattern.
enqueueBInput :: forall o m. MonadAff m => RBI.BInput -> H.HalogenM State Action () o m Unit
enqueueBInput input = do
  st <- H.get
  now <- currentStep
  let tag = now + inputBufferSteps
  -- Local always applies (SOLO plays it); rig-send only in ATLANTIS.
  H.modify_ \s -> s { pending = s.pending <> [ { step: tag, input } ] }
  when (st.sounding == Rig) $ for_ st.binnacle \bin ->
    liftEffect $ Transport.send (Binnacle.socket bin) (balInputMsg now input)

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
savedOf s = { presets: s.presets }

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

-- | What the nav shows instead of a channel number: how many drum lanes are
-- | routed somewhere, and — loudly — how many have a leg whose port is missing.
-- |
-- | A leg pointing at an absent port is the silent failure this rig keeps
-- | producing: the sequence plays, the meters move, and one destination is dead.
-- | Counting them is cheap and turns it into something you can read.
routingHealth :: State -> String
routingHealth s =
  let ports = RO.portNames s.outs
      lanes = map RM.SDrumLane (range 0 15)
      legsOf src = RM.liveLegsFor s.routing src
      broken = length (filter (\l -> RM.reachOf { found: ports, rigUp: true } l.dest /= RM.Reachable)
                        (concatMap legsOf lanes))
      routed = length (filter (\src -> not (null (legsOf src))) lanes)
  in if broken > 0
       then show routed <> " lanes · " <> show broken <> " UNREACHABLE"
       else show routed <> " lanes routed"

-- | The note that names each drum lane: `canonKit` in order. Lanes are kit
-- | positions, so the note IS the lane, which is why all three brains share
-- | one table.
kitNotes :: Array Int
kitNotes = map _.note P.canonKit

-- | The routing table as this machine plays it, and as the rig is told to:
-- | `Reef.Routing`'s form, resolved against the ports that exist.
drumsOf :: State -> RR.DrumRouting
drumsOf st = RO.drumRouting st.outs st.routing kitNotes

-- | Tell the rig the routing table, when it is the rig that sounds. Not before
-- | MIDI access arrives: with no ports known every leg resolves to nothing,
-- | and pushing that would silence the rig.
pushRouting :: forall o m. MonadAff m => H.HalogenM State Action () o m Unit
pushRouting = do
  st <- H.get
  when (st.sounding == Rig && not (null st.outs)) $ for_ st.binnacle \bin ->
    liftEffect $ Transport.send (Binnacle.socket bin) ("balistes-routing " <> RR.encodeDrumRouting (drumsOf st))

-- | Emit one already-resolved hit down its lane's legs, `delay0` from now for
-- | `durMs`, or, when the cell is ratcheted (n > 1), as n evenly-spaced
-- | retriggers at flat velocity. Push/ratchet/open are resolved by the caller;
-- | where it goes is `Reef.Routing.drumSends`, the function the rig voice calls.
emitHit
  :: RO.Outs -> RR.DrumRouting -> Number -> Number -> Int -> Number -> Int -> Int -> Effect Unit
emitHit outs drums stepMs delay0 note durMs velocity n =
  if n <= 1 then
    void $ RO.sendAll outs (RR.drumSends drums { note, velocity, atMs: delay0, durMs, stepMs })
  else
    let sub = stepMs / toNumber n
    in for_ (range 0 (n - 1)) \k ->
         void $ RO.sendAll outs (RR.drumSends drums
           { note, velocity, atMs: delay0 + toNumber k * sub, durMs: sub * 0.9, stepMs: sub })

-- | Apply a function to library pattern `i` (no-op if out of range).
-- | Apply `f` to the rhythm in bank entry `i` and write it back.
-- |
-- | The write-back is where the single-source-of-truth invariant is maintained:
-- | the content is re-printed NAME-STRIPPED and any name `f` set is lifted into
-- | the envelope. So a caller may go on treating the name as a field of the
-- | pattern (as every edit path did when the library was `Array FixedPattern`)
-- | without the stored text ever growing a second, divergent copy of it.
-- |
-- | No-op when the entry is out of range or belongs to another brain — `fixedSel`
-- | now indexes the whole bank, so both are reachable states.
modRhythmAt :: Int -> (P.FixedPattern -> P.FixedPattern) -> State -> State
modRhythmAt i f s = case patternAt s i of
  Nothing -> s
  Just pat ->
    let pat' = f pat
    in s { presets = fromMaybe s.presets (modifyAt i (setRhythm pat') s.presets) }
  where
  setRhythm pat' p = p
    { content = rhythmContent pat'
    , name = if pat'.name == "" then Nothing else Just pat'.name
    }

-- | A fresh bank entry holding one rhythm, named from the pattern.
presetOfRhythm :: P.FixedPattern -> Preset
presetOfRhythm p =
  { content: rhythmContent p
  , name: if p.name == "" then Nothing else Just p.name
  , starred: false
  }

-- | Fold Amphora's `balistes-grid` rhythms into the bank, keyed by NAME: keep
-- | every local entry (it may carry unsaved edits), append only rhythms the bank
-- | has never seen.
-- |
-- | Name is still the key, but the comparison is now scoped to rhythms rather
-- | than run over the whole bank. Once other brains' artefacts are savable to the
-- | store too, a bare name is no longer unique across the collection — a Grids
-- | point and a rhythm can share one — so the brain has to be part of any
-- | cross-brain key. Filtering to `rhythmEntries` first is what keeps that true
-- | here (see docs/DESIGN-balistes-bank-coherence.md, slice 5 hazards).
mergeRhythmsByName :: State -> Array P.FixedPattern -> Array Preset
mergeRhythmsByName s incoming =
  let known = map (\(Tuple _ p) -> p.name) (rhythmEntries s)
  in s.presets <> map presetOfRhythm (filter (\p -> not (any (_ == p.name) known)) incoming)

-- | Apply a function to the selected cell of the active fixed rhythm.
-- | A recalled rhythm plays as a frozen snapshot (`scratchFixed`), so the banked
-- | preset it came from is never changed behind your back. Editing it used to
-- | do nothing at all, silently. Now the first edit makes it a library rhythm of
-- | its own, named as the recalled one plus " (edited)", selected and sounding,
-- | and the edit lands on that.
thawed :: State -> State
thawed s = case s.scratchFixed of
  Nothing -> s
  Just pat ->
    let
      n = length s.presets
      copy = pat { name = (if pat.name == "" then "rhythm" else pat.name) <> " (edited)" }
    in
      s { presets = s.presets <> [ presetOfRhythm copy ]
        , active = AFixed n, fixedSel = n, scratchFixed = Nothing, selected = Nothing }

modSelectedCell :: (P.Cell -> P.Cell) -> State -> State
modSelectedCell f s = case s.selected of
  Just { lane, step } | isNothing s.scratchFixed ->
    modRhythmAt s.fixedSel (P.modifyCell lane step f) s
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
            [ mutableBand s, gridsBand s ]
        , assemblePanel s
        ]
    , presetModal s
    , laneEditModal s
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
    [ HH.div [ style "display:flex;align-items:baseline;justify-content:space-between;border-bottom:1px solid #00000018;padding-bottom:6px;margin-bottom:10px" ]
        [ HH.span [ style $ engrave <> ";font-size:12px;letter-spacing:0.16em;color:#3f3c33" ]
            [ HH.text "ASSEMBLE" ]
        , HH.button
            [ HE.onClick \_ -> OpenLaneEdit
            , HP.title "write the expression, with the notation guide"
            , style $ engrave <> ";font-size:8px;letter-spacing:0.1em;padding:2px 8px;border-radius:5px;"
                <> "border:1px solid #00000022;color:#6a6657;background:transparent;cursor:pointer" ]
            [ HH.text "EDIT…" ]
        ]
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
    , HH.div [ style "display:flex;align-items:center;justify-content:space-between;border-bottom:1px solid #00000014;padding-bottom:4px;margin-bottom:8px" ]
        [ HH.span [ style $ engrave <> ";font-size:9px;color:#8a8676" ] [ HH.text "BEATS" ]
        , clickModeSwitch s
        ]
    , HH.div [ style "display:flex;gap:5px;flex-wrap:wrap;margin-bottom:12px" ]
        (if null s.presets
           then [ HH.span [ style $ engrave <> ";font-size:8px;opacity:0.5" ] [ HH.text "NOTHING BANKED YET" ] ]
           else mapWithIndex (presetToken s) s.presets)
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
           (faIcons g)
    else
      HH.span
        [ HP.title (a <> " — not a banked snapshot, so this step will not resolve")
        , style "font-family:'SF Mono',Menlo,monospace;font-size:10px;color:#a8562f;white-space:nowrap" ]
        [ HH.text (a <> " ?") ]

-- One banked snapshot as a click-to-insert glyph pair. Inserting writes the ALIAS
-- (quoted) into the lane — the stable, content-derived name — while what you click
-- and read is the glyph.
-- | One banked beat as its 2-glyph. What clicking does depends on `clickMode`:
-- | AUDITION plays it (select a rhythm, recall another brain's capture — the same
-- | split the bank list uses), ASSEMBLE appends its token to the lane.
-- |
-- | The index is the bank position, which is what selection and recall address.
presetToken :: forall m. State -> Int -> Preset -> H.ComponentHTML Action () m
presetToken s i p =
  let alias = presetAlias p
      g = G.glyphFromAlias alias
      auditioning = s.clickMode == Audition
      act = if auditioning
              then (if isRhythm p then SelectPattern (AFixed i) else RecallPreset i)
              else InsertLaneToken alias
  in HH.button
      [ HE.onClick \_ -> act
      , HP.title (fromMaybe alias p.name
          <> (if auditioning then " — play it" else " — append to the lane"))
      , style $ "display:flex;align-items:center;gap:3px;padding:3px 8px;border:1px solid #a8a392;"
          <> "border-radius:5px;cursor:pointer;background:#f3f1e8" ]
      (faIcons g)

-- | AUDITION / ASSEMBLE. The Vetula HUNT/PERFORM shape: one switch saying what
-- | the gesture below it means, so you can jam on the banked beats to find what
-- | works and then write with the same clicks — without leaving the surface for
-- | the preset modal, which covers the bands you are listening to.
clickModeSwitch :: forall m. State -> H.ComponentHTML Action () m
clickModeSwitch s =
  HH.div [ style "display:flex;gap:0;align-items:center" ]
    [ seg Audition "AUDITION", seg Assemble "ASSEMBLE" ]
  where
  seg m label =
    let on = s.clickMode == m
    in HH.button
         [ HE.onClick \_ -> SetClickMode m
         , HP.title (case m of
             Audition -> "clicking a beat plays it"
             Assemble -> "clicking a beat appends it to the lane")
         , style $ engrave <> ";font-size:8px;letter-spacing:0.1em;padding:2px 7px;cursor:pointer;"
             <> "border:1px solid " <> (if on then "#6f6a5c" else "#00000018")
             <> ";color:" <> (if on then "#2f2c25" else "#8a8676")
             <> ";background:" <> (if on then "#00000012" else "transparent") ]
         [ HH.text label ]

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
      , navReadout (routingHealth s)
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
            (faIcons g)
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
    -- "PUBLISH" read as a transport verb and said nothing about where the bytes
    -- went. Naming the destination also disambiguates it from the CONTINUOUS
    -- local save (persistLib writes to localStorage on every edit) — this is the
    -- one that puts it somewhere else, not the one that stops you losing work.
    , armBtn "SAVE TO AMPHORA ⚱" false PublishActive
    , case s.publishMsg of
        Just msg -> HH.span [ style $ engrave <> ";font-size:8px;color:#2f6a4a" ] [ HH.text msg ]
        Nothing -> HH.text ""
    , cellStrip s
    ]

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
-- | The lane EDITOR — a full-size surface for writing the tidal expression of a
-- | combined beat, with the notation guide right there. Same shape as Vetula's
-- | sequence editor (`Vetula.App.perfEditModal`): title, the expression, a live
-- | reading of it, click-to-use examples, and the grammar beside the field so the
-- | notation is learnable where it is used rather than in a doc.
-- |
-- | The ASSEMBLE panel keeps its inline box for quick edits; this is where you
-- | compose. Both write the same `lane`, so there is one source of truth and no
-- | commit step — `SetLane` already raises `LaneEdited` to the shell, which owns
-- | and persists it.
-- |
-- | Backdrop and panel are SIBLINGS, matching `presetModal` below: nesting would
-- | need the panel to stop click propagation, which needs an Effect-carrying
-- | no-op action. Siblings get the same behaviour with no plumbing.
laneEditModal :: forall m. State -> H.ComponentHTML Action () m
laneEditModal s =
  if not s.laneEditOpen then HH.text ""
  else
    HH.div [ style "position:fixed;inset:0;z-index:60" ]
      [ HH.div
          [ style "position:absolute;inset:0;background:#00000055"
          , HE.onClick \_ -> CloseLaneEdit ]
          []
      , HH.div
          [ style $ "position:absolute;top:50%;left:50%;transform:translate(-50%,-50%);"
              <> "width:720px;max-width:94vw;max-height:86vh;overflow-y:auto;border-radius:10px;"
              <> "background:linear-gradient(#f6f2e8,#efe9db);border:1px solid #a8a392;"
              <> "box-shadow:0 14px 48px #00000044;padding:20px 24px" ]
          [ HH.div [ style "display:flex;align-items:baseline;justify-content:space-between;margin-bottom:14px" ]
              [ HH.span [ style $ engrave <> ";font-size:13px;letter-spacing:0.16em;color:#3f3c33" ]
                  [ HH.text "BEAT EXPRESSION" ]
              , HH.button
                  [ HE.onClick \_ -> CloseLaneEdit
                  , style "border:none;background:none;color:#8a8676;font-size:16px;cursor:pointer;line-height:1" ]
                  [ HH.text "✕" ]
              ]
          , HH.textarea
              [ HP.value s.lane
              , HE.onValueInput SetLane
              , HP.placeholder "\"lo house 110\" <\"trap 140\" ~> # dilla"
              , style $ "width:100%;box-sizing:border-box;height:120px;resize:vertical;padding:9px 11px;"
                  <> "border:1px solid #a8a392;border-radius:6px;background:#f3f1e8;"
                  <> "font-family:\'SF Mono\',Menlo,monospace;font-size:13px;line-height:1.6;color:#1c1a12" ]
          -- The same score the panel draws, so what you are typing is read back as
          -- glyphs immediately — an unresolvable atom shows as `name ?` rather
          -- than silently doing nothing at play time.
          , laneScore s
          , HH.div [ style $ engrave <> ";font-size:8px;opacity:0.6;margin:9px 0 14px;line-height:1.6" ]
              [ HH.text ("NOW: " <> (if s.laneReadout == "" then "—" else s.laneReadout)) ]
          , modalSection "BEATS" "click to append — every banked beat, all three machines"
              (HH.div [ style "display:flex;gap:5px;flex-wrap:wrap" ]
                (if null s.presets
                   then [ HH.span [ style $ engrave <> ";font-size:8px;opacity:0.5" ] [ HH.text "NOTHING BANKED YET" ] ]
                   else mapWithIndex (\i p -> presetToken (s { clickMode = Assemble }) i p) s.presets))
          , modalSection "STRUCTURE" "rests, alternation, transform stacks"
              (HH.div [ style "display:flex;gap:5px;flex-wrap:wrap" ]
                (map laneToken [ "~", "<", ">", "# dilla", "# flat" ]))
          , modalSection "EXAMPLES" "click to replace the expression"
              (HH.div [ style "display:flex;gap:5px;flex-wrap:wrap" ]
                (map exampleChip laneExamples))
          , modalSection "NOTATION" "one step per token; steps divide the macro-cycle"
              (HH.div [ style "display:grid;grid-template-columns:auto 1fr;gap:4px 14px;font-size:11px;color:#55503f" ]
                (concatMap guideRow laneGuide))
          ]
      ]
  where
  exampleChip ex =
    HH.button
      [ HE.onClick \_ -> SetLane (fst ex)
      , HP.title (snd ex)
      , style $ "padding:3px 9px;border:1px solid #a8a392;border-radius:5px;cursor:pointer;"
          <> "font-family:\'SF Mono\',Menlo,monospace;font-size:10px;color:#3f3c33;background:#f3f1e8" ]
      [ HH.text (fst ex) ]
  guideRow g =
    [ HH.span [ style "font-family:\'SF Mono\',Menlo,monospace;color:#3f3c33" ] [ HH.text (fst g) ]
    , HH.span [] [ HH.text (snd g) ]
    ]

-- | Worked expressions, in the order you would meet them. The fourth is AC\'s
-- | motivating example — four bars of one beat, a bar of another, a fill every
-- | other cycle — which the grammar already expresses at bars-per-step 1.
laneExamples :: Array (Tuple String String)
laneExamples =
  [ Tuple "a b" "two beats, one per step"
  , Tuple "a ~ b ~" "with rests between them"
  , Tuple "<a b> c" "alternates a and b on successive cycles"
  , Tuple "a a a a b <~ c>" "four of a, then b, then c every other cycle"
  , Tuple "a # dilla" "a, with the dilla push applied"
  ]

laneGuide :: Array (Tuple String String)
laneGuide =
  [ Tuple "name" "a banked beat, by its glyph alias (click one above)"
  , Tuple "~" "a rest — the machine goes quiet for that step"
  , Tuple "<a b>" "alternation: a different member each cycle"
  , Tuple "# verb" "a transform applied to the step (# dilla, # flat, # scale)"
  ]

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
          , bankFilterRow s
          , modalSection "BANK"
              "every machine's artefacts in one list — named ones are yours, the rest are captures"
              (bankList s)
          ]
      ]

-- | ALL / G / R / T, plus the +NEW action that used to ride at the end of the
-- | rhythm chips. Filtering is a VIEW over one collection, not a partition of it:
-- | the bank exists to hold the three brains intermingled (that is the
-- | macro-tidal purpose in DESIGN-tri-snapshot.md), so narrowing is something you
-- | do to read it, never something that splits it.
bankFilterRow :: forall m. State -> H.ComponentHTML Action () m
bankFilterRow s =
  HH.div [ style "display:flex;align-items:center;gap:6px;margin-bottom:10px" ]
    ( [ filterBtn Nothing "ALL" ]
        <> map (\b -> filterBtn (Just b) (brainBadge b)) [ BGrids, BFixed ]
        <> [ HH.div [ style "flex:1 1 auto" ] []
           , HH.button
               [ HE.onClick \_ -> NewPattern
               , HP.title "new empty rhythm"
               , style $ "padding:4px 11px;border:1px dashed #a8a392;border-radius:6px;cursor:pointer;"
                   <> "font-family:Georgia,serif;font-size:11px;color:#6a6657;background:#00000006" ]
               [ HH.text "+ NEW RHYTHM" ]
           ] )
  where
  filterBtn mb label =
    let on = s.bankFilter == mb
    in HH.button
         [ HE.onClick \_ -> SetBankFilter mb
         , HP.title (maybe "show every brain" brainLabel mb)
         , style $ engrave <> ";font-size:9px;letter-spacing:0.1em;padding:3px 9px;border-radius:5px;cursor:pointer;"
             <> "border:1px solid " <> (if on then "#6f6a5c" else "#00000018")
             <> ";color:" <> (if on then "#2f2c25" else "#8a8676")
             <> ";background:" <> (if on then "#00000010" else "transparent") ]
         [ HH.text label ]

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
-- | The bank, starred entries first.
-- |
-- | Sorted on STAR ONLY, deliberately — star trumps naming (AC, 2026-08-07), and
-- | an anonymous starred capture is a perfectly good go-to. Naming is a separate
-- | axis (promotion, see `RenamePreset`) and must NOT sort, because the name is
-- | edited character-by-character in a live input: re-ordering on `name` would
-- | slide the row out from under the cursor on the first keystroke. A star is one
-- | deliberate click, so a row jumping to the top there is the feedback you want.
-- |
-- | `sortWith` is stable, so entries keep capture order within each group.
-- |
-- | NB the index carried through is the ORIGINAL position in `s.presets`, since
-- | that is what Star/Delete/Rename address. Sorting bare presets and re-indexing
-- | would silently point every row's actions at the wrong entry.
bankList :: forall m. State -> H.ComponentHTML Action () m
bankList s =
  let rows = sortWith (\(Tuple _ p) -> if p.starred then 0 else 1)
               (filter (\(Tuple _ p) -> matchesFilter s p) (mapWithIndex Tuple s.presets))
  in if null rows
       then HH.div [ style $ engrave <> ";font-size:9px;opacity:0.5" ]
              [ HH.text (if null s.presets
                  then "NOTHING BANKED YET — THE `c` HOTKEY CAPTURES THE LIVE STATE."
                  else "NOTHING FOR THIS BRAIN — TRY ALL.") ]
       else HH.div [ style "display:flex;flex-direction:column;gap:4px" ]
              (map (\(Tuple i p) -> snapshotRow i p) rows)

-- | Does this entry pass the current brain filter? Unparseable entries show only
-- | under ALL — they badge as `·` and there is no brain to file them under, but
-- | hiding them entirely would make a decode problem invisible.
matchesFilter :: State -> Preset -> Boolean
matchesFilter s p = case s.bankFilter of
  Nothing -> true
  Just b -> (brainOf <$> parseTri p.content) == Just b

-- | Is this bank entry a rhythm? Rhythms fold into the same bank as of v5, but
-- | until the two modal sections merge (slice 5b) they keep their existing homes:
-- | RHYTHMS renders them, SNAPSHOTS renders everything else. Without this the
-- | fold would dump a dozen rhythms into the captures list and the step that was
-- | supposed to change nothing visible would rearrange the screen.
isRhythm :: Preset -> Boolean
isRhythm p = isJust (rhythmOfContent (fromMaybe "" p.name) p.content)

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
      -- The badge + glyph pair are the RECALL target. The name field is a
      -- SIBLING, not a child: nesting an input inside the click handler would
      -- recall the snapshot on every attempt to put the cursor in the field.
      -- One gesture, brain-appropriate meaning. A rhythm IS the artefact now that
      -- the library folded in, so clicking it selects it for editing (what the
      -- RHYTHMS chips did). Another brain's entry is a captured moment, so
      -- clicking it restores that state (what SNAPSHOTS did). Recalling a rhythm
      -- ephemerally — the old scratchFixed path — is still reachable, but it is
      -- no longer the obvious meaning of clicking your own saved rhythm.
      , HH.span
          [ HE.onClick \_ -> if isRhythm p then SelectPattern (AFixed i) else RecallPreset i
          , HP.title (if isRhythm p then "select for editing" else "recall")
          , style "display:flex;align-items:center;gap:8px;cursor:pointer;flex:0 0 auto" ]
          [ brainTag p
          , HH.span [ style "display:inline-flex;align-items:center;gap:3px" ] (faIcons g)
          ]
      -- Placeholder is the glyph alias, so an unnamed capture shows the very
      -- label it is going by — the field reads as "this is its name until you
      -- give it one", which is the promotion story made visible.
      , HH.input
          [ HP.value (fromMaybe "" p.name)
          , HP.placeholder (presetAlias p)
          , HE.onValueInput (RenamePreset i)
          , HP.title "name this snapshot (blank = leave it anonymous)"
          , style $ "flex:1 1 auto;min-width:0;padding:2px 6px;border:1px solid transparent;"
              <> "border-radius:4px;background:transparent;"
              <> "font-family:'SF Mono',Menlo,monospace;font-size:10px;color:#4a463b" ]
      , HH.span
          [ HE.onClick \_ -> SavePreset i
          , HP.title "save to Amphora (shared store)"
          , style "cursor:pointer;color:#8a8676;font-size:11px;flex:0 0 auto" ]
          [ HH.text "⚱" ]
      , HH.span
          [ HE.onClick \_ -> DeletePreset i
          , HP.title "delete"
          , style "cursor:pointer;color:#b0a898;font-size:11px" ]
          [ HH.text "✕" ]
      ]

-- | The brain a banked snapshot came from, as a small engraved letter beside its
-- | glyph pair — so one badged list reads across all three brains and you can see
-- | at a glance which machine an entry is from (AC, 2026-08-07). Engraved letter
-- | rather than a coloured pill, per the Hainbach × Rams line in
-- | DESIGN-tri-snapshot.md.
-- |
-- | **Derived, never read off the stored text.** `printTri`'s on-disk tags are the
-- | FROZEN M/G/T from when the brains were MUTABLE/GRIDS/TIDAL, while the display
-- | letters are G/R/T — so stored `G` means RYTM and displayed `G` means GRIDS.
-- | Going through parseTri → brainOf → brainBadge is what keeps those two
-- | alphabets apart; taking the first character of `p.content` would be
-- | confidently wrong for two brains out of three.
-- |
-- | Unparseable content renders a dim `·`: a slot whose text no longer decodes
-- | is exactly the case where a made-up letter would mislead.
brainTag :: forall m. Preset -> H.ComponentHTML Action () m
brainTag p =
  let
    mBrain = brainOf <$> parseTri p.content
    label = maybe "·" brainBadge mBrain
    title = case mBrain of
      Just b -> brainLabel b
      Nothing -> "unrecognised snapshot content"
  in
    HH.span
      [ HP.title title
      , style $ engrave <> ";font-size:9px;letter-spacing:0.08em;width:11px;"
          <> "text-align:center;flex:0 0 auto;color:"
          <> (if isJust mBrain then "#6f6a5c" else "#bdb8a8") ]
      [ HH.text label ]

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





