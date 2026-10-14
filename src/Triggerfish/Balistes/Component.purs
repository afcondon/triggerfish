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

import Data.Array (concatMap, deleteAt, filter, find, head, insertAt, length, mapWithIndex, modifyAt, null, partition, range, (!!))
import Data.Ord (abs)
import Reef.Balistes.Kit as Kit
import Data.Tuple (Tuple(..), fst, snd)
import Data.Foldable (all, any, elem, foldl, for_, minimum)
import Effect.Random (randomInt)
import Data.Map as Map
import Data.Int (floor, round, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Triggerfish.Balistes.Follow as Follow
import Triggerfish.StageCopy as StageCopy
import Data.String (contains) as String
import Data.String.Common (trim) as String
import Data.String.Pattern (Pattern(..)) as String
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
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
import Triggerfish.Routing.Store as RStore
import Reef.Routing as RR
import Triggerfish.Balistes.Types
  ( KnobTarget(..), targetRange, applyKnob, Active(..), ClickMode(..)
  , DragKind(..), Paint(..), Spot, State, Action(..), activePattern, selectedPattern, patternAt, rhythmEntries, rigUrl, gridCfg
  , editVel
  , padId )
import Triggerfish.Browser as Browser
import Triggerfish.Balistes.TriSnapshot (TriSnapshot(..), parseTri, printTri, rhythmContent, rhythmOfContent)
import Triggerfish.Glyph as G
import Triggerfish.GlyphView (faIcons)
import Triggerfish.Library as Library
import Triggerfish.Preset (Preset, indexOfContent, newPreset, presetAlias, presetDoc, presetGlyph, rename, withWords, wordsOf)
import Triggerfish.Balistes.Widgets (armBtn, instColor)
import Triggerfish.Balistes.View.Fixed (cellStrip, fixedSvg, lengthControl)
import Triggerfish.Balistes.View.Grids (heatSvg, knobStack, padSvg)
import Triggerfish.Macro (Form(..), parseLane)
import Triggerfish.Balistes.Source as Source
import Triggerfish.Balistes.Store as Store
import Triggerfish.Balistes.Remote as Remote
import Triggerfish.Balistes.Lepidoptera (printPattern, parsePattern)
import Triggerfish.Balistes.Stage as Stage
import Triggerfish.SourceQuery (Query(..))
import Triggerfish.Transport (Mode(..), Sounding(..))
import Triggerfish.Transport.Store as TStore
import Reef.Balistes.Sim as Sim
import Triggerfish.Ui.Pointer as Pointer
import Triggerfish.Ui.Style (engrave, style)
import Web.Event.Event (EventType(..))
import Web.Event.Event as E
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Web.HTML.HTMLInputElement as HInput
import Web.HTML.HTMLTextAreaElement as HTextArea
import Halogen.Query.Event as HQE

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
        , sounding: Silent, playStep: 0, nextModelStep: 0, pending: []
        , binnacle: Nothing, outs: [], routing: RM.defaultTable, midiName: "…"
        , clockTempo: 120.0, clockLocked: false, clockBeat: 0.0, clockBar: 0, anchorCount: 0
        , nowMicros: 0.0, dragging: Nothing, paint: Nothing, dragSub: Nothing
        , presets: []
        , identity: Nothing, lastChip: Nothing
        , active: AGrids, editing: false, clickMode: Assemble, laneEditOpen: false, fixedSel: 0, lane: "", laneReadout: "", selected: [], cursor: Nothing, copied: [], mutes: 0, fill: false, fillHeld: false
        , scratchFixed: Nothing
        , stageRhythms: Nothing, shelf: Nothing, undoDelete: Nothing, follow: Nothing, followQueue: [] }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction, handleQuery = handleQuery, initialize = Just Initialize }
    }

-- | Answer the shell: the source (TIDAL tab) — the reflective header (X/Y,
-- | densities, groove, ratchets, tapped pads) over the editable lane/routing
-- | doc — or adopt the rack's shared free-run baseline.
handleQuery :: forall m a. MonadAff m => Query a -> H.HalogenM State Action () Output m (Maybe a)
handleQuery = case _ of
  -- The browser drawer (docs/kb/plans/the-deck.md, 2026-10-05): each beat by
  -- its name, with the machine it runs on (Grids or Rytm) as its tag.
  AskBrowser reply -> do
    s <- H.get
    let row i p = { slot: i, name: fromMaybe "untitled" p.name, icons: (presetGlyph p).icons, tag: maybe "" machineTag (parseTri p.content), current: s.identity == Just p.content, section: "", builtin: false, drag: Stage.rhythmDrag p, hint: Stage.rhythmHint p, actions: [ "delete" ] }
    pure (Just (reply { title: "Beats", modes: false, keep: "keep (c)", notice: maybe "" (\d -> "Deleted " <> fromMaybe "untitled" d.preset.name) s.undoDelete, rows: mapWithIndex row s.presets }))
  BrowserRecall i _ next -> do
    H.modify_ _ { undoDelete = Nothing }
    recallPreset i
    pure (Just next)
  BrowserKeep next -> do
    H.modify_ _ { undoDelete = Nothing }
    captureNow
    pure (Just next)
  -- the drawer's delete (it asks first), and its undo
  BrowserAction i "delete" next -> do
    s <- H.get
    for_ (s.presets !! i) \preset -> do
      H.modify_ (deletePresetAt i >>> _ { undoDelete = Just { at: i, preset } })
      persist
    pure (Just next)
  BrowserAction _ _ next -> pure (Just next)
  BrowserUndo next -> do
    s <- H.get
    for_ s.undoDelete \d -> do
      H.modify_ _ { presets = fromMaybe (s.presets <> [ d.preset ]) (insertAt d.at d.preset s.presets), undoDelete = Nothing }
      persist
    pure (Just next)
  -- the dice, in the shell's bar (AC, 2026-10-14): it shakes Grids up
  AskBar reply -> pure $ Just $ reply
    { tabs: [], marks: "", icons: [], rebusTip: "", pickers: []
    , help: "RYTM: click toggles a hit; drag along a row paints or erases; drag a hit up or down for its velocity. "
        <> "Shift-click or shift-drag selects; then ⌫ clears, ← → move, ⌘C ⌘V copy and paste at the last cell pressed, Esc deselects. "
        <> "FILL (or hold F) plays and edits the fill. M on a lane head mutes it, in both bands. Click a lane's name to hear it."
    , chips:
        [ { id: "dice", label: "", icons: [ { icon: "dice", color: "#3f3c33" } ], active: false, attention: false
          , tip: "shake Grids up: a new place on the map, densities, randomness and open (push and ratchets stay)" }
        , { id: "densities", label: "", icons: map (\i -> { icon: "dice-five", color: instColor i }) [ 0, 1, 2 ]
          , active: false, attention: false, tip: "new BD, SD and HH densities, the place on the map kept" }
        , { id: "dilla", label: "", icons: [ { icon: "dice-five", color: dillaColor } ], active: false, attention: false
          , tip: "a new Dilla push: kick near the grid, snare late, hats a little either side" } ]
    }
  BarAction act next -> do
    case act of
      "chip:dice" -> handleAction Dice
      "chip:densities" -> handleAction DiceDensities
      "chip:dilla" -> handleAction DiceDilla
      _ -> pure unit
    pure (Just next)
  BrowserDrop _ next -> pure (Just next)
  BrowserRename i name next -> do
    H.modify_ \s -> s { presets = rename i (String.trim name) s.presets, undoDelete = Nothing }
    persist
    pure (Just next)
  -- No marks here: asked for its text, the shell falls back to AskSource.
  AskMarkText _ -> pure Nothing
  AddMarkSnapshot _ _ _ next -> pure (Just next)
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
    -- Stopping, or leaving the rig, stops the rig's voice: the page's beat,
    -- or the line it is following.
    when ((st.sounding == Rig || isJust st.follow) && s /= Rig) $
      for_ st.binnacle \bin -> toRigVoice bin "balistes-stop"
    -- Leaving Silent: the Grids model did not advance while stopped, so put it on
    -- the step the clock is at now, as if it had. Its step is the absolute step
    -- mod 32; only the perturbation draws (one per wrap) differ from a model that
    -- kept running, and a rig handoff sends the whole model, so both sides agree.
    when (st.sounding == Silent && s /= Silent) do
      now <- currentStep
      H.modify_ \t -> t { bal = t.bal { step = now `mod` 32 }, nextModelStep = now }
    H.modify_ _ { sounding = s }
    -- Playing on the rig pushes the page's beat, unless a line is playing it
    -- (resuming a line is the shell's: Triggerfish.Standalone's Play).
    when (s == Rig && isNothing st.follow) (handleAction PushBalistes)
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
        Nothing -> do
          -- named for the machine it runs on and the time, until renamed
          at <- liftEffect Browser.stamp
          let named = maybe "Beat" machineTag (parseTri text) <> " · " <> at
          H.modify_ \st -> st
            { presets = st.presets <> [ newPreset text (Just named) ]
            , identity = Just text
            }
      persist

-- | The machine a beat runs on, as the browser tags it.
machineTag :: TriSnapshot -> String
machineTag = case _ of
  TSGrids _ -> "Grids"
  TSFixed _ -> "Rytm"

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
    win <- liftEffect window
    _ <- H.subscribe $ HQE.eventListener KET.keydown (Window.toEventTarget win) (Just <<< RytmKey)
    _ <- H.subscribe $ HQE.eventListener KET.keyup (Window.toEventTarget win) (Just <<< RytmKeyUp)
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
    -- The bank is what Amphora keeps (`Triggerfish.Library`): this browser's
    -- copy at once, Amphora's when it answers or changes elsewhere. The first
    -- time, it starts from what this browser kept before and the old
    -- `balistes-grid` rhythms.
    legacy <- liftEffect (maybe [] _.presets <$> Store.load)
    { emitter: libE, listener: libL } <- liftEffect HS.create
    _ <- H.subscribe (LibraryChanged <$ libE)
    opened <- liftEffect $ Library.open (presetDoc "balistes" (map presetOfRhythm <$> Remote.fetchLibrary)) legacy (HS.notify libL unit)
    H.modify_ _ { presets = withWords opened.items, shelf = Just opened.shelf }
    H.modify_ _ { binnacle = Just bin }
    -- The rig's stage keeps a copy of the named rhythms, which a
    -- `balistes $ "<a b>"` line plays by name: subscribe on every connect,
    -- since a rig restart empties it.
    { emitter: stageE, listener: stageL } <- liftEffect HS.create
    _ <- H.subscribe stageE
    liftEffect $ Binnacle.onAppMessage bin (HS.notify stageL <<< StageFrameIn)
    liftEffect $ Binnacle.onOpen bin (HS.notify stageL StageOpen)

  Step tick -> do
    -- what the rig's line plays from this step on, shown before the step runs
    H.modify_ \s -> case partition (\f -> f.step <= tick.index) s.followQueue of
      { yes: [], no: _ } -> s
      { yes, no } -> foldl (flip (showNow tick.index)) (s { followQueue = no }) yes
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
    -- Following a line the rig plays: the model runs to show it, but the rig
    -- is what sounds, and a rest shows nothing moving.
    let following = isJust st.follow
        audible = st.sounding == Local && not following
        offset = maybe 0 _.from st.follow
    unless ((st.sounding == Silent && not following) || maybe false _.rest st.follow) case st.active of
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
            -- pattern onto the wire-flat reef pattern (toWire).
            for_ (filter (\e -> Kit.audible st.mutes e.note) (RF.renderFixedFill st.fill (P.toWire pat) (tick.index - offset))) \e ->
              emitHit st.outs (RO.soloDrumRouting st.outs st.routing kitNotes) stepMs
                (max 0.0 (tick.delayMs + toNumber e.pushMs))
                e.note e.durMs e.velocity e.ratchet
          H.modify_ _ { playStep = (tick.index - offset) `mod` pat.steps }
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
          for_ (filter (\e -> Kit.audible st.mutes e.note) (Sim.renderStep bal0 playedStep r.fired)) \e ->
            emitHit st.outs (RO.soloDrumRouting st.outs st.routing kitNotes) stepMs
              (max 0.0 (tick.delayMs + toNumber e.pushMs))
              e.note e.durMs e.velocity e.ratchet
        H.modify_ \s -> s
          { bal = r.bal, playStep = playedStep
          -- r.bal is the state that plays NEXT, at absolute step tick.index + 1;
          -- PushBalistes stamps the handoff with this for phase alignment.
          , nextModelStep = tick.index + 1
          , pending = keepInputs }
  Frame -> do
    st <- H.get
    case st.binnacle of
      Just bin -> do
        now <- liftEffect $ Clock.unixMicrosNow (Binnacle.clock bin)
        r <- liftEffect $ Clock.read (Binnacle.clock bin)
        -- Write only when something drawn or read has moved. Every write
        -- re-renders the whole panel, and at 30 a second, stopped, that was most
        -- of this page's idle CPU. What is drawn: the rounded tempo, the lock
        -- and the bar; what is read:
        -- the tempo (step length). The step is read from the clock where it is
        -- needed (`currentStep`).
        let moved = r.tempo /= st.clockTempo || r.locked /= st.clockLocked
              || r.bar /= st.clockBar || r.anchorCount /= st.anchorCount
        when moved $ H.modify_ \s -> s
          { nowMicros = now
          , clockTempo = r.tempo
          , clockLocked = r.locked
          , clockBeat = r.beat
          , clockBar = r.bar
          , anchorCount = r.anchorCount
          }
      Nothing -> pure unit
    -- Report the identity chip up to the shell's status board, but only when it
    -- actually changed (this fires ~30×/s) — capture/recall/divergence all land here.
    s2 <- H.get
    let cv = chipViewOf s2
    when (cv /= s2.lastChip) do
      H.modify_ _ { lastChip = cv }
      H.raise (IdentityChanged cv)

  StageOpen -> do
    H.modify_ _ { stageRhythms = Nothing }
    st <- H.get
    for_ st.binnacle \bin -> liftEffect (Transport.send (Binnacle.socket bin) StageCopy.subscribeLine)
  -- the stage's whole table, on subscribing: it gets any of our rhythms it
  -- lacks, and loses any we have deleted
  -- Another browser changed the bank (or this one's change was kept on top
  -- of theirs): show it, and the stage follows.
  LibraryChanged -> do
    st <- H.get
    for_ st.shelf \shelf -> do
      items <- liftEffect (Library.current shelf)
      H.modify_ _ { presets = withWords items, undoDelete = Nothing }
    publishRhythms
  StageFrameIn msg -> do
    -- The stage's beats are everyone's: only those this bank names count as
    -- ours, so connecting adds and updates ours and never deletes another
    -- browser's. A delete is only ever a beat this page had and lost.
    for_ (StageCopy.table Stage.stagePrefix msg) \held -> do
      H.modify_ \s -> s { stageRhythms = Just (StageCopy.ours held (Stage.bankRhythms s.bal s.presets)) }
      publishRhythms
    for_ (Follow.parseNow msg) \now ->
      H.modify_ \s -> s { followQueue = s.followQueue <> [ now ] }
    when (Follow.endsLine msg) $
      H.modify_ _ { follow = Nothing, followQueue = [] }
  MidiReady outs nm -> do
    H.modify_ _ { outs = outs, midiName = nm }
    -- a saved IAC bus this browser no longer has: moved to the one it has
    healed <- liftEffect (RStore.heal (RO.portNames outs))
    for_ healed \t -> H.modify_ _ { routing = t }
    pushRouting

  -- Reset shifts the model step (jump to 0), so it's DEFERRED-ON-BOTH: enqueued +
  -- broadcast tagged for a near-future step, applied by the drain here and by the
  -- voice on the rig at the SAME absolute step — no pattern offset. (When stopped it
  -- queues and applies on the next play, the drain's `<=` self-heal.)
  -- Shake Grids up (AC, 2026-10-13): a new place on the map, new densities in
  -- ranges that stay musical, a touch of randomness, a new open-hat boundary,
  -- and a fresh seed. The groove (Dilla push) and ratchets are left alone: a
  -- push rolled at random is the one thing that easily sounds wrong. Applied
  -- at once, as a pad drag is, and the settled values sent to the rig.
  Dice -> do
    x <- liftEffect (randomInt 0 255)
    y <- liftEffect (randomInt 0 255)
    { bd, sd, hh } <- liftEffect rollDensities
    rand <- liftEffect (randomInt 0 60)
    open <- liftEffect (randomInt 20 140)
    let setters =
          [ RBI.BSetX x, RBI.BSetY y, RBI.BSetDensity 0 bd, RBI.BSetDensity 1 sd, RBI.BSetDensity 2 hh
          , RBI.BSetRandomness rand, RBI.BSetOpen open ]
    H.modify_ \s -> s { bal = M.reseed (foldl (flip RBI.applyBInput) s.bal setters) }
    for_ (setters <> [ RBI.BReseed ]) broadcastBInput
  -- The three coloured dice (AC, 2026-10-14): new BD/SD/HH densities, as
  -- Dice rolls them, leaving the place on the map where it is.
  DiceDensities -> do
    { bd, sd, hh } <- liftEffect rollDensities
    settle [ RBI.BSetDensity 0 bd, RBI.BSetDensity 1 sd, RBI.BSetDensity 2 hh ]
  -- The Dilla dice: a push rolled within the feel's own shape, the kick near
  -- the grid, the snare late, the hats (closed and open together) a little
  -- early or late. Kept small: a wide push is the easy way to sound wrong.
  DiceDilla -> do
    bd <- liftEffect (randomInt (-5) 5)
    sd <- liftEffect (randomInt 4 22)
    hh <- liftEffect (randomInt (-14) 6)
    settle [ RBI.BSetPush 0 bd, RBI.BSetPush 1 sd, RBI.BSetPush 2 hh, RBI.BSetPush 3 hh ]

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
              -- up or down on a hit shapes its velocity, a unit a pixel, and
              -- the whole selection's with it if the hit is in it; the strip
              -- shows it as it moves
              DHit lane step -> case st.paint of
                Just Undecided | abs dist >= 4 ->
                  let origin = { lane, step }
                      group = if elem origin st.selected then st.selected else [ origin ]
                      vels = map (\spot -> { spot, vel: maybe editVel (\p -> (P.cellAt p spot.lane spot.step).vel) (selectedPattern st) }) group
                  in H.modify_ _ { paint = Just (Shaping vels), selected = group }
                Just (Shaping vels) ->
                  H.modify_ \s -> foldl (\acc v -> modRhythmAt acc.fixedSel (P.modifyCell v.spot.lane v.spot.step _ { vel = clamp 1 127 (v.vel + dist) }) acc) s vels
                _ -> pure unit
      Nothing -> pure unit
  DragEnd -> do
    st <- H.get
    for_ st.dragSub H.unsubscribe
    -- Live knob sync: broadcast the SETTLED gesture to the rig as a tick-tagged
    -- BInput. reef_balistes_voice applies it (via the shared reef applyBInput) on the
    -- tagged model step, so the rig follows the edit. Absolute idempotent setters, so
    -- replaying the settled value lands the rig exactly where the drag settled.
    for_ (st.dragging >>= \d -> dragToBInput d.kind st.bal) broadcastBInput
    H.modify_ _ { dragging = Nothing, dragSub = Nothing, paint = Nothing }
    case st.dragging, st.paint of
      -- a press and release on a hit, without moving, removes it
      Just { kind: DHit lane step }, Just Undecided -> do
        H.modify_ \s -> (modRhythmAt s.fixedSel (P.modifyCell lane step (P.removeIn s.fill)) s)
          { selected = filter (_ /= { lane, step }) s.selected }
        persistLib
      Just { kind: DHit _ _ }, Just (Selecting _) -> pure unit
      Just { kind: DHit _ _ }, Just _ -> persistLib
      _, _ -> pure unit

  BankNow -> captureNow
  -- Bank the named brain, whether or not it is the sounding one — the glyph you
  -- click lives in that brain's own band, so it must bank what THAT band shows.
  BankBrain target -> do
    s <- H.get
    case printTri <$> triOf s target of
      Nothing -> pure unit
      Just text -> do
        when (isNothing (indexOfContent text s.presets)) $
          H.modify_ \st -> st { presets = st.presets <> [ newPreset text Nothing ] }
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

  RecallPreset i -> recallPreset i
  StarPreset i ->
    H.modify_ \s -> s { presets = fromMaybe s.presets (modifyAt i (\p -> p { starred = not p.starred }) s.presets) }
  DeletePreset i -> do
    H.modify_ (deletePresetAt i)
    persist
  -- FLAT goes to the rig as four pushes, as a knob's settled value does
  FlatGroove -> H.gets _.bal >>= \b -> settle (pushes (M.flatPush b).push)
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
      { active = a, scratchFixed = Nothing
      , fixedSel = case a of
          AFixed i -> i
          _ -> s.fixedSel }
    st <- H.get
    when (st.sounding == Rig) case a of
      AFixed _ -> repushFixed
      AGrids -> pushHandoff st
  ToggleEdit -> H.modify_ \s -> s { editing = not s.editing }
  -- A press on a cell (AC, 2026-10-14): on an empty cell it adds a hit and
  -- paints along the row; on a hit, what it does waits for the pointer (see
  -- `Paint`). Shift-click adds a hit to the selection or takes it out, and
  -- shift-drag adds every hit in the rectangle; painting deselects. Every press moves the cursor,
  -- where a paste lands. On a recalled snapshot the first edit makes an
  -- editable copy (`thawed`) rather than doing nothing. The rhythm is saved
  -- and sent to the rig once, when the press ends.
  CellDown lane step shift -> do
    s0 <- H.get
    sid <- setupDrag
    let s = (thawed s0) { cursor = Just { lane, step } }
        origin = { lane, step }
        lit = litAt s origin
        press = { kind: DHit lane step, startY: 0, startVal: 0 }
    if shift then
      let base = if not lit then s.selected
                 else if elem origin s.selected then filter (_ /= origin) s.selected
                 else s.selected <> [ origin ]
      in H.put s { selected = base, dragging = Just press, dragSub = Just sid, paint = Just (Selecting base) }
    else if lit then
      H.put s { dragging = Just press, dragSub = Just sid, paint = Just Undecided }
    else
      H.put (modRhythmAt s.fixedSel (P.modifyCell lane step (P.addIn s.fill editVel)) s)
        { selected = [], dragging = Just press, dragSub = Just sid, paint = Just Painting }
  -- Painting and erasing go along the origin's row only: a vertical drag on a
  -- hit crosses into the rows above and below, and must not erase them. A
  -- selection rectangle takes any cell.
  PaintOver lane step -> do
    st <- H.get
    case st.dragging, st.paint of
      Just { kind: DHit lane0 step0 }, Just (Selecting base) ->
        let inRect sp = between (min lane0 lane) (max lane0 lane) sp.lane && between (min step0 step) (max step0 step) sp.step
            steps = maybe 0 _.steps (selectedPattern st)
            rect = filter (litAt st) (filter inRect (do
                     l <- range 0 (P.kitSize - 1)
                     t <- range 0 (steps - 1)
                     pure { lane: l, step: t }))
        in H.modify_ _ { selected = base <> filter (\sp -> not (elem sp base)) rect }
      Just { kind: DHit lane0 step0 }, Just mode | lane == lane0 && step /= step0 ->
        let set f = modRhythmAt st.fixedSel (P.modifyCell lane step f)
            add = P.addIn st.fill editVel
            remove = P.removeIn st.fill
        in case mode of
          Painting -> H.modify_ \s -> if litAt s { lane, step } then s else set add s
          Undecided -> H.modify_ \s -> (set remove (modRhythmAt s.fixedSel (P.modifyCell lane0 step0 remove) s))
            { paint = Just Erasing, selected = [] }
          Erasing -> H.modify_ \s -> if litAt s { lane, step } then set remove s else s
          _ -> pure unit
      _, _ -> pure unit
  SetCellVel d -> do
    H.modify_ (modSelectedCell \c -> c { vel = clamp 1 127 (c.vel + d) })
    persistLib
  SetCellProb d -> do
    H.modify_ (modSelectedCell \c -> c { prob = clamp 0 100 (c.prob + d) })
    persistLib
  SetCellRatchet d -> do
    H.modify_ (modSelectedCell \c -> c { ratchet = clamp 1 8 (c.ratchet + d) })
    persistLib
  -- the whole group takes the condition after the first hit's
  CycleCellCond -> do
    st <- H.get
    for_ (head st.selected >>= \sp -> map (\p -> P.cycleCond (P.cellAt p sp.lane sp.step).cond) (selectedPattern st)) \cond -> do
      H.modify_ (modSelectedCell _ { cond = cond })
      persistLib
  RytmKeyUp e -> for_ (KE.fromEvent e) \ke -> do
    st <- H.get
    when ((KE.key ke == "f" || KE.key ke == "F") && st.fillHeld) do
      handleAction (SetFill false)
  -- A selection is of hits as the grid showed them, so it goes with the view.
  SetFill on -> do
    st <- H.get
    when (on /= st.fill) do
      H.modify_ _ { fill = on, selected = [], fillHeld = false }
      pushFill
  ToggleMute lanes -> do
    H.modify_ \s ->
      let on = not (all (Kit.muted s.mutes) lanes)
      in s { mutes = foldl (\m l -> if Kit.muted m l == on then m else Kit.toggleMute l m) s.mutes lanes }
    pushMutes
  ClearSelected -> do
    H.modify_ \s -> (modSelectedCell (P.removeIn s.fill) s) { selected = [] }
    persistLib
  -- The selection's keys (AC, 2026-10-14), unless a field has the focus:
  -- ⌫ clears the selected hits, ← and → move them a step (round the end),
  -- ⌘C copies them, ⌘V pastes the copy at the cursor, Esc deselects.
  RytmKey e -> for_ (KE.fromEvent e) \ke -> unless (targetIsField e) do
    st <- H.get
    let has = not (null st.selected) && isNothing st.scratchFixed
        command = KE.metaKey ke || KE.ctrlKey ke
        take' act = do
          liftEffect (E.preventDefault e)
          act
    case KE.key ke of
      -- F held is the fill, as long as it is held (the FILL button latches it)
      k | (k == "f" || k == "F") && not command && not (KE.altKey ke) && not (KE.repeat ke) && not st.fill -> take' do
        handleAction (SetFill true)
        H.modify_ _ { fillHeld = true }
      k | has && not command && (k == "Backspace" || k == "Delete") -> take' (handleAction ClearSelected)
      "ArrowLeft" | has && not command -> take' (moveSelection (-1))
      "ArrowRight" | has && not command -> take' (moveSelection 1)
      "Escape" | has -> take' (H.modify_ _ { selected = [] })
      k | command && (k == "c" || k == "C") && has -> take' do
        for_ (selectedPattern st) \p ->
          let first = fromMaybe 0 (minimum (map _.step st.selected))
          in H.modify_ _ { copied = map (\sp -> { lane: sp.lane, step: sp.step - first, cell: P.cellAt p sp.lane sp.step }) st.selected }
      k | command && (k == "v" || k == "V") && not (null st.copied) -> take' do
        H.modify_ \s0 ->
          let s = thawed s0
              at = maybe 0 _.step s.cursor
              steps = maybe 0 _.steps (selectedPattern s)
              landed = filter (\c -> c.step < steps) (map (\c -> c { step = c.step + at }) s.copied)
          in (foldl (\acc c -> modRhythmAt acc.fixedSel (P.modifyCell c.lane c.step (const c.cell)) acc) s landed)
               { selected = map (\c -> { lane: c.lane, step: c.step }) landed }
        persistLib
      _ -> pure unit
  -- The rhythm's length (AC, 2026-10-14): a pick repeats it into the new
  -- bars, a nudge adds or drops one silent step. A selection past the end
  -- goes with the steps.
  SetLength n -> resizeRhythm (P.extendTo n)
  NudgeLength d -> do
    st <- H.get
    for_ (selectedPattern st) \p -> resizeRhythm (P.resizeTo (p.steps + d))
  -- Grids pushed to RYTM: what it plays, baked into a new rhythm and selected
  -- there, to edit, lengthen and fill; what is playing does not change.
  PushToRytm -> do
    H.modify_ \s ->
      let n = length s.presets
          p = M.bake ("grids " <> show s.bal.x <> "·" <> show s.bal.y) s.bal
      in s { presets = s.presets <> [ presetOfRhythm p ], fixedSel = n, scratchFixed = Nothing, selected = [] }
    persist
  -- a fresh empty rhythm, selected and opened in EDIT so all 16 lanes show.
  NewPattern -> do
    H.modify_ \s ->
      let n = length s.presets
          p = P.emptyPattern ("pattern " <> show (length (rhythmEntries s) + 1)) 32
      in s { presets = s.presets <> [ presetOfRhythm p ]
           , active = AFixed n, fixedSel = n, editing = true, selected = [] }
    persistLib
  -- A lane's head was clicked: one hit of it, where the routing sends it. In
  -- Atlantis the rig plays it (`balistes-hit`), through the table it holds, so
  -- the FH-2's gates and SuperDirt's samples sound too; in Solo this page does,
  -- as it plays its own steps.
  HearLane lane -> do
    st <- H.get
    mode <- liftEffect TStore.load
    case mode, st.binnacle of
      Just Atlantis, Just bin ->
        liftEffect $ Transport.send (Binnacle.socket bin) ("balistes-hit " <> show lane <> " 110")
      _, _ ->
        liftEffect $ emitHit st.outs (RO.soloDrumRouting st.outs st.routing kitNotes) 125.0 0.0
          (P.laneNote lane) 120.0 110 1
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
          toRigVoice bin ("balistes-fixed " <> encodeFixed (P.toWire pat))
      -- Grids: the phase-aligned BalSim handoff.
      AGrids -> pushHandoff st
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
  , open: b.open, push: b.push, ratchet: b.ratchet }

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
-- | Apply settled values at once, as a pad or knob drag does, and send them to
-- | the rig.
settle :: forall o m. MonadAff m => Array RBI.BInput -> H.HalogenM State Action () o m Unit
settle setters = do
  H.modify_ \s -> s { bal = foldl (flip RBI.applyBInput) s.bal setters }
  for_ setters broadcastBInput

-- | Dice's densities (AC, 2026-10-14): anywhere on the knob 0..255 one roll in
-- | three, otherwise in the range that reliably grooves (BD busy, SD and HH freer).
rollDensities :: Effect { bd :: Int, sd :: Int, hh :: Int }
rollDensities = ado
  bd <- roll 150 235
  sd <- roll 110 210
  hh <- roll 120 230
  in { bd, sd, hh }
  where
  roll lo hi = do
    wide <- randomInt 0 2
    if wide == 0 then randomInt 0 255 else randomInt lo hi

-- | A push array (BD, SD, HH, OH) as the four inputs that set it.
pushes :: Array Int -> Array RBI.BInput
pushes = mapWithIndex RBI.BSetPush

-- | The Dilla dice's colour: a plum, apart from the three lanes' own.
dillaColor :: String
dillaColor = "#7a3f6a"

broadcastBInput :: forall o m. MonadAff m => RBI.BInput -> H.HalogenM State Action () o m Unit
broadcastBInput input = do
  st <- H.get
  now <- currentStep
  -- Rig-send only in ATLANTIS (onRig = not audible); SOLO is silent to the rig.
  -- Not while following a line: the rig is playing the line, not this.
  when (st.sounding == Rig && isNothing st.follow) $ for_ st.binnacle \bin ->
    liftEffect $ Transport.send (Binnacle.socket bin) (balInputMsg now input)

-- | Say something to the rig's Balistes voice. Whatever the page says to it
-- | (a pushed beat, a stop) takes the voice off any `balistes $` line, so the
-- | page stops following that line.
toRigVoice :: forall o m. MonadAff m => Binnacle.Binnacle -> String -> H.HalogenM State Action () o m Unit
toRigVoice bin line = do
  H.modify_ _ { follow = Nothing, followQueue = [] }
  liftEffect $ Transport.send (Binnacle.socket bin) line

-- | Push the phase-aligned handoff: the whole BalSim stamped with nextModelStep, so
-- | the rig holds it until that step. Used by the Push button and by snapshot recall
-- | (a whole-kit jump, which the handoff is the natural fit for).
pushHandoff :: forall o m. MonadAff m => State -> H.HalogenM State Action () o m Unit
pushHandoff st =
  -- ATLANTIS only. Called by SyncToRig (the entering-ATLANTIS handoff, where audible
  -- is already false) and by snapshot recall (a no-op in SOLO — no rig leak).
  when (st.sounding == Rig && isNothing st.follow) $ for_ st.binnacle \bin ->
    toRigVoice bin
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
    when (st2.sounding == Rig && isNothing st2.follow) $ for_ st2.binnacle \bin ->
      toRigVoice bin ("balistes-fixed " <> encodeFixed (P.toWire pat))
-- | Show what the rig's line plays from `now.step`: the saved beat its word
-- | names, as the rig plays it (a rhythm from that step, a Grids beat from the
-- | engine state the rig sent), or a rest for `~` or a word with no beat.
-- | The chip and the drawer's bold row follow `identity`. Heard late (the
-- | line's first event, announced as the rig starts it), a Grids engine is
-- | run on to step `at`, where the rig's is.
showNow :: Int -> Follow.Now -> State -> State
showNow at now s = case find (\p -> elem now.word (wordsOf p)) s.presets of
  Just p | Just named <- p.name -> case parseTri p.content, now.grids of
    Just (TSGrids _), Just engine ->
      s { active = AGrids, scratchFixed = Nothing, bal = catchUp (at - now.step) engine, identity = Just p.content, follow = Just { from: now.step, rest: false } }
    Just (TSFixed _), _ | Just pat <- rhythmOfContent named p.content ->
      let idx = fromMaybe 0 (map fst (find (\(Tuple _ q) -> q.name == pat.name) (rhythmEntries s)))
      in s { active = AFixed idx, scratchFixed = Just pat, identity = Just p.content, follow = Just { from: now.step, rest: false } }
    _, _ -> rest
  _ -> rest
  where
  rest = s { identity = Nothing, follow = Just { from: now.step, rest: true } }
  catchUp n b = if n <= 0 then b else catchUp (n - 1) (M.tick b).bal

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

-- | Map a settled drag to the BInput that reproduces it on the rig: the four
-- | knob kinds and ratchets.
dragToBInput :: DragKind -> M.Balistes -> Maybe RBI.BInput
dragToBInput kind b = case kind of
  DKnob (KDens i) -> Just (RBI.BSetDensity i (M.densityOf i b))
  DKnob KRand -> Just (RBI.BSetRandomness b.randomness)
  DKnob (KPush i) -> Just (RBI.BSetPush i (M.pushOf i b))
  DKnob KOpen -> Just (RBI.BSetOpen (M.openOf b))
  DCell inst step -> Just (RBI.BSetRatchet inst step (M.ratchetAt b inst step))
  DHit _ _ -> Nothing

-- | Delete preset `i` from the bank.
deletePresetAt :: Int -> State -> State
deletePresetAt i s = s { presets = fromMaybe s.presets (deleteAt i s.presets) }

-- | Project component `State` onto the persisted artefact (library + preset bank).

-- | Save the whole artefact to localStorage (library + bank + sequence). Called
-- | after any bank / sequence edit; `persistLib` layers the rig re-push on top.
persist :: forall o m. MonadAff m => H.HalogenM State Action () o m Unit
persist = do
  s <- H.modify \st -> st { presets = withWords st.presets }
  for_ s.shelf \shelf -> do
    kept <- liftEffect (Library.put shelf s.presets)
    H.modify_ _ { presets = kept }
  publishRhythms

-- | Bring the stage's copy of the named rhythms up to date with the bank: a
-- | write per new or changed one, a delete per one this page had that is gone. A bank with no
-- | rhythms is a browser that has saved none here, not one that deleted them
-- | all, so it changes nothing (as Vetula's progressions).
publishRhythms :: forall o m. MonadAff m => H.HalogenM State Action () o m Unit
publishRhythms = do
  st <- H.get
  for_ st.stageRhythms \seen -> for_ st.binnacle \bin -> do
    let now = Stage.bankRhythms st.bal st.presets
    unless (Map.isEmpty now) do
      liftEffect $ for_ (StageCopy.lines Stage.stagePrefix seen now) (Transport.send (Binnacle.socket bin))
      H.modify_ _ { stageRhythms = Just now }

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
  when (st.sounding == Rig && isNothing st.follow) case st.active of
    AFixed _ -> for_ (activePattern st) \pat ->
      for_ st.binnacle \bin ->
        toRigVoice bin ("balistes-fixed " <> encodeFixed (P.toWire pat))
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
  pushMutes
  pushFill

-- | The fill, to the rig.
pushFill :: forall o m. MonadAff m => H.HalogenM State Action () o m Unit
pushFill = do
  st <- H.get
  when (st.sounding == Rig) $ for_ st.binnacle \bin ->
    liftEffect $ Transport.send (Binnacle.socket bin) ("balistes-fill " <> if st.fill then "1" else "0")

-- | The muted lanes, to the rig, which drops their hits in every mode.
pushMutes :: forall o m. MonadAff m => H.HalogenM State Action () o m Unit
pushMutes = do
  st <- H.get
  when (st.sounding == Rig) $ for_ st.binnacle \bin ->
    liftEffect $ Transport.send (Binnacle.socket bin) ("balistes-mute " <> show st.mutes)

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
presetOfRhythm p = newPreset (rhythmContent p) (if p.name == "" then Nothing else Just p.name)

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
        , active = AFixed n, fixedSel = n, scratchFixed = Nothing, selected = [] }

resizeRhythm :: forall o m. MonadAff m => (P.FixedPattern -> P.FixedPattern) -> H.HalogenM State Action () o m Unit
resizeRhythm f = do
  H.modify_ \s0 ->
    let s = modRhythmAt s0.fixedSel f (thawed s0)
        steps = maybe 0 _.steps (selectedPattern s)
    in s { selected = filter (\c -> c.step < steps) s.selected }
  persistLib

modSelectedCell :: (P.Cell -> P.Cell) -> State -> State
modSelectedCell f s
  | isNothing s.scratchFixed = foldl (\acc sp -> modRhythmAt acc.fixedSel (P.modifyCell sp.lane sp.step f) acc) s s.selected
  | otherwise = s

-- | A key typed into a field (the lane editor) is the field's.
targetIsField :: E.Event -> Boolean
targetIsField e = case E.target e of
  Just t -> isJust (HInput.fromEventTarget t) || isJust (HTextArea.fromEventTarget t)
  Nothing -> false

-- | Does a hit sound at this cell of the selected rhythm, as the grid shows
-- | it (with the fill, or without)?
litAt :: State -> Spot -> Boolean
litAt s sp = maybe false (\p -> P.soundsIn s.fill (P.cellAt p sp.lane sp.step)) (selectedPattern s)

-- | Move the selected hits `d` steps, round the end of the rhythm; what they
-- | land on is written over.
moveSelection :: forall o m. MonadAff m => Int -> H.HalogenM State Action () o m Unit
moveSelection d = do
  H.modify_ \s -> case selectedPattern s of
    Nothing -> s
    Just p ->
      let moved sp = sp { step = (sp.step + d + p.steps) `mod` p.steps }
          held = map (\sp -> { to: moved sp, cell: P.cellAt p sp.lane sp.step }) s.selected
          cleared = foldl (\acc sp -> modRhythmAt acc.fixedSel (P.modifyCell sp.lane sp.step (const P.emptyCell)) acc) s s.selected
      in (foldl (\acc h -> modRhythmAt acc.fixedSel (P.modifyCell h.to.lane h.to.step (const h.cell)) acc) cleared held)
           { selected = map _.to held }
  persistLib

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
    [ style $ "position:fixed;top:var(--tf-bar);left:var(--tf-left,0px);right:var(--tf-right,0px);bottom:0;display:flex;flex-direction:column;"
        <> "user-select:none;-webkit-user-select:none;background:#b7b1a0;font-family:Georgia,serif" ]
    [ navBar s
    -- The bands, full width. Patterns are combined in Limulus (AC,
    -- 2026-10-04), its drawer on the right of the page (the shared bar).
    -- ASSEMBLE, the old right column, went when named beats came (2026-10-05).
    , HH.div
        [ style "flex:1 1 auto;min-height:0;display:flex;align-items:stretch" ]
        [ HH.div
            [ style "flex:1 1 auto;min-width:0;overflow-y:auto;display:flex;flex-direction:column;gap:10px;padding:10px 14px 14px" ]
            [ mutableBand s, gridsBand s ]
        ]
    , laneEditModal s
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
    ( [ HH.div [ style "flex:1 1 auto;min-width:8px" ] []
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
  band s AGrids "GRIDS" [ xyReadout, armBtn "PUSH TO RYTM" false PushToRytm ]
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
        Just pat -> HH.div [ style "width:100%" ] [ fixedSvg s pat ] )
  where
  extras =
    -- Its name is given and changed in the Library drawer (double-click),
    -- the one place a beat is named, so the drag carries what the row says.
    [ HH.span [ style "font-family:Georgia,serif;font-size:12px;color:#1c1a12;min-width:80px" ]
        [ HH.text (maybe "" _.name (selectedPattern s)) ]
    , armBtn (if s.editing then "● EDITING" else "EDIT") s.editing ToggleEdit
    , armBtn "+ NEW" false NewPattern
    , armBtn "FILL" s.fill (SetFill (not s.fill))
    , maybe (HH.text "") lengthControl (selectedPattern s)
    , cellStrip s
    ]

-- ---------------------------------------------------------------------------
-- The lane editor
-- ---------------------------------------------------------------------------

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
-- | Backdrop and panel are SIBLINGS: nesting would
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

modalSection :: forall m. String -> String -> H.ComponentHTML Action () m -> H.ComponentHTML Action () m
modalSection label blurb body =
  HH.div [ style "margin-bottom:18px" ]
    [ HH.div [ style $ engrave <> ";font-size:9px;color:#8a8676;border-bottom:1px solid #00000014;padding-bottom:4px;margin-bottom:9px" ]
        [ HH.text label ]
    , HH.div [ style $ engrave <> ";font-size:8px;opacity:0.6;margin-bottom:9px" ] [ HH.text blurb ]
    , body
    ]

-- | Is this bank entry a RYTM rhythm (clicking it selects it for editing)
-- | rather than a captured moment (clicking it restores that state)?
isRhythm :: Preset -> Boolean
isRhythm p = isJust (rhythmOfContent (fromMaybe "" p.name) p.content)

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





