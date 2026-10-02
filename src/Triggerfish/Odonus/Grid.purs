-- | The Odonus grid + four-head bank, Hainbach dress. The 16 cells are shown
-- | as parameter-major small multiples — a NOTE field of value knobs, then
-- | GATE / SKIP / GLIDE / LENGTH fields — over a bank of four playheads; each
-- | head carries a René-style access **pattern** shown as a small-multiple
-- | thumbnail beside its direction / speed / interval knobs, and a 16-switch
-- | head-activation matrix cuts between playhead combinations. Aesthetic:
-- | Swiss rigor × vintage-lab materiality (see BRIEF.md).
module Triggerfish.Odonus.Grid (component, Output(..)) where

import Prelude

import Data.Array (any, concatMap, deleteAt, elem, filter, find, findIndex, head, length, mapMaybe, mapWithIndex, modifyAt, null, partition, range, replicate, snoc, sort, updateAt, (!!))
import Data.Foldable (foldl, for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Int (ceil, floor, round, toNumber)
import Data.Ord (abs)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.String.Common (joinWith)
import Effect (Effect)
import Effect.Aff (attempt)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Data.Either (Either(..))
import Data.String.CodeUnits (take)
import Effect.Timer (setInterval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Triggerfish.Odonus.Model as M
import Triggerfish.Poly as Poly
import Reef.Voices as RV
import Tidal.Harmony as Harmony
import Triggerfish.Cue as Cue
import Tidal.Scales as Scales
import Triggerfish.Odonus.Marbles as Marbles
import Triggerfish.Odonus.Gen as Gen
import Triggerfish.Odonus.Forms as Forms
import Triggerfish.Ui.Pointer as Pointer
import Binnacle as Binnacle
import Binnacle.Clock as Clock
import Binnacle.Midi as Midi
import Binnacle.Scheduler as Scheduler
import Binnacle.Time as Time
import Binnacle.Transport as Transport
import Reef.Engine as RE
import Reef.Input as RI
import Reef.PitchSet (PitchSet(..))
import Reef.Rample as Rample
import Effect.Console as Console
import Reef.Protocol (decodeTagged, encodeSim, encodeTagged)
import Data.String (Pattern(..), stripPrefix) as Str
import Web.Event.Event (EventType(..), preventDefault)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME
import Triggerfish.Odonus.Grid.Types
  ( Action(..), GenKind(..), KnobTarget(..), Stage(..), stagePath, stageFromPath, RegionEdge(..), PlaySource(..), TwisterField(..), Logbook, NoteEvent, PolyInst, Slots, State, applyTarget, genDefaultAmt, genDefaultRate, genKinds, genLabel
  , marblesPadId, rateMax, replayTimelineId, setAmt, setRate, targetRange
   )
import Triggerfish.Scale (scaleTypes)
import Triggerfish.Transport (Sounding(..))
import Triggerfish.Odonus.Grid.Widgets (engrave, style)
import Triggerfish.Odonus.Logbook as Logbook
import Triggerfish.Odonus.View.Scope (scopePanel)
import Triggerfish.Odonus.View.Playheads (playheadsPanel)
import Triggerfish.Odonus.View.Playheads as Playheads
import Triggerfish.Ui.Euclid as Euclid
import Triggerfish.Odonus.View.Grid (gridPanel)
import Triggerfish.Odonus.View.Replay (replayPanel)
import Triggerfish.Odonus.View.Nav (navBar)
import Triggerfish.Odonus.Patch (capturePatch, harmonicSummary, loadText, patchText, recallText, recallGestureText)
import Triggerfish.Odonus.Store as Store
import Triggerfish.Clips as Clips
import Triggerfish.Clips.Store as ClipStore
import Triggerfish.Amphora as Amphora
import Triggerfish.Glyph as G
import Triggerfish.Preset (indexOfContent, presetAlias)
import Triggerfish.Odonus.Lepidoptera (parsePatch, printPatch)
import Triggerfish.SourceQuery (Query(..))
import Triggerfish.Routing.Model as RM
import Triggerfish.Routing.Out as RO
import Triggerfish.Routing.Store as RStore
import Triggerfish.Odonus.View.Generate (generatePanel, cellParamsPanel)
import Triggerfish.Odonus.View.Scenes (sceneName)

-- | The upward message to the shell: Odonus's identity-chip view (or `Nothing` when
-- | nothing is parked), for the six-machine status board. Raised from the Frame loop
-- | only when the view changes (see `chipViewOf`). Mirrors Balistes/Selene's Output.
-- | `StageChanged` carries the new stage's URL segments so the shell can write
-- | the hash. Push, not poll — the shell would otherwise have to interrogate
-- | every machine on a timer to notice a mode change it didn't cause.
data Output = IdentityChanged (Maybe G.ChipView) | StageChanged (Array String)

component :: forall i m. MonadAff m => H.Component Query i Output m
component =
  H.mkComponent
    { initialState: \_ ->
        { odo: M.defaultOdonus, sounding: Silent, dragging: Nothing, dragSub: Nothing
        , notes: [], logbook: Logbook.emptyLog, stage: Perform, selEuclid: Nothing, navScenes: false, playing: Nothing, regionDrag: Nothing, contextOpen: false, clips: [], twisterField: FNote, binnacle: Nothing, nowMicros: 0.0
        , outs: [], routing: RM.defaultTable, midiName: "…", clockTempo: 120.0, clockLocked: false
        , clockBeat: 0.0, clockBar: 0, anchorCount: 0
        , scenes: [], sceneNameInput: "", publishMsg: Nothing
        , stepDiv: 1, headNote: [ Nothing, Nothing, Nothing, Nothing ]
        -- No tables yet: fetched from Amphora on Initialize. Until then the
        -- allocator still works, it just drives uncorrected volts.
        , polys: polyInit []
        , rampleVoices: RV.empty RV.rample
        , polyNote: Just "calibration tables not loaded"
        , swing: 0.0, velHumanize: 12
        , gen: map (\k -> { kind: k, on: false, rate: genDefaultRate k, amt: genDefaultAmt k }) genKinds
        , genSpread: 0.5, genBias: 0.5, genSeed: Marbles.seedFrom 1, genFrozen: false, pending: [], nextModelStep: 0
        -- SOURCE folds away by default: the dedicated TIDAL tab is the
        -- one-stop view of the whole setup; Odonus's own eDSL pane is for
        -- when you want to inspect just this module.
        , collapsed: [ "SOURCE" ], lastTap: "", lastTapMicros: 0.0
        , vetulaHarmony: Nothing, reconciled: false
        , presets: [], identity: Nothing, lastChip: Nothing }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction, handleQuery = handleQuery, initialize = Just Initialize }
    }

-- | Answer the shell: the current eDSL (TIDAL tab), or adopt the rack's shared
-- | free-run baseline so all modules share a downbeat with no rig.
handleQuery :: forall m a. MonadAff m => Query a -> H.HalogenM State Action Slots Output m (Maybe a)
handleQuery = case _ of
  SetRouting t k -> do
    H.modify_ _ { routing = t }
    pure (Just k)
  AskSource reply -> do
    s <- H.get
    pure (Just (reply (patchText s)))
  PutSource _ next -> pure (Just next)   -- shell never rewrites Odonus's patch
  -- No in-machine lane view yet; the rack-wide TIDAL page still owns this one.
  PutLane _ _ next -> pure (Just next)
  -- Routed in from the URL. Goes through `handleAction SetStage` rather than
  -- writing `stage` directly, so arriving by link gets the same hush/clear
  -- treatment as clicking the tab — a URL must not be a laxer path into a stage.
  SetStagePath segs next -> do
    s <- H.get
    for_ (stageFromPath segs) \stg ->
      when (stg /= s.stage) (handleAction (SetStage stg))
    pure (Just next)
  AskClock reply -> do
    s <- H.get
    pure (Just (reply { tempo: s.clockTempo, locked: s.clockLocked }))
  SyncFree startMicros tempo next -> do
    s <- H.get
    for_ s.binnacle \bin ->
      liftEffect (Clock.setFreeBaseline (Binnacle.clock bin) { startMicros, tempo })
    pure (Just next)
  -- The ONE transport query (control-surface MISU refactor). The shell pushes this
  -- machine's derived `Sounding`; we edge-detect and act:
  --   * leaving Local  → note-off held local voices (the scheduler keeps ticking).
  --   * leaving Rig    → per-voice `reef-stop` (silence just THIS rig voice).
  --   * entering Rig   → full `reef-sim-at` handoff (re-issuing Rig re-hands-off).
  -- Local emission is gated on `sounding == Local`; rig streaming on `== Rig`.
  SetSounding s next -> do
    st <- H.get
    let wasLocal = st.sounding == Local
        nowLocal = s == Local
    when (wasLocal && not nowLocal) do
      liftEffect $ silenceHeld st.outs st.routing st.headNote
      -- And the poly instruments, which `silenceHeld` cannot reach: their notes
      -- are not MIDI and the Saïch's oscillators never stop, so a transport that
      -- merely stops ticking leaves the last chord droning for ever. (Rings
      -- returns nothing here and rings out on its own decay, which is the
      -- difference between having a note-off and not.)
      for_ st.binnacle \bin -> do
        let stopped = map (\p -> { p, r: RV.allOff 0.0 p.voices }) st.polys
        liftEffect $ for_ stopped \s ->
          Poly.emitAll (Binnacle.socket bin) s.p.rig 0.0 s.r.emits
        H.modify_ _ { polys = map (\s -> s.p { voices = s.r.voices }) stopped }
      -- Nothing to silence — a struck slice rings out on its own — but the
      -- allocator must forget what it thinks is sounding, or the first notes
      -- after a restart would steal voices that are long finished.
      H.modify_ _ { rampleVoices = RV.empty RV.rample }
    H.modify_ \s' -> s'
      { sounding = s
      , headNote = if wasLocal && not nowLocal then map (const Nothing) s'.headNote else s'.headNote }
    when (st.sounding == Rig && s /= Rig) $
      for_ st.binnacle \bin -> liftEffect $ Transport.send (Binnacle.socket bin) "reef-stop"
    when (s == Rig) (handleAction PushToRig)
    pure (Just next)
  AskSounding reply -> do
    s <- H.get
    pure (Just (reply s.sounding))
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
  -- macro-tidal harmonic authority: install the resting context scale the shell
  -- polled from Vetula as Odonus's pitchSet (the injected-realize seam). Rides the
  -- lockstep-safe RI.SetPitchSet input so the BEAM voice stays in sync. Odonus no
  -- longer owns a scale — it follows whatever Vetula supplies.
  SetContextPitchSet root offsets harmony next -> do
    enqueue (RI.SetPitchSet (PitchSet { offsets, root: 48 + root, period: Just 12 }))
    st <- H.get
    let ours = st.odo.harmony == st.vetulaHarmony
    when (harmony /= st.vetulaHarmony && (isJust harmony || ours)) do
      enqueue (RI.SetHarmony harmony)
    H.modify_ _ { vetulaHarmony = harmony }
    pure (Just next)
  -- The shell's CAPTURE hotkey: bank the live patch as a preset and park identity
  -- on it (the chip shows the freshly-minted glyph, held). See captureNow.
  Capture next -> do
    captureNow
    pure (Just next)
  -- The status-board chip's recall menu: report each preset as its glyph alias +
  -- optional name + star flag; recall / star / delete a chosen preset.
  AskBank reply -> do
    s <- H.get
    pure (Just (reply (mapWithIndex (\i p -> { slot: i, alias: presetAlias p, name: fromMaybe "" p.name, starred: p.starred }) s.presets)))
  RecallSlot i next -> do
    recallPreset i
    pure (Just next)
  StarSlot i next -> do
    H.modify_ \s -> s { presets = fromMaybe s.presets (modifyAt i (\p -> p { starred = not p.starred }) s.presets) }
    persistAll
    pure (Just next)
  DeleteSlot i next -> do
    H.modify_ \s -> s { presets = fromMaybe s.presets (deleteAt i s.presets) }
    persistAll
    pure (Just next)

-- | Bank the live patch as a preset — the CAPTURE hotkey. DEDUPS by content (an
-- | unchanged authored patch ⇒ identical glyph, so hammering the hotkey is
-- | idempotent): already banked ⇒ just re-park `identity`; otherwise append an
-- | anonymous preset. `content` is `patchText s` (the same authored slice `AskSource`
-- | answers — playhead-independent, so playback doesn't perturb it). Then persist.
captureNow :: forall m. MonadAff m => H.HalogenM State Action Slots Output m Unit
captureNow = do
  s <- H.get
  let text = patchText s
  case indexOfContent text s.presets of
    Just _ -> H.modify_ _ { identity = Just text }
    Nothing -> H.modify_ \st -> st
      { presets = st.presets <> [ { content: text, name: Nothing, starred: false } ]
      , identity = Just text
      }
  persistAll

-- | Recall preset `i`: apply its patch text PHASE-PRESERVING (`recallText`, exactly
-- | as RecallScene) so a live change flows on without a playhead jump, and park the
-- | chip on the preset's text (glyph SOLID; ghosts on later divergence).
recallPreset :: forall m. MonadAff m => Int -> H.HalogenM State Action Slots Output m Unit
recallPreset i = do
  st <- H.get
  case st.presets !! i of
    Nothing -> pure unit
    Just p -> do
      H.modify_ \s -> (recallText p.content s) { identity = Just p.content }
      persistAll

-- | The identity-chip view Odonus reports to the shell's status board: the glyph of
-- | the parked patch + whether the live patch has diverged from it (edited away).
-- | `Nothing` when nothing is parked. `patchText` excludes the playhead/seed, so a
-- | running-but-unedited patch stays SOLID.
chipViewOf :: State -> Maybe G.ChipView
chipViewOf s = case s.identity of
  Nothing -> Nothing
  Just text -> Just { glyph: G.glyphOf text, diverged: patchText s /= text }

-- | Run the action, then persist the live patch — except for the high-frequency
-- | / non-authoring actions (the clock tick, the river frame, a knob DRAG in
-- | flight, MIDI readiness, and Initialize itself, which has just restored).
-- | DragEnd is NOT excluded, so a knob edit persists once it settles.
handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action Slots Output m Unit
handleAction a = do
  -- The frame tick keeps the clock in state only while something drawn needs it
  -- (see Frame), so anything else that reads it (a tap's debounce, a logbook
  -- mark, a capture, a gesture's tag) gets it fresh first. Not the ticks
  -- themselves, and not a drag in flight, which writes state on every move.
  case a of
    Frame -> pure unit
    Step _ -> pure unit
    DragMove _ -> pure unit
    Initialize -> pure unit
    _ -> freshClock
  dispatch a
  case a of
    Frame -> pure unit
    Step _ -> pure unit
    DragMove _ -> pure unit
    MidiReady _ _ -> pure unit
    Initialize -> pure unit
    SetSceneName _ -> pure unit   -- per-keystroke; nothing authoring changed yet
    PublishScene _ -> pure unit   -- a network write; no local authoring changed
    _ -> persistAll

-- | Persist the live working patch + the named scene library.
persistAll :: forall o m. MonadAff m => H.HalogenM State Action Slots o m Unit
persistAll = do
  s <- H.get
  liftEffect (Store.saveAll { live: patchText s, scenes: s.scenes, presets: s.presets })

-- | Persist the captured clip harvest to the SHARED library store (#27) — separate
-- | from the Odonus patch envelope, so a clip is pickable in Vetula and beyond.
persistClips :: forall o m. MonadAff m => H.HalogenM State Action Slots o m Unit
persistClips = do
  s <- H.get
  liftEffect (ClipStore.saveClips s.clips)

-- | An Amphora library item as a local scene (payload = the scene's eDSL text).
amphoraScene :: Amphora.LibItem -> { name :: String, text :: String }
amphoraScene it = { name: it.name, text: it.payload }

-- | Merge incoming (Amphora) scenes over the current local ones by name: keep
-- | every local scene, then append any incoming scene whose name isn't present.
mergeScenesByName
  :: Array { name :: String, text :: String }
  -> Array { name :: String, text :: String }
  -> Array { name :: String, text :: String }
mergeScenesByName current incoming =
  current <> filter (\p -> not (any (\q -> q.name == p.name) current)) incoming

dispatch :: forall m. MonadAff m => Action -> H.HalogenM State Action Slots Output m Unit
dispatch = case _ of
  Initialize -> do
    -- Announce the opening stage so the shell can write a COMPLETE URL from a cold
    -- start (`#{slug}/{stage}`, not the bare `#{slug}`). Without this the address
    -- bar under-specifies until you touch a stage tab — still a valid route, since
    -- an empty stage path means "leave the stage alone", but not a link that
    -- reopens what you were actually looking at.
    H.gets _.stage >>= \stg -> H.raise (StageChanged (stagePath stg))
    -- Connect to the rig. Binnacle's clock free-runs at 120 until the
    -- Link anchor arrives, then phase-locks — so Triggerfish runs solo
    -- without the rig, and joins the ensemble the moment it's up.
    bin <- liftEffect $ Binnacle.connect { url: rigUrl, tempo: 120.0 }
    -- Follow the rig: an `odonus` line evaluated elsewhere (Limulus) is applied
    -- by the BEAM voice, which broadcasts its tick-tagged gestures; queue them
    -- here like our own so both runtimes apply them on the same step.
    { emitter: rigE, listener: rigL } <- liftEffect HS.create
    _ <- H.subscribe rigE
    liftEffect $ Binnacle.onAppMessage bin (HS.notify rigL <<< RigFrame)
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
        -- EVERY output port, because the routing table may name any of them —
        -- so adding a destination in the router works with no MIDI re-init.
        outs <- RO.openAll access
        names <- Midi.outputNames access
        -- Control surface IN (MidiFighter Twister, bank 1): route every incoming
        -- message through the SAME Action pipeline the trackpad uses, so a
        -- knob-turn is byte-identical to a knob-drag (and stays in lockstep on-rig).
        -- Missing surface → skip; the subscription lives for the session. Fold the
        -- in/out port status into one line so a name-match miss is visible, not silent.
        minput <- Midi.findInput access twisterInputName
        innames <- Midi.inputNames access
        -- WHICH ports matter is now a property of the routing table, so the
        -- "is anything missing" judgement moved to the view (`midiPortsOk`),
        -- against the actual routes. This line records only what exists.
        let twNm = case minput of
              Just _ -> twisterInputName <> " ✓"
              Nothing -> "no '" <> twisterInputName <> "' in: " <> joinWith ", " innames
            nm = show (length names) <> " ports · " <> twNm
        HS.notify midiL (MidiReady outs nm)
        for_ minput \inp -> void $ Midi.onMessage inp \m ->
          HS.notify midiL (TwisterMsg m.status m.data1 m.data2)
      Nothing -> HS.notify midiL (MidiReady [] "unavailable")
    H.modify_ _ { binnacle = Just bin }
    -- Restore the saved scene library + the live working patch (each is
    -- Lepidoptera text; unparseable / absent storage falls back to defaults).
    msaved <- liftEffect Store.loadAll
    for_ msaved \sv -> do
      H.modify_ _ { scenes = sv.scenes, presets = sv.presets }
      H.modify_ (loadText sv.live)
    -- Cold-start read of the routing table. The shell OWNS it and pushes it
    -- down via SetRouting; this only covers the window before its first
    -- broadcast, so a solo-mounted Odonus is routed from the first tick.
    mroute <- liftEffect RStore.load
    for_ mroute \t -> H.modify_ _ { routing = t }
    -- Restore the shared MIDI clip library (#27) — captured anywhere, pickable here.
    savedClips <- liftEffect ClipStore.loadClips
    H.modify_ _ { clips = savedClips }
    -- Merge the shared Amphora scene library over the local one (by name), in the
    -- BACKGROUND: awaiting it blocked Initialize (hence all queries to Odonus) until
    -- the ~30s offline timeout. The store being offline is not fatal — keep local.
    void $ H.fork do
      dbRes <- liftAff (attempt (Amphora.fetchCollection "odonus-scene"))
      case dbRes of
        Right items | not (null items) ->
          H.modify_ \s -> s { scenes = mergeScenesByName s.scenes (map amphoraScene items) }
        _ -> pure unit
    -- Calibration tables for any poly instrument, likewise in the background and
    -- likewise non-fatal. Missing tables are LOUD rather than silent: without
    -- them the four Saich voices differ by up to 16 cents at the same voltage,
    -- so a note migrating between oscillators shifts pitch — which is precisely
    -- the artefact voice allocation is meant not to introduce.
    void $ H.fork do
      calRes <- liftAff (attempt (Amphora.fetchCollection "vco-calibrations"))
      case calRes of
        Left _ ->
          H.modify_ _ { polyNote = Just "Amphora unreachable — poly voices uncorrected" }
        Right items -> do
          let polys = polyInit items
              missing = concatMap (\p -> filter isNothing p.rig.tables) polys
              wanted = length (concatMap (\p -> p.rig.tables) polys)
          H.modify_ _
            { polys = polys
            , polyNote =
                if null missing then Nothing
                else Just (show (length missing) <> " of " <> show wanted
                  <> " poly calibration tables missing — those voices uncorrected")
            }
  Step tick -> do
    -- Note-offs must not wait for the next MODEL step. A gate shorter than a
    -- step would otherwise sound until something replaced it, so gate length
    -- would have no audible effect at all — and with `stepDiv` above 1 the
    -- error is several beats. Retire on every scheduler tick, the finest grid
    -- this component sees.
    stTick <- H.get
    when (stTick.sounding == Local && isNothing stTick.playing) do
      for_ stTick.binnacle \bin -> do
        let stepped = map (\p -> { p, r: RV.expireAt tick.firePerfMs p.voices }) stTick.polys
        unless (null (concatMap (\s -> s.r.emits) stepped)) do
          liftEffect $ for_ stepped \s ->
            Poly.emitAll (Binnacle.socket bin) s.p.rig tick.firePerfMs s.r.emits
          H.modify_ _ { polys = map (\s -> s.p { voices = s.r.voices }) stepped }
    st <- H.get
    -- Global step divider: the scheduler ticks on a fine 1/16 grid; advance the
    -- model only every stepDiv ticks, so STEP LENGTH sets what a 1× head plays.
    -- CO-SIM: advance the model whenever the machine is armed (Local OR Rig), not
    -- just Local — otherwise Atlantis mode freezes the display. `tick.index` is
    -- Link-absolute (ceil beat/stepBeats), so the frontend's modelStep matches the
    -- rig's step and the co-simulation stays byte-identical (same inputs are
    -- broadcast). MIDI emission below stays Local-only; the BEAM sounds in Rig mode.
    when (st.sounding /= Silent && tick.index `mod` st.stepDiv == 0) do
      let
        modelStep = tick.index / st.stepDiv
        -- LOCKSTEP (P4c): apply any tick-tagged inputs whose step has arrived
        -- BEFORE the model steps — exactly as reef_voice's drain does on the BEAM,
        -- so a deferred gesture lands on the same step on both runtimes. `<=` (not
        -- `==`) self-heals: an input tagged while the transport was stopped applies
        -- on the first step after it resumes; drained entries drop from `pending`.
        due = filter (\p -> p.step <= modelStep) st.pending
        stillPending = filter (\p -> p.step > modelStep) st.pending
        -- HARMONY: then the chord overlay follows Odonus's Tidal harmony pattern,
        -- if it has one, as Littorina reads it at this step's cycle position
        -- (stepDiv quarter-beats a step, four beats a cycle) — the same call
        -- reef_voice makes on the BEAM.
        -- The scale pattern (`scale "..."`) is sampled first, as the harmony
        -- snaps past the scale; reef_voice does the same.
        sim0 = RE.followHarmony (Harmony.harmonySampler (modelStep * st.stepDiv) 16)
                 (RE.followScale (Scales.scaleSampler (modelStep * st.stepDiv) 16)
                   (RI.applyInputs (map _.input due)
                     { odo: st.odo, gen: st.gen, spread: st.genSpread, bias: st.genBias, seed: st.genSeed, frozen: st.genFrozen }))
        -- The randomisation matrix fires BEFORE the heads read, so any mutated
        -- value is what plays this step. Each source drifts one notch at a time.
        g = Gen.runGen
              { gen: sim0.gen, spread: sim0.spread, bias: sim0.bias
              , odo: sim0.odo, seed: sim0.seed, frozen: sim0.frozen }
        o1 = g.odo
        -- Heads the generator just silenced (HEADS source) get a note-off below,
        -- so a glide note can't stick on a voice that's now muted.
        muteOf o h = maybe true _.mute (o.heads !! h)
        newlyMuted = filter (\h -> not (muteOf st.odo h) && muteOf g.odo h) (range 0 3)
        r = M.stepEmit o1
        msPerBeat = 60000.0 / max 30.0 st.clockTempo
        -- One model step in ms (a 1× head's note spacing at this STEP LENGTH).
        stepMs = (0.25 * toNumber st.stepDiv) * msPerBeat
        -- Swing: lag the off-beat (odd) model steps by a fraction of a step, so
        -- the grid breathes instead of being metronomic. Applied to the audible
        -- onset (and the scope), not the model advance.
        swingMs = if modelStep `mod` 2 == 1 then st.swing * stepMs else 0.0
        -- ABSOLUTE onset (performance.now ms) for this step's notes: schedule at the
        -- fire time itself, not now+delay, so render-pipeline latency isn't added.
        emitAtMs = tick.firePerfMs + swingMs
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
              v = clamp 1 127 (f.vel + accent + hum)
          in { items: acc.items <> [ { f, v } ], seed }
        velied = foldl velStep { items: [], seed: g.seed } r.fired
        firedV = velied.items
      -- Local MIDI I/O only. In Rig (Atlantis) mode the BEAM voice sounds; the
      -- frontend advances the same model purely to mirror it (co-sim display), so
      -- it must NOT also emit — otherwise you'd double-trigger on the rig.
      -- Solo: while a REPLAY loop is running, the live model still advances
      -- (silently) but does NOT emit — replay owns the MIDI out (#151, R2b).
      when (st.sounding == Local && isNothing st.playing) do
        -- Silence any voice the generator muted this step.
        liftEffect $ for_ newlyMuted \h -> case join (st.headNote !! h) of
          Just n -> noteOffEverywhere st.outs st.routing h n
          Nothing -> pure unit
        -- Emit MIDI with per-head legato: glide cells HOLD until the next note
        -- (tie if same pitch, portamento-slide if different); non-glide cells are
        -- gated notes whose length scales with tempo.
        liftEffect $ for_ firedV \fv ->
          emitNote st.outs st.routing fv.f.headIdx emitAtMs (gateMsFor fv.f) fv.v
            (prevOf fv.f.headIdx) fv.f

      -- Poly instruments are driven separately from the MIDI fan-out, because
      -- an allocator is STATEFUL and shared: several heads routed to one Saich
      -- are competing for the same four oscillators, so they cannot each be
      -- handled independently the way a MIDI leg can. One pass per INSTRUMENT —
      -- two instruments share nothing, so they get a state each.
      when (st.sounding == Local && isNothing st.playing) do
        for_ st.binnacle \bin -> do
          let sock = Binnacle.socket bin
              polyNotes = map
                (\fv -> { headIdx: fv.f.headIdx, pitch: fv.f.pitch, gateMs: gateMsFor fv.f })
                firedV
              played = map (playPoly st.routing polyNotes emitAtMs) st.polys
          liftEffect $ for_ played \p ->
            Poly.emitAll sock p.poly.rig emitAtMs p.emits
          H.modify_ _ { polys = map _.poly played }

      -- The Rample as ONE instrument, its four voices allocated. Outside the
      -- `binnacle` block above because this one needs no ES-9 socket: the
      -- allocator's decisions leave as MIDI like any other note.
      when (st.sounding == Local && isNothing st.playing) do
        for_ (ramplePolyOf st.routing) \cfg -> do
          let rampleNotes = map
                (\fv -> { headIdx: fv.f.headIdx, pitch: fv.f.pitch
                        , gateMs: gateMsFor fv.f, vel: fv.v })
                firedV
              rr = playRample cfg st.routing rampleNotes emitAtMs st.rampleVoices
          liftEffect do
            emitRample st.outs cfg rr.emits
            for_ rr.refused \pitch ->
              Console.warn $ "[rample] dropped note " <> show pitch
                <> ": outside the card (" <> show cfg.pitchOfSlot0 <> ".."
                <> show (cfg.pitchOfSlot0 + cfg.slots - 1) <> ")"
          H.modify_ _ { rampleVoices = rr.voices }
      let
        -- A glide note stays held (its pitch); a gated note auto-ends.
        nextNote f = if f.glide then Just f.pitch else Nothing
        newHeadNote = foldl
          (\arr f -> fromMaybe arr (updateAt f.headIdx (nextNote f) arr))
          st.headNote r.fired
        -- Clear the held-note slots of voices the generator just muted.
        clearedHeadNote = foldl (\arr h -> fromMaybe arr (updateAt h Nothing arr)) newHeadNote newlyMuted
        -- Built from `firedV` (not `r.fired`) so the capture carries velocity and
        -- gate length too — REPLAY re-emits these faithfully. The scope ignores them.
        fresh = map (\fv -> { pitch: fv.f.pitch, headIdx: fv.f.headIdx
                            , fireUnixMicros: tick.fireUnixMicros + swingMs * 1000.0
                            , vel: fv.v, gateMs: gateMsFor fv.f }) firedV
      H.modify_ \s -> s
        { odo = r.odo, notes = fresh <> s.notes, headNote = clearedHeadNote
        -- Performance logbook (#151): the same fresh notes accumulate, unpruned,
        -- into the always-on capture (chunked + retention-bounded). Frontend-only.
        -- Paused while a REPLAY loop runs, so replay-time silent gen doesn't pollute
        -- the log.
        , logbook = if isJust s.playing then s.logbook
                    else Logbook.logAppend (tick.fireUnixMicros) fresh s.logbook
        -- LOCKSTEP (P4c, Option 2): the model seed advances ONLY via runGen (g.seed),
        -- NOT via the velocity-humanise draws (velied.seed). Humanise still reads the
        -- seed to jitter velocity, but must not perturb the shared generative stream —
        -- otherwise the frontend's seed would diverge from the rig's (which does no
        -- humanise), and the co-simulation would drift. Expression stays local; the
        -- model stays byte-identical to reef_engine.stepTick on the BEAM.
        , genSeed = g.seed
        -- Write back the gen-config / pad any DUE inputs mutated (a no-op unless a
        -- gen gesture was synced this step), and drop the drained pending entries.
        , gen = sim0.gen, genSpread = sim0.spread, genBias = sim0.bias, genFrozen = sim0.frozen
        , pending = stillPending
        -- LOCKSTEP (P5): the state written back here (odo/gen/genSeed) is exactly
        -- what the NEXT model step will consume, and that step is modelStep + 1. A
        -- Push reads this to stamp the handoff (reef-sim-at), so the BEAM plays the
        -- pushed state on the same absolute step the frontend will — no phase flam.
        , nextModelStep = modelStep + 1 }
  Frame -> do
    st <- H.get
    case st.binnacle of
      Just bin -> do
        -- Connect-time rig reconcile (once, hidden): fire a global `hush` on the
        -- first Frame — by now (~one frame after connect) the WS is open, whereas
        -- an inline send right after connect would race the handshake and drop
        -- (Binnacle's send no-ops on a not-open socket). Clears voices orphaned by
        -- a previous session; a fresh load has nothing armed, so the rig should be
        -- silent, and arming re-pushes. Keeps the "user never tracks rig state" MISU
        -- promise across a frontend reload.
        when (not st.reconciled) do
          liftEffect $ Transport.send (Binnacle.socket bin) "hush"
          H.modify_ _ { reconciled = true }
        now <- liftEffect $ Clock.unixMicrosNow (Binnacle.clock bin)
        r <- liftEffect $ Clock.read (Binnacle.clock bin)
        -- Scene SEQUENCING moved to the macro-tidal Tidal page; Odonus just tracks
        -- the clock here (scenes are captured/recalled, not auto-chained).
        --
        -- Written only while something drawn needs it: every write re-renders the
        -- whole grid, and at 30 a second, idle, that was most of this page's CPU.
        -- What is drawn from the clock: the tempo, the lock, the bar, the BEAT
        -- readout, and the river, which moves only while a note or a mark is still
        -- inside its window. REPLAY runs from here, so it keeps the clock current
        -- too. Other readers take it fresh (`freshClock`, in handleAction).
        let inWindow at = now - at < windowMicros || st.nowMicros - at < windowMicros
            moved = r.tempo /= st.clockTempo || r.locked /= st.clockLocked
              || r.bar /= st.clockBar || r.anchorCount /= st.anchorCount
              || floor r.beat /= floor st.clockBeat
              || not (null st.notes) || isJust st.playing
              || any (inWindow <<< _.atMicros) st.logbook.marks
        when moved $ H.modify_ \s -> s
          { nowMicros = now
          , clockTempo = r.tempo
          , clockLocked = r.locked
          , clockBeat = r.beat
          , clockBar = r.bar
          , anchorCount = r.anchorCount
          , notes = filter (\n -> (now - n.fireUnixMicros) < windowMicros) s.notes
          }
        driveReplay
      Nothing -> pure unit
    -- Report the identity chip up to the shell's status board, but only when it
    -- actually changed (this fires ~30×/s) — capture/recall/divergence all land here.
    s2 <- H.get
    let cv = chipViewOf s2
    when (cv /= s2.lastChip) do
      H.modify_ _ { lastChip = cv }
      H.raise (IdentityChanged cv)
  MidiReady outs nm -> H.modify_ _ { outs = outs, midiName = nm }
  -- A move made on the rig (`odonus $ ...`, Reef.Move): its gestures arrive
  -- tagged for the step the BEAM voice applies them on, and join `pending`, as
  -- `enqueue` does for ours, so the Step loop applies them on that same step and
  -- the panel shows what is playing. Only while the rig is what's sounding.
  -- A Review cue from Limulus (`odonus $ mark`, `odonus $ loop 2`, `loop off`):
  -- what the Review surface's own controls do. A loop opens the Review surface.
  RigFrame msg | Just cue <- Cue.readCue "odonus" msg -> case cue of
    Cue.MarkCue -> handleAction MarkNow
    Cue.StopCue -> handleAction StopPlay
    Cue.LoopCue n -> do
      st <- H.get
      let i = if n == 0 then length st.logbook.marks - 1 else n - 1
      when (i >= 0 && i < length st.logbook.marks) do
        when (st.stage /= Review) (handleAction (SetStage Review))
        handleAction (PlayRegion i)
  RigFrame msg -> for_ (Str.stripPrefix (Str.Pattern "reef-input ") msg) \json ->
    case decodeTagged json of
      Left _ -> liftEffect $ Console.warn ("Odonus: a reef-input from the rig did not decode: " <> take 120 json)
      Right t -> do
        st <- H.get
        when (st.sounding == Rig) do
          when (t.tick < st.nextModelStep) $ liftEffect $ Console.warn
            ("Odonus: a rig move for step " <> show t.tick <> " arrived at step "
              <> show st.nextModelStep <> "; applied late, so this page may drift from the rig")
          H.modify_ \s -> s { pending = s.pending <> [ { step: t.tick, input: t.input } ] }
  -- MidiFighter Twister, bank 1: the 16 encoders map 1:1 onto the 16 cells.
  -- ROTATE (absolute CC on the rotate channel) sets the ACTIVE grid's field for that
  -- cell — scaled from 0..127 into the field's range and pushed through the SAME
  -- Set* input a trackpad drag emits, so it defers + broadcasts (lockstep P4c)
  -- identically. PUSH on a top-row encoder (0..3) SELECTS the active grid
  -- (NOTE/LEN/RATCHET/VEL). Other channels / kinds / future banks are ignored.
  TwisterMsg status d1 d2 -> do
    let kind = status `div` 16   -- high nibble: 0xB CC, 0x9 note-on
        ch = status `mod` 16     -- low nibble: 0-based MIDI channel
        bank = d1 `div` 16       -- CC 0..15 = bank 1 (cells), 16..31 = bank 2 (voices)
        idx = d1 `mod` 16        -- position within the bank's 4×4
        isRotate = kind == 0xB && ch == twisterRotateCh
        isPush = (kind == 0xB && ch == twisterPushCh && d2 > 0) || (kind == 0x9 && d2 > 0)
    case bank of
      -- Bank 1: the 16 cells (value + boolean grids) + the Notes-pane MACRO.
      0 ->
        if isRotate then do
          st <- H.get
          case st.twisterField of
            -- Value grids: 0..127 → the field's range → the same Set* input the knob emits.
            FNote -> valueRotary (CellNote idx) 0 M.knobMax d2
            FLen -> valueRotary (CellDur idx) 1 8 d2
            FRatchet -> valueRotary (CellRatchet idx) 1 8 d2
            FVel -> valueRotary (CellVel idx) 1 127 d2
            -- Boolean grids: right (≥64) = on, left = off. Fire the existing TOGGLE
            -- only when the cell disagrees, giving absolute on/off from a toggle input.
            FGate -> for_ (st.odo.cells !! idx) \c -> when (c.gate /= (d2 >= 64)) (twisterApply (RI.ToggleGate idx))
            FSkip -> for_ (st.odo.cells !! idx) \c -> when (c.skip /= (d2 >= 64)) (twisterApply (RI.ToggleSkip idx))
            FGlide -> for_ (st.odo.cells !! idx) \c -> when (c.glide /= (d2 >= 64)) (twisterApply (RI.ToggleGlide idx))
            -- Macro pane: the 16 rotaries drive the Notes-pane globals, not the cells.
            FMacro -> twisterMacro idx d2 st
        else if isPush then
          for_ (twisterFieldForPush idx) \fld -> H.modify_ _ { twisterField = fld }
        else pure unit
      -- Bank 2: each ROW is a voice (head 0..3); the four columns are pattern /
      -- euclid-k / euclid-n / transpose. Pushes here are unmapped for now.
      1 -> when isRotate $ twisterVoice idx d2
      _ -> pure unit
  -- Cell edits — deferred + broadcast (lockstep P4c) so they land on the same
  -- model step on both runtimes.
  ToggleGlide i -> enqueue (RI.ToggleGlide i)
  ToggleGate i -> enqueue (RI.ToggleGate i)
  ToggleSkip i -> enqueue (RI.ToggleSkip i)
  SetAllNotes v -> enqueue (RI.SetAllNotes v)
  -- SeedMelody THREADS the shared seed, so deferred-on-both is mandatory: applied
  -- a step apart the two PRNGs would desync permanently. (Reef.Gen.seedMelody
  -- ignores its harmony-PC arg, so RI.SeedMelody == the old inline call.)
  SeedMelody -> enqueue RI.SeedMelody
  -- A FORM is a KNOWN line where MELODY is a random one, so it needs no PRNG:
  -- the figure is resolved against the live model here and travels as one
  -- absolute `SetNotes`, which replays identically on the rig without the
  -- BEAM having to carry the library.
  StampForm ix -> do
    st <- H.get
    enqueue (RI.SetNotes (Forms.stampForm ix st.odo))
  -- LOCKSTEP (P4c): head mute + activation-matrix are DEFERRED, not applied now —
  -- enqueued for a near-future step and broadcast to the rig so both runtimes flip
  -- the head on the same step (no flam through the edit). The note-off + held-slot
  -- clearing that the immediate handlers used to do is now done by the Step loop's
  -- `newlyMuted` path when the deferred mute actually lands (it compares the
  -- pre-step odo to the post-gen odo, so a manual mute is caught there too).
  ToggleHeadMute h -> enqueue (RI.ToggleHeadMute h)
  SetHeadMask mask -> enqueue (RI.SetHeadMask mask)
  -- Head gestures — deferred + broadcast (lockstep P4c).
  CyclePattern h -> enqueue (RI.CyclePattern h)
  SetHeadDir h d -> enqueue (RI.SetHeadDir h d)
  -- Click-radios / clickers: same deferred-broadcast lockstep path as SetHeadDir.
  -- Absolute sets (the model clamps), so replaying the value on the rig is idempotent.
  SetHeadSpeed h ix -> enqueue (RI.SetHeadSpeedIx h ix)
  -- Relative nudges, so a burst of clicks accumulates even while the edit is
  -- buffered for the rig (an absolute current±1 would re-read the stale value).
  NudgeHeadPulses h d -> enqueue (RI.NudgeHeadPulses h d)
  NudgeHeadSteps h d -> enqueue (RI.NudgeHeadEuclidSteps h d)
  -- Click a voice's Euclid ring to select it; arrows then edit it (Selene's idiom,
  -- now the shared one). Selecting is local-only — it changes what you're pointing
  -- at, not what the rig plays, so it never goes near the lockstep queue.
  SelectEuclid h -> H.modify_ \s -> s { selEuclid = Just h }
  -- The ring lost focus — a click anywhere outside it, which should read as
  -- "nothing is selected" rather than leaving a lit ring that no longer takes the
  -- arrow keys. Guarded on the head index so a late blur from the ring you just
  -- LEFT can't clear the ring you just arrived at.
  DeselectEuclid h -> H.modify_ \s ->
    if s.selEuclid == Just h then s { selEuclid = Nothing } else s
  -- An arrow on the focused ring. The shared widget turns the keystroke into ONE
  -- signed axis edit, which then rides the existing relative inputs — so a burst of
  -- arrow presses accumulates correctly while the edits are buffered for the rig,
  -- exactly as the corner clickers did.
  EuclidKey ev -> do
    s <- H.get
    for_ s.selEuclid \h ->
      for_ (Euclid.dirOf (KE.key ev)) \dir ->
        for_ (s.odo.heads !! h) \hd -> do
          liftEffect (preventDefault (KE.toEvent ev))
          let cur = { beats: hd.pulses, steps: hd.esteps }
              d = Euclid.stepOf Playheads.euclidBounds dir (KE.shiftKey ev) cur
          when (d.amount /= 0) $ enqueue $ case d.axis of
            Euclid.Beats -> RI.NudgeHeadPulses h d.amount
            Euclid.Steps -> RI.NudgeHeadEuclidSteps h d.amount
  UnifyHeads -> enqueue RI.UnifyHeads
  PhaseShift d -> enqueue (RI.NudgeOffsets d)
  CycleScaleType dir -> enqueue (RI.CycleScaleType dir)
  -- The Select widget picks a preset by NAME; jump straight to it by driving the
  -- existing relative CycleScaleType input. `cur`/`tgt` use the same fromMaybe(-1)
  -- rule cycleScaleType uses internally, so the delta lands exactly on target even
  -- from a custom (unrecognised) scale.
  PickScale name -> do
    st <- H.get
    let names = map _.name scaleTypes
        cur = fromMaybe (-1) (findIndex (\n -> n == M.scaleTypeName st.odo) names)
        tgt = fromMaybe cur (findIndex (\n -> n == name) names)
    when (tgt /= cur) $ enqueue (RI.CycleScaleType (tgt - cur))
  ToggleDist -> enqueue RI.ToggleDistribution
  -- Quantizer gestures — deferred + broadcast (lockstep P4c).
  SetRoot pc -> enqueue (RI.SetRoot pc)
  SetOctave n -> enqueue (RI.SetOctaveShift n)
  SetDegShift n -> enqueue (RI.SetDegShift n)
  ToggleScaleNote pc -> enqueue (RI.ToggleScaleNote pc)
  -- Capture the WHOLE current setup under the typed name (or an auto-name), as
  -- its Lepidoptera text — the named, recallable preset. (persistAll runs in the
  -- handleAction wrapper.)
  CaptureScene -> H.modify_ \s ->
    let nm = if s.sceneNameInput == "" then sceneName s else s.sceneNameInput
    in s { scenes = s.scenes <> [ { name: nm, text: printPatch ((capturePatch s) { name = nm }) } ]
         , sceneNameInput = "" }
  SetSceneName n -> H.modify_ _ { sceneNameInput = n }
  -- Publish scene i to the shared Amphora store (odonus-scene collection). The
  -- scene text is already its canonical Lepidoptera form; the name rides the
  -- label. Store offline → a transient failure message, never fatal.
  PublishScene i -> do
    s <- H.get
    case s.scenes !! i of
      Nothing -> pure unit
      Just sc -> do
        H.modify_ _ { publishMsg = Just "publishing…" }
        res <- liftAff (attempt (Amphora.publish
          { kind: "odonus-scene", collection: "odonus-scene"
          , name: sc.name, source: "user", payload: sc.text, tags: [] }))
        H.modify_ _ { publishMsg = Just case res of
          Right hash -> "✓ " <> sc.name <> " · " <> take 8 hash
          Left _ -> "✗ publish failed (store offline?)" }
  RecallScene i -> H.modify_ \s -> case s.scenes !! i of
    Just sc -> recallText sc.text s
    Nothing -> s
  -- Recall the scene's gesture but stay in the live key/progression (#150).
  RecallGesture i -> H.modify_ \s -> case s.scenes !! i of
    Just sc -> recallGestureText sc.text s
    Nothing -> s
  DeleteScene i -> H.modify_ \s -> s { scenes = fromMaybe s.scenes (deleteAt i s.scenes) }
  -- Performance logbook (#151): flag / drop a good bit, or purge the whole log.
  MarkNow -> H.modify_ \s ->
    let rb = Logbook.regionBounds s.clockTempo s.nowMicros s.clockBeat
        m = { atMicros: s.nowMicros, beat: s.clockBeat, from: rb.from, to: rb.to, patch: patchText s }
    in s { logbook = Logbook.pushMark m s.logbook }
  DeleteMark i -> H.modify_ \s -> s { logbook = Logbook.deleteMark i s.logbook }
  ClearLog -> H.modify_ \s -> s { logbook = Logbook.emptyLog }
  -- Resizing the surface is NOT a transport or session action (AC, 2026-08-06).
  -- It used to be: leaving REPLAY wiped the logbook, on the theory that the buffer
  -- was a per-visit scratchpad. But the generator never stopped, ◆ mark should work
  -- either way, and silently discarding the take on a VIEW change is exactly the
  -- incoherence the rename fixes. The logbook now survives; only the region PREVIEW
  -- stops, because its stop control lives on the surface being collapsed.
  SetStage v -> do
    st <- H.get
    when (st.stage == Review && v == Perform) hushReplayVoices
    H.raise (StageChanged (stagePath v))
    H.modify_ \s -> s
      { stage = v
      , playing = if v == Review then s.playing else Nothing
      , regionDrag = if v == Review then s.regionDrag else Nothing
      , contextOpen = if v == Review then s.contextOpen else false
      }

  ToggleSceneMenu -> H.modify_ \s -> s { navScenes = not s.navScenes }
  -- REPLAY (#151, R2b): start looping the one-bar region around mark i. The Frame
  -- loop (driveReplay) schedules each iteration; StopPlay ends it.
  PlayRegion i -> startRegion i
  -- Stop the loop AND cut anything already sounding: the windowed scheduler
  -- leaves at most one lookahead of notes queued, and all-notes-off silences a
  -- note mid-ring, so stop is instant.
  StopPlay -> do
    hushReplayVoices
    H.modify_ _ { playing = Nothing }
  -- REPLAY region drag (#151, R2c): grab a band's edge (resize) or body (slide).
  -- The pointer maps straight to a recording time via padNorm over the timeline.
  RegionDown i edge cx cy -> do
    sid <- setupRegionDrag
    st <- H.get
    grab <- liftEffect $ pointerMicros st cx cy
    for_ (st.logbook.marks !! i) \m ->
      H.modify_ _ { regionDrag = Just
        { markIdx: i, edge, grabMicros: grab, startFrom: m.from, startTo: m.to, moved: false }
      , dragSub = Just sid }
  RegionMove cx cy -> do
    st <- H.get
    for_ st.regionDrag \rd -> do
      cur <- liftEffect $ pointerMicros st cx cy
      -- A body grab only becomes a slide past a small threshold, so a click (with
      -- a stray pixel of jitter) still plays; edge grabs resize from the first move.
      let past = abs (cur - rd.grabMicros) > (timelineBounds st).span * 0.005
      when (rd.moved || rd.edge /= EdgeBody || past) do
        let d = cur - rd.grabMicros
            minLen = 60.0e6 / max 30.0 st.clockTempo   -- ≥ one beat
            bounds = case rd.edge of
              EdgeFrom -> { from: min (rd.startTo - minLen) cur, to: rd.startTo }
              EdgeTo -> { from: rd.startFrom, to: max (rd.startFrom + minLen) cur }
              EdgeBody -> { from: rd.startFrom + d, to: rd.startTo + d }
        H.modify_ \s ->
          let s1 = setRegionBounds rd.markIdx bounds s
              s2 = s1 { regionDrag = map (_ { moved = true }) s1.regionDrag }
          in syncPlaying rd.markIdx bounds s2
  RegionUp -> do
    st <- H.get
    for_ st.dragSub H.unsubscribe
    for_ st.regionDrag \rd ->
      case rd.edge, rd.moved of
        -- A bare click on the body plays the region; a resize/slide is finalized
        -- by snapping its edges to the beat grid so a freehand drag stays musical.
        EdgeBody, false -> startRegion rd.markIdx
        _, _ -> for_ (st.logbook.marks !! rd.markIdx) \m ->
          let snapped = { from: Logbook.snapMicrosToBeat st.clockTempo m m.from
                        , to: Logbook.snapMicrosToBeat st.clockTempo m m.to }
          in H.modify_ \s -> syncPlaying rd.markIdx snapped (setRegionBounds rd.markIdx snapped s)
    H.modify_ _ { regionDrag = Nothing, dragSub = Nothing }
  -- Promote a captured good bit into the SCENES list: a mark's stored patch IS
  -- a scene's text (same Lepidoptera form), so the loop can graduate into a
  -- chainable, recallable (as-saved / in-key #150) scene. Auto-named by its key.
  SaveMarkScene i -> H.modify_ \s -> case s.logbook.marks !! i of
    Just m ->
      let nm = case harmonicSummary m.patch of
                 Just h -> "loop · " <> h.root <> " " <> h.scale
                 Nothing -> "loop " <> show (i + 1)
      in s { scenes = s.scenes <> [ { name: nm, text: m.patch } ] }
    Nothing -> s
  -- Lift a mark's region out as a captured clip (#27): copy its notes, rebased to
  -- zero, into a shared-library `MidiClip` with metadata populated cheaply from the
  -- capturing patch (key/scale → tags/key/context, tempo → bpm, machine → source).
  -- Full fidelity is preserved (headIdx/vel/gate); lossy projections are deferred to
  -- playback. Persisted to the SHARED store, so it's pickable in Vetula and beyond.
  SaveMarkClip i -> do
    st <- H.get
    for_ (st.logbook.marks !! i) \m -> do
      let hsum = harmonicSummary m.patch
          nm = case hsum of
                 Just h -> "clip · " <> h.root <> " " <> h.scale
                 Nothing -> "clip " <> show (length st.clips + 1)
          evs = materializeRegion m.from m.to st.logbook
          clip =
            { id: "odonus-" <> show m.atMicros
            , events: evs
            , lenMicros: m.to - m.from
            , heads: Clips.headCount evs
            , capturedMicros: st.nowMicros
            , source: "odonus"
            , name: nm
            , tags: case hsum of
                Just h -> [ "odonus", h.root, h.scale ]
                Nothing -> [ "odonus" ]
            , notes: ""
            , bpm: Just st.clockTempo
            , key: map (\h -> h.root <> " " <> h.scale) hsum
            , context: Just m.patch
            }
      H.modify_ \s -> s { clips = [ clip ] <> s.clips }
      persistClips
  PlayClip i -> startClip i
  -- Rename a captured clip in place (commits on blur); persist to the shared store.
  RenameClip i nm -> do
    H.modify_ \s -> s { clips = fromMaybe s.clips (modifyAt i (_ { name = nm }) s.clips) }
    persistClips
  DeleteClip i -> do
    hushReplayVoices
    H.modify_ \s -> s { clips = fromMaybe s.clips (deleteAt i s.clips)
                      , playing = case s.playing of
                          Just p | p.source == FromClip i -> Nothing
                          _ -> s.playing }
    persistClips
  ToggleContext -> H.modify_ \s -> s { contextOpen = not s.contextOpen }
  -- STEP LENGTH is a transport/clock param, not a SimState edit, so it rides its
  -- own `reef-steplen` verb (not the tick-tagged input path): apply locally, then
  -- tell the BEAM voice the new model-step length so it steps at the same rate and
  -- keeps the same model-step numbering (lockstep P4c).
  SetStepDiv d -> do
    H.modify_ \s -> s { stepDiv = d }
    st <- H.get
    sendStepLen st
  KnobDown target startVal -> do
    sid <- setupDrag
    H.modify_ _ { dragging = Just { target, startY: 0, startVal, curVal: startVal }, dragSub = Just sid }
  DragMove clientY -> do
    st <- H.get
    case st.dragging of
      Just drag
        | drag.startY == 0 ->
            H.modify_ _ { dragging = Just drag { startY = clientY } }
        | otherwise -> do
            let
              -- cell.note is a raw knob now (0 .. knobMax); the pipeline equal-maps
              -- it over the scale, so the range is fixed, not the set cardinality.
              r = case drag.target of
                    CellNote _ -> { lo: 0, hi: M.knobMax }
                    _ -> targetRange drag.target
              delta = round (toNumber (drag.startY - clientY) * toNumber (r.hi - r.lo) / 140.0)
              newVal = clamp r.lo r.hi (drag.startVal + delta)
            -- Apply locally for responsive knob feel; remember the live value so
            -- DragEnd can broadcast the settled value to the rig (lockstep P4c).
            case drag.target of
              GenRate kind -> H.modify_ \s -> s { gen = setRate kind newVal s.gen }
              GenAmt kind -> H.modify_ \s -> s { gen = setAmt kind newVal s.gen }
              SwingAmt -> H.modify_ \s -> s { swing = toNumber newVal / 100.0 }
              VelHuman -> H.modify_ \s -> s { velHumanize = newVal }
              _ -> H.modify_ \s -> s { odo = applyTarget drag.target newVal s.odo }
            H.modify_ \s -> s { dragging = map (_ { curVal = newVal }) s.dragging }
      _ -> pure unit
  DragEnd -> do
    st <- H.get
    case st.dragSub of
      Just sid -> H.unsubscribe sid
      Nothing -> pure unit
    -- Sync the settled knob value on release (lockstep P4c): a Set* input for the
    -- final value, deferred + broadcast so the rig jumps to the same value on the
    -- same step. Skipped for a bare click (no drag). SWING is the exception: it's
    -- expression (never a tick-tagged model input, so targetToInput returns Nothing)
    -- but it DOES shift the audible onset, so the rig must know it to render the same
    -- groove — it rides its own `reef-swing` verb (like reef-steplen), sent
    -- immediately on release, not deferred. Humanise stays fully local (velocity only,
    -- undetectable in timing).
    for_ st.dragging \drag ->
      when (drag.startY /= 0) case drag.target of
        SwingAmt -> sendSwing st
        _ -> for_ (targetToInput drag.target drag.curVal) enqueue
    H.modify_ _ { dragging = Nothing, dragSub = Nothing }
  ToggleGen kind -> do
    -- Same double-dispatch guard as the panels: a flip-toggle would cancel itself
    -- if the 30fps re-render replays the click, so debounce per source. On the real
    -- click, DEFER + broadcast (lockstep P4c) — gen config drives generation, so it
    -- must flip on the same step on both runtimes.
    st <- H.get
    let k = "g:" <> genLabel kind
    unless (tapBounced k st) do
      H.modify_ (markTap k)
      enqueue (RI.ToggleGen kind)
  -- Freeze / thaw ALL generation. Deferred-on-both like ToggleGen (so both runtimes
  -- pause on the same step); debounced against the 30fps re-render double-dispatch.
  -- The generator config is untouched — freezing only gates runGen — so you can
  -- freeze a moment you like and save it before it drifts.
  ToggleFreeze -> do
    st <- H.get
    unless (tapBounced "freeze" st) do
      H.modify_ (markTap "freeze")
      enqueue (RI.SetFrozen (not st.genFrozen))
  MarblesPad cx cy btns ->
    -- Wired to mousedown + mousemove; act only while the button is held.
    -- X = BIAS (peak's horizontal position in the histogram, low→high notes);
    -- Y = SPREAD, inverted so up = wider.
    when (btns == 1) do
      { x, y } <- liftEffect $ Pointer.padNorm marblesPadId cx cy
      H.modify_ \s -> s { genBias = x, genSpread = 1.0 - y }
  -- MarblesRoll threads the seed → deferred-on-both (as SeedMelody / ChordRoll).
  MarblesRoll -> enqueue RI.RollAllNotes
  -- Pin the PRNG seed to a known value for reproducible golden takes. Local only —
  -- the next Push hands the fixed seed to the rig, so both start from the same
  -- point. Do it while stopped, then Push, then record.
  ReseedTo n -> H.modify_ \s -> s { genSeed = Marbles.seedFrom n }
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
    -- The lockstep HANDOFF (P4d): serialize the WHOLE SimState — Odonus + gen
    -- config + Marbles pad + seed — with the shared reef codec and push it over
    -- the already-open rig WebSocket. The BEAM's reef_voice decodes it with the
    -- SAME codec (Reef.Protocol.decodeSim) and co-simulates from this exact state,
    -- generation and seed included, clock-locked to the same Link beat. Carrying
    -- the seed is what keeps the two runtimes' generative matrices bit-identical.
    st <- H.get
    -- LOCKSTEP (P5): stamp the handoff with the absolute model step this snapshot
    -- is the state FOR — `nextModelStep`, the step the frontend's next Step loop
    -- will emit from exactly this `odo`/`gen`/`genSeed`. The BEAM installs the state
    -- but holds it until that same step (start_sim_at_json sets last_step = N-1), so
    -- both runtimes play the pushed state on the SAME absolute step. That kills the
    -- old flam: `reef-sim` let the BEAM snap to ITS current step (~a model-step / up
    -- to a beat away from the frontend's), which sounded as an echo after every Push.
    -- Carry the model-step LENGTH in the handoff too (`reef-sim-at <step> <beats>
    -- <json>`): `nextModelStep` is numbered in model steps (0.25 × stepDiv beats),
    -- so the BEAM must install that same grid to interpret the step index. Setting
    -- step length and hold-step atomically here means NO follow-up `reef-steplen`
    -- (which would reset the voice's last_step and undo the phase alignment).
    for_ st.binnacle \bin ->
      liftEffect $ Transport.send (Binnacle.socket bin)
        ("reef-sim-at " <> show st.nextModelStep <> " " <> show (stepBeatsOf st) <> " "
           <> encodeSim
                { odo: st.odo, gen: st.gen, spread: st.genSpread, bias: st.genBias, seed: st.genSeed, frozen: st.genFrozen })
    -- A fresh voice starts with swing 0, so re-assert the current swing (its own
    -- verb, no last_step reset — safe right after the handoff).
    sendSwing st
  HushRig -> do
    -- Stop the reef voice (and everything else) on the rig via the existing
    -- hush verb, over the same socket the push used.
    st <- H.get
    for_ st.binnacle \bin ->
      liftEffect $ Transport.send (Binnacle.socket bin) "hush"


-- | REPLAY loop driver (#151, R2b), run each Frame. A WINDOWED scheduler: each
-- | frame it queues only the notes falling in the short lookahead window ahead of
-- | the scheduling watermark — NOT a whole loop iteration at once. So StopPlay
-- | leaves at most `replayLookaheadMs` of notes queued (and StopPlay also sends
-- | all-notes-off), rather than a full loop that keeps sounding. Each note is a
-- | self-contained scheduleNoteAtMs (auto note-off). Also advances the 0..1
-- | playhead for the view. No-op when idle.
driveReplay :: forall o m. MonadAff m => H.HalogenM State Action Slots o m Unit
driveReplay = do
  st <- H.get
  case st.playing of
    Nothing -> pure unit
    Just ps -> do
      nowMs <- liftEffect Time.perfNow
      let
        loopLenMs = max 1.0 (ps.lenMicros / 1000.0)
        horizon = nowMs + replayLookaheadMs
      -- Queue each note's NEXT occurrence after the watermark, if it lands inside
      -- the window. The loop repeats every loopLenMs, so an event at phase `off`
      -- (its rebased time) sounds at loopStartMs + off + k·loopLenMs; pick the
      -- first k past the watermark. Windows are one frame wide (≪ a loop), so ≤
      -- one hit per event. `ps.events` are rebased to [0, lenMicros).
      liftEffect $ for_ ps.events \e -> do
        let
          off = e.fireUnixMicros / 1000.0
          k = ceil ((ps.scheduledUntilMs - ps.loopStartMs - off) / loopLenMs)
          atMs = ps.loopStartMs + off + toNumber k * loopLenMs
        -- REPLAY fans out exactly as live play does, so a looped phrase drives
        -- the same envelopes and doubles the same way the take did.
        when (atMs > ps.scheduledUntilMs && atMs <= horizon) $
          void $ RO.fanNoteAt st.outs st.routing (RM.SOdonusHead e.headIdx)
            { note: e.pitch, velocity: e.vel, atMs, durMs: e.gateMs }
      H.modify_ \s -> case s.playing of
        Just p ->
          let
            elapsed = nowMs - p.loopStartMs
            frac = (elapsed - toNumber (floor (elapsed / loopLenMs)) * loopLenMs) / loopLenMs
          in s { playing = Just p { scheduledUntilMs = max p.scheduledUntilMs horizon
                                  , playheadFrac = max 0.0 (min 1.0 frac) } }
        Nothing -> s

-- | Bring the clock in state up to now, for an action that reads it. The frame
-- | tick no longer keeps it current while nothing drawn needs it.
freshClock :: forall o m. MonadAff m => H.HalogenM State Action Slots o m Unit
freshClock = do
  st <- H.get
  for_ st.binnacle \bin -> do
    now <- liftEffect $ Clock.unixMicrosNow (Binnacle.clock bin)
    r <- liftEffect $ Clock.read (Binnacle.clock bin)
    H.modify_ _ { nowMicros = now, clockTempo = r.tempo, clockLocked = r.locked, clockBeat = r.beat, clockBar = r.bar }

-- | All-notes-off (CC 123) on the four Odonus head channels — cuts any note the
-- | REPLAY loop left ringing, so StopPlay / leaving REPLAY is instantly silent.
hushReplayVoices :: forall o m. MonadAff m => H.HalogenM State Action Slots o m Unit
hushReplayVoices = do
  st <- H.get
  -- CC 123 on every channel any head is routed to. A hush that reached only the
  -- port a head used to hardcode would leave its other legs ringing.
  liftEffect $ for_ (range 0 3) \h ->
    for_ (RO.resolveLegs st.outs st.routing (RM.SOdonusHead h)) \r -> case r.wire, r.out of
      Just w, Just o -> Midi.sendCC o { channel: w.channel - 1, controller: 123, value: 0 }
      _, _ -> pure unit

-- | Start looping the region stored on mark `i` (shared by PlayRegion and a bare
-- | click at the end of a region drag).
startRegion :: forall o m. MonadAff m => Int -> H.HalogenM State Action Slots o m Unit
startRegion i = do
  st <- H.get
  for_ (st.logbook.marks !! i) \m -> do
    nowMs <- liftEffect Time.perfNow
    -- Watermark starts a hair before the origin so a phase-0 note (off == 0) is
    -- included on the first frame rather than falling on the strict `>` boundary.
    H.modify_ _ { playing = Just
      { source: FromRegion i
      , events: materializeRegion m.from m.to st.logbook, lenMicros: m.to - m.from
      , fromMicros: m.from, toMicros: m.to
      , loopStartMs: nowMs, scheduledUntilMs: nowMs - 1.0, playheadFrac: 0.0 } }

-- | Start auditioning captured clip `i` — same looping scheduler as a region,
-- | but the notes come from the clip (already rebased) and it isn't on the
-- | timeline, so nothing highlights there.
startClip :: forall o m. MonadAff m => Int -> H.HalogenM State Action Slots o m Unit
startClip i = do
  st <- H.get
  for_ (st.clips !! i) \c -> do
    nowMs <- liftEffect Time.perfNow
    H.modify_ _ { playing = Just
      { source: FromClip i
      , events: c.events, lenMicros: c.lenMicros
      , fromMicros: 0.0, toMicros: c.lenMicros
      , loopStartMs: nowMs, scheduledUntilMs: nowMs - 1.0, playheadFrac: 0.0 } }

-- | The timeline's earliest-note origin and total span — the SAME formula the
-- | Replay view uses to lay notes out, so pointer↔time round-trips exactly.
timelineBounds :: State -> { tMin :: Number, span :: Number }
timelineBounds st =
  let evs = st.logbook.live <> concatMap _.events st.logbook.chunks
      tMin = foldl (\a e -> min a e.fireUnixMicros) 1.0e18 evs
      tMax = foldl (\a e -> max a e.fireUnixMicros) 0.0 evs
  in { tMin, span: max 1.0 (tMax - tMin) }

-- | The pointer's recording-time position: its normalised X within the timeline
-- | element mapped over the timeline span.
pointerMicros :: State -> Int -> Int -> Effect Number
pointerMicros st cx cy = do
  { x } <- Pointer.padNorm replayTimelineId cx cy
  let b = timelineBounds st
  pure (b.tMin + x * b.span)

-- | Write a region's bounds onto its mark.
setRegionBounds :: Int -> { from :: Number, to :: Number } -> State -> State
setRegionBounds i b s =
  s { logbook = s.logbook
        { marks = fromMaybe s.logbook.marks
            (modifyAt i (\m -> m { from = b.from, to = b.to }) s.logbook.marks) } }

-- | If mark `i` is the one currently looping, carry a bounds edit onto the live
-- | loop too (re-materialising its notes), so a resize/slide is heard on the next
-- | iteration.
syncPlaying :: Int -> { from :: Number, to :: Number } -> State -> State
syncPlaying i b s = case s.playing of
  Just p | p.source == FromRegion i ->
    s { playing = Just p { fromMicros = b.from, toMicros = b.to
                         , events = materializeRegion b.from b.to s.logbook
                         , lenMicros = b.to - b.from } }
  _ -> s

-- | The captured notes falling inside a [from,to] window, copied out and rebased
-- | so the earliest is at 0 — a self-contained loop body (the replay scheduler
-- | and a captured clip both read this form).
materializeRegion :: Number -> Number -> Logbook -> Array NoteEvent
materializeRegion from to lb =
  map (\e -> e { fireUnixMicros = e.fireUnixMicros - from })
    (filter (\e -> e.fireUnixMicros >= from && e.fireUnixMicros <= to)
      (lb.live <> concatMap _.events lb.chunks))

-- | Schedule the next loop iteration this far before its onset (perf ms).
replayLookaheadMs :: Number
replayLookaheadMs = 120.0

-- | True if this target was just toggled (< 120ms ago) — the second of a
-- | double-dispatched click. nowMicros advances via the Frame loop. Shared by
-- | the accordion panels and the GENERATE source LEDs.
tapBounced :: String -> State -> Boolean
tapBounced k s = k == s.lastTap && (s.nowMicros - s.lastTapMicros) < 120000.0

markTap :: String -> State -> State
markTap k s = s { lastTap = k, lastTapMicros = s.nowMicros }

-- | Lockstep (P4c): defer a synced gesture instead of applying it now. Tag it for
-- | `soundingStep + inputBufferSteps` — far enough ahead to clear the BEAM voice's
-- | scheduling lookahead — enqueue it locally (the Step loop applies it on that
-- | step, via `Reef.Input.applyInput`), and broadcast the SAME tick-tagged input to
-- | the rig, where reef_voice applies it on the same step. Both runtimes evolve
-- | identically through the edit — the flam-free property survives live editing.
-- | When the rig isn't attached the broadcast is skipped; the local queue still
-- | applies it, so the standalone webapp behaves the same (just quantized to the
-- | grid instead of instant).
enqueue :: forall o m. MonadAff m => RI.Input -> H.HalogenM State Action Slots o m Unit
enqueue input = do
  st <- H.get
  -- STOPPED: the Step loop is gated on `sounding /= Silent`, so a deferred edit
  -- would sit in `pending` forever and the surface would be inert until playback.
  -- With nothing sounding there's no flam to avoid, so apply the edit NOW. The
  -- resulting setup state is exactly what the Push handoff ships to the rig, so
  -- lockstep starts from what you built while stopped.
  if st.sounding == Silent
    then H.modify_ \s ->
      let sim = RI.applyInputs [ input ]
                  { odo: s.odo, gen: s.gen, spread: s.genSpread, bias: s.genBias
                  , seed: s.genSeed, frozen: s.genFrozen }
      in s { odo = sim.odo, gen = sim.gen, genSpread = sim.spread, genBias = sim.bias
           , genSeed = sim.seed, genFrozen = sim.frozen }
    else do
      let tagStep = soundingStep st + inputBufferSteps
      -- The local model always buffers the edit (SOLO plays it locally); only the
      -- send to the rig is gated on ATLANTIS (onRig = not audible), so SOLO is silent
      -- to the rig. In ATLANTIS the handoff created the voice and these stream to it.
      H.modify_ \s -> s { pending = s.pending <> [ { step: tagStep, input } ] }
      when (st.sounding == Rig) $ for_ st.binnacle \bin ->
        liftEffect $ Transport.send (Binnacle.socket bin)
          ("reef-input " <> encodeTagged { tick: tagStep, input })

-- | Apply a Twister edit for RESPONSIVE feel. Unlike `enqueue` — which, while
-- | playing, DEFERS the input to a near-future model step to stay flam-free with the
-- | rig — this applies it to local state IMMEDIATELY, exactly as the on-screen knob
-- | does mid-drag. That removes the step-grid lag ("slow polling") AND fixes the
-- | boolean grids: the next message reads the freshly-applied state, so a crossing
-- | toggles once instead of thrashing on stale reads. On ATLANTIS it still broadcasts
-- | the tagged input so the rig converges, landing the edit on-grid (rig stays
-- | flam-free; only the local view runs ahead, as it already does under a knob drag).
twisterApply :: forall o m. MonadAff m => RI.Input -> H.HalogenM State Action Slots o m Unit
twisterApply input = do
  st <- H.get
  H.modify_ \s ->
    let sim = RI.applyInputs [ input ]
                { odo: s.odo, gen: s.gen, spread: s.genSpread, bias: s.genBias
                , seed: s.genSeed, frozen: s.genFrozen }
    in s { odo = sim.odo, gen = sim.gen, genSpread = sim.spread, genBias = sim.bias
         , genSeed = sim.seed, genFrozen = sim.frozen }
  when (st.sounding == Rig) $ for_ st.binnacle \bin ->
    liftEffect $ Transport.send (Binnacle.socket bin)
      ("reef-input " <> encodeTagged { tick: soundingStep st + inputBufferSteps, input })

-- | Tell the BEAM voice the current model-step length in beats (lockstep P4c). A
-- | no-op when the rig isn't attached. Sent on Push and on every STEP LENGTH change
-- | so reef_voice's grid tracks the frontend's — otherwise the BEAM keeps stepping
-- | at 1/16 while the frontend steps coarser, and the two desync.
sendStepLen :: forall o m. MonadAff m => State -> H.HalogenM State Action Slots o m Unit
sendStepLen st =
  when (st.sounding == Rig) $ for_ st.binnacle \bin ->
    liftEffect $ Transport.send (Binnacle.socket bin)
      ("reef-steplen " <> show (stepBeatsOf st))

-- | Tell the BEAM voice the current swing fraction (lockstep P4f render stage 2). A
-- | no-op when the rig isn't attached. Swing lags the odd model steps by
-- | `swing × stepMs` on the audible onset; the rig applies the SAME shift to the same
-- | absolute-step parity so the groove renders identically. Sent on Push (fresh voice
-- | defaults to 0) and on the swing knob's release. Not tick-tagged — swing is timing
-- | expression, not model state, so it never enters the deterministic SimState.
sendSwing :: forall o m. MonadAff m => State -> H.HalogenM State Action Slots o m Unit
sendSwing st =
  when (st.sounding == Rig) $ for_ st.binnacle \bin ->
    liftEffect $ Transport.send (Binnacle.socket bin)
      ("reef-swing " <> show st.swing)

-- | Map a settled knob (target + final value) to the `Reef.Input` that sets it,
-- | for the DragEnd broadcast (lockstep P4c). Every knob setter is an ABSOLUTE,
-- | idempotent set (`fanOffsets n` → offset = i·n, etc.), so replaying the final
-- | value on the BEAM lands exactly where the frontend's drag settled. `Nothing`
-- | for the two local-only knobs (swing / velocity-humanise): those are expression
-- | that never leaves this runtime, like the humanise draws in the Step loop.
targetToInput :: KnobTarget -> Int -> Maybe RI.Input
targetToInput t v = case t of
  CellNote i -> Just (RI.SetNote i v)
  CellDur i -> Just (RI.SetCellDur i v)
  CellRatchet i -> Just (RI.SetCellRatchet i v)
  CellVel i -> Just (RI.SetCellVel i v)
  HeadDir h -> Just (RI.SetHeadDir h v)
  HeadSpeed h -> Just (RI.SetHeadSpeedIx h v)
  HeadTransp h -> Just (RI.SetHeadTransp h v)
  HeadOffset h -> Just (RI.SetHeadOffset h v)
  HeadLen h -> Just (RI.SetHeadLen h v)
  HeadDiv h -> Just (RI.SetHeadPulses h v)
  HeadEStep h -> Just (RI.SetHeadEuclidSteps h v)
  Spread -> Just (RI.SetSpread v)
  GateLen -> Just (RI.SetGatePct v)
  FanOff -> Just (RI.FanOffsets v)
  StaggerLen -> Just (RI.StaggerLengths v)
  HeadSpread -> Just (RI.SpreadOctaves v)
  GenRate kind -> Just (RI.SetRate kind v)
  GenAmt kind -> Just (RI.SetAmt kind v)
  SwingAmt -> Nothing
  VelHuman -> Nothing

-- | The model step currently SOUNDING, off the shared Link clock — the same
-- | quantity reef_voice derives (`trunc(BeatNow / step_beats)`), so a step tagged
-- | here means the same step on the BEAM. Model-step length is `stepBeats × stepDiv`
-- | (STEP LENGTH divides the scheduler's 1/16 grid); the BEAM voice is told the
-- | same length via `reef-steplen`, so both agree on which model step is which at
-- | any step length. (`floor(floor(beat/0.25)/stepDiv) = floor(beat/(0.25·stepDiv))`,
-- | so this matches the Step loop's `tick.index / stepDiv`.)
soundingStep :: State -> Int
soundingStep s = floor (s.clockBeat / stepBeatsOf s)

-- | Model-step length in beats: the scheduler's 1/16 grid times the STEP LENGTH
-- | divider. This is what the BEAM voice must step on to stay in lockstep, sent via
-- | `reef-steplen` on Push and whenever STEP LENGTH changes.
stepBeatsOf :: State -> Number
stepBeatsOf s = gridCfg.stepBeats * toNumber s.stepDiv

-- | How far ahead a synced input is scheduled. Must exceed reef_voice's scheduling
-- | lookahead (LOOKAHEAD_MS = 200ms) in steps, so the broadcast reaches the rig
-- | before it has drained that step: at 120bpm a 1/16 is 125ms, so 2 steps (250ms)
-- | clears it with margin. Fast tempi (≳160bpm) would want a larger buffer.
inputBufferSteps :: Int
inputBufferSteps = 2

-- | The rig WebSocket (purerl-tidal). Binnacle subscribes to the Link
-- | anchor here and relays gates/CV to es9-daemon.
rigUrl :: String
rigUrl = "ws://127.0.0.1:3012/ws"

-- | One Odonus step = a 16th note; poll at 25ms. Lookahead 180ms sits in the gap
-- | ABOVE the render-pipeline latency (~130-150ms, so absolute-scheduled notes are
-- | committed before their onset and fire on time) and BELOW the deferred-input
-- | buffer (inputBufferSteps × step ≈ 250ms @120bpm — a live edit must still be
-- | queued before the Step loop reaches its tagged step, so lookahead < that).
gridCfg :: Scheduler.GridConfig
gridCfg = { stepBeats: 0.25, lookaheadMs: 180.0, tickMs: 25 }

-- | The per-head legato state machine for one emitted note. `prev` is the note
-- | currently held on this head's channel (from a previous glide), if any.
-- |   glide + same pitch  → tie: leave the held note ringing (no retrigger)
-- |   glide + diff pitch  → slide: porta-on, note-on new, note-off old (overlap)
-- |   glide + nothing held → start a held note (no auto-off)
-- |   no glide             → gated note: end any held note, then a roll of
-- |                          `ratchet` retriggers filling `gateMs` (1 = a single
-- |                          hit; the old behaviour). Glide and ratchet don't mix
-- |                          (a slide is a single sustained event).
-- | `atMs` is the ABSOLUTE performance.now onset for this note (from the
-- | scheduler's `firePerfMs`, + swing). Every event is scheduled at an absolute
-- | timestamp so Web MIDI fires it on the beat regardless of how long the Halogen
-- | pipeline took to reach here — this is what keeps the frontend monitor locked to
-- | the backend rather than trailing it by the render latency.
emitNote
  :: RO.Outs -> RM.Table -> Int -> Number -> Number -> Int -> Maybe Int -> M.Fired
  -> Effect Unit
emitNote outs tbl headIdx atMs gateMs vel prev f =
  for_ (RO.resolveLegs outs tbl (RM.SOdonusHead headIdx)) \r ->
    case r.wire, r.out of
      Just w, Just o
        -- Checked BEFORE `carriesLine`, because a Rample is neither a line nor
        -- an ordinary trigger: its pitch does not travel in the note at all.
        | Just rp <- w.rample -> rample o (w.channel - 1) rp (atMs + r.leg.offsetMs)
        | RM.carriesLine r.leg.dest -> line o (w.channel - 1) (atMs + r.leg.offsetMs)
        | otherwise -> trigger o (w.channel - 1) (fromMaybe p w.noteOverride) (atMs + r.leg.offsetMs)
      _, _ -> pure unit
  where
  p = f.pitch
  -- A TRIGGER leg (an FH-2 envelope or gate): fired once, at the note's velocity
  -- and for its gate length, so a sustaining envelope tracks the gate rather than
  -- running on its own. Velocity is carried because `velDepth` is the ONLY
  -- per-note expression this path has — the shape itself is config.
  --
  -- Ratchets are deliberately not subdivided here: retriggering an envelope once
  -- per ratchet is a different musical decision from the one the grid recorded.
  trigger o ch note t =
    Midi.scheduleNoteAtMs o { channel: ch, note, velocity: vel, atMs: t, durMs: gateMs }
  -- A RAMPLE leg: the pitch becomes a start-point CC ahead of the note, and the
  -- note is only the trigger. `Reef.Rample` owns the arithmetic — the same
  -- module purerl-tidal's sink calls — so the browser and the BEAM cannot
  -- disagree about which slice a pitch is.
  --
  -- A pitch the card does not hold is DROPPED, not clamped: a silently
  -- transposed note is harder to notice than a missing one.
  --
  -- Unlike an envelope, ratchets ARE subdivided here. Retriggering a sample is
  -- exactly what a ratchet means on a sample player, and it costs one extra
  -- note per hit, not one extra CC — the slice does not change between them.
  -- A tie is honoured for the same reason it is on a line: the player asked for
  -- the slice to keep ringing.
  rample o ch rp t =
    let held = case prev, f.glide of
          Just q, true | q == p -> true
          _, _ -> false
        layer = { velocity: Nothing, slots: rp.slots
                , pitchOfSlot0: Just rp.pitchOfSlot0, slotPitches: Nothing }
        rat = if f.ratchet < 1 then 1 else f.ratchet
        sub' = gateMs / toNumber rat
    in if held then pure unit
       else case Rample.slotFor layer p of
         -- SAID, not swallowed. A pitch the card does not hold is dropped
         -- rather than transposed, and a drop nobody reports is indistinguishable
         -- from the module declining to retrigger — which is the exact
         -- confusion this warning exists to end.
         Nothing ->
           Console.warn $ "[rample] v" <> show rp.voice <> " dropped note " <> show p
             <> ": outside the card (" <> show rp.pitchOfSlot0 <> ".."
             <> show (rp.pitchOfSlot0 + rp.slots - 1) <> ")"
         Just slot -> do
           Midi.sendCCAtMs o
             { channel: ch
             , controller: Rample.startCC rp.voice
             , value: Rample.ccForSlot slot rp.slots
             , atMs: t - toNumber rp.settleMs
             }
           for_ (range 0 (rat - 1)) \k ->
             Midi.scheduleNoteAtMs o
               { channel: ch, note: rp.trigger, velocity: vel
               , atMs: t + toNumber k * sub'
               , durMs: if rat <= 1 then gateMs else sub' * 0.85 }
  -- A LINE leg: the full legato state machine (tie / slide / gated + ratchet).
  line o ch t =
    let portaOn = do
          Midi.sendCC o { channel: ch, controller: 65, value: 127 }
          Midi.sendCC o { channel: ch, controller: 5, value: 40 }
        portaOff = Midi.sendCC o { channel: ch, controller: 65, value: 0 }
        rat = if f.ratchet < 1 then 1 else f.ratchet
        ratchetNote =
          if rat <= 1 then Midi.scheduleNoteAtMs o { channel: ch, note: p, velocity: vel, atMs: t, durMs: gateMs }
          else
            let sub = gateMs / toNumber rat
            in for_ (range 0 (rat - 1)) \k ->
                 Midi.scheduleNoteAtMs o
                   { channel: ch, note: p, velocity: vel
                   , atMs: t + toNumber k * sub, durMs: sub * 0.85 }
    in case prev, f.glide of
      Just q, true | q == p -> pure unit                         -- tie
      Just q, true -> do                                          -- slide
        portaOn
        Midi.noteOnAtMs o { channel: ch, note: p, velocity: vel, atMs: t }
        Midi.noteOffAtMs o { channel: ch, note: q, atMs: t + 60.0 }
      Just q, false -> do                                         -- gated, end held
        Midi.noteOffAtMs o { channel: ch, note: q, atMs: t }
        portaOff
        ratchetNote
      Nothing, true -> do                                         -- start held
        portaOff
        Midi.noteOnAtMs o { channel: ch, note: p, velocity: vel, atMs: t }
      Nothing, false -> do                                        -- gated
        portaOff
        ratchetNote

-- | Note-off one head's held note on EVERY leg it was started on.
noteOffEverywhere :: RO.Outs -> RM.Table -> Int -> Int -> Effect Unit
noteOffEverywhere outs tbl h n =
  for_ (RO.resolveLegs outs tbl (RM.SOdonusHead h)) \r -> case r.wire, r.out of
    Just w, Just o ->
      -- A Rample's note is its TRIGGER, never the pitch. Sending the pitch here
      -- would be a note-off for a note that was never started, and leave the
      -- trigger that WAS started still held.
      let note = case w.rample of
            Just rp -> rp.trigger
            Nothing -> n
      in Midi.noteOffAt o { channel: w.channel - 1, note, delayMs: 0.0 }
    _, _ -> pure unit

-- | Note-off every held note (e.g. on Stop) and clear the held-note table.
-- | Routed, like the note-ons: a held note must be released on every leg it was
-- | started on. Releasing only the port a head used to hardcode would leave a
-- | second destination droning — the shape "stopping doesn't stop anything" is
-- | made of.
silenceHeld :: RO.Outs -> RM.Table -> Array (Maybe Int) -> Effect Unit
silenceHeld outs tbl held =
  forWithIndex_ held \h mn -> case mn of
    Just n -> noteOffEverywhere outs tbl h n
    Nothing -> pure unit

-- | MIDI output port (substring match). On macOS enable the IAC Driver in
-- | Audio MIDI Setup and receive this bus in Ableton; each head sends on
-- | its own channel (I→1 … IV→4). (The es9 modular path lives in
-- | Binnacle.Output for when the rig is patched.)
midiPortName :: String
midiPortName = "IAC"

-- | The FH-2's own USB MIDI port (substring match) — the envelope destination.
-- | Its polyenv envelopes listen on MIDI channels 1..8 of THIS port, which is why
-- | a head's envelope trigger cannot ride the note's port: same channel number,
-- | different device, different meaning. Matches the needle fh2-config uses.
-- |
-- | Hardcoded here only until step 2 of docs/DESIGN-routing.md makes the router's
-- | rows editable; then it becomes the head row's destination like any other.
envPortName :: String
envPortName = "FH-2"

-- | The MidiFighter Twister presents a MIDI port whose name contains this.
twisterInputName :: String
twisterInputName = "Twister"

-- | Factory-default Twister map: encoder ROTATE arrives as CC on MIDI channel 1
-- | (0-based 0), the encoder PUSH switch as CC on channel 2 (0-based 1). Set the
-- | Twister to match in the MidiFighter Utility — encoders ABSOLUTE, switches CC.
twisterRotateCh :: Int
twisterRotateCh = 0

twisterPushCh :: Int
twisterPushCh = 1

-- | Map an absolute 0..127 encoder onto an integer range [lo,hi].
twisterScale :: Int -> Int -> Int -> Int
twisterScale lo hi d2 = lo + round (toNumber d2 / 127.0 * toNumber (hi - lo))

-- | PUSH switch → active grid. Row 1 (enc 0..3) = the four cell VALUE grids; row 2
-- | (enc 4..7) = the three cell BOOLEAN grids + the MACRO pane. Other pushes are
-- | free (future banks). The "dedicated selector pushes" scheme.
twisterFieldForPush :: Int -> Maybe TwisterField
twisterFieldForPush = case _ of
  0 -> Just FNote
  1 -> Just FLen
  2 -> Just FRatchet
  3 -> Just FVel
  4 -> Just FGate
  5 -> Just FSkip
  6 -> Just FGlide
  7 -> Just FMacro
  _ -> Nothing

-- | A value-grid rotary: 0..127 → [lo,hi] → the same Set* input the knob emits.
valueRotary
  :: forall o m. MonadAff m
  => KnobTarget -> Int -> Int -> Int -> H.HalogenM State Action Slots o m Unit
valueRotary tgt lo hi d2 = for_ (targetToInput tgt (twisterScale lo hi d2)) twisterApply

-- | The MACRO grid: the 16 rotaries drive the Notes-pane globals (not the cells).
-- | Layout, row-major over the 4×4:
-- |   0 octave   1 degree    2 marbles-X  3 marbles-Y
-- |   4 gen-on   5 depth     6 rate       7 roll
-- |   8 step-div 9 gate%    10 swing     11 humanise
-- | Continuous knobs map absolute; gen-on crosses at the midpoint; roll is a
-- | one-shot per turn (debounced). Positions 12..15 are unused for now.
twisterMacro
  :: forall o m. MonadAff m
  => Int -> Int -> State -> H.HalogenM State Action Slots o m Unit
twisterMacro cell d2 st = case cell of
  0 -> twisterApply (RI.SetOctaveShift (twisterScale (-2) 2 d2))
  1 -> twisterApply (RI.SetDegShift (twisterScale 0 6 d2))
  2 -> H.modify_ _ { genBias = toNumber d2 / 127.0 }
  3 -> H.modify_ _ { genSpread = toNumber d2 / 127.0 }
  -- Generation on/off for the NOTES source specifically (not global freeze): fire
  -- the toggle only when the source disagrees with the knob (right ≥64 = on).
  4 -> for_ (find (\g -> g.kind == GNotes) st.gen) \g ->
         when (g.on /= (d2 >= 64)) (twisterApply (RI.ToggleGen GNotes))
  5 -> twisterApply (RI.SetAmt GNotes (twisterScale 0 100 d2))
  6 -> twisterApply (RI.SetRate GNotes (twisterScale 0 rateMax d2))
  7 -> unless (tapBounced "tw-roll" st) do
         H.modify_ (markTap "tw-roll")
         twisterApply RI.RollAllNotes
  8 -> do
         H.modify_ _ { stepDiv = twisterScale 1 16 d2 }
         H.get >>= sendStepLen
  9 -> enqueue (RI.SetGatePct (twisterScale 10 200 d2))
  10 -> do
         H.modify_ _ { swing = toNumber (twisterScale 0 60 d2) / 100.0 }
         H.get >>= sendSwing
  11 -> H.modify_ _ { velHumanize = twisterScale 0 40 d2 }
  _ -> pure unit

-- | Bank 2 rotary: each ROW is a voice (head 0..3); the four columns are
-- | pattern / euclid-k / euclid-n / transpose. `idx` is the 0..15 grid position.
twisterVoice :: forall o m. MonadAff m => Int -> Int -> H.HalogenM State Action Slots o m Unit
twisterVoice idx d2 =
  let voice = idx `div` 4
      knob = idx `mod` 4
  in case knob of
    0 -> twisterPattern voice d2
    1 -> valueRotary (HeadDiv voice) 0 16 d2          -- Euclidean k (pulses)
    2 -> valueRotary (HeadEStep voice) 1 16 d2         -- Euclidean n (steps)
    3 -> valueRotary (HeadTransp voice) (-24) 24 d2    -- per-voice transpose
    _ -> pure unit

-- | The pattern knob: access patterns are cycle-only in the model (no absolute
-- | setter), so map the absolute encoder to a target index and CYCLE the wrapping
-- | ring up to it — the same `CyclePattern` input the on-screen button uses,
-- | applied the needed number of times (Euclidean mod, so it wraps cleanly).
twisterPattern :: forall o m. MonadAff m => Int -> Int -> H.HalogenM State Action Slots o m Unit
twisterPattern voice d2 = do
  st <- H.get
  let nPat = length M.patternLibrary
  for_ (st.odo.heads !! voice) \h -> do
    let desired = twisterScale 0 (nPat - 1) d2
        delta = ((desired - h.patternIx) `mod` nPat + nPat) `mod` nPat
    for_ (replicate delta unit) \_ -> twisterApply (RI.CyclePattern voice)

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

setupDrag :: forall o m. MonadAff m => H.HalogenM State Action Slots o m H.SubscriptionId
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

-- | Like `setupDrag`, but for a REPLAY region drag: emits `RegionMove` with BOTH
-- | pointer coords (padNorm needs clientX + clientY) and `RegionUp` on release.
setupRegionDrag :: forall o m. MonadAff m => H.HalogenM State Action Slots o m H.SubscriptionId
setupRegionDrag =
  H.subscribe $ HS.makeEmitter \emit -> do
    moveFn <- eventListener \e -> case ME.fromEvent e of
      Just me -> emit (RegionMove (ME.clientX me) (ME.clientY me))
      Nothing -> pure unit
    upFn <- eventListener \_ -> emit RegionUp
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
render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render s =
  HH.div
    -- The whole surface is non-selectable: knob drags and toggle/matrix clicks
    -- never start a text selection. Content fills it at full height; the stage
    -- switch is in the nav (`stageTabs`), not floating over the surface.
    [ style $ "position:fixed;top:var(--tf-bar);left:0;right:0;bottom:0;overflow:hidden;"
        <> "user-select:none;-webkit-user-select:none;"
        <> "background:#b7b1a0;font-family:Georgia,serif" ]
    [ navBar s
    , case s.stage of
        -- KEY carries the SCENES song machinery in one merged column (#139); it now
        -- sits at the RHS so the working order reads Scope · Playheads · Odonus ·
        -- Generate · Key (the source/song settings live to the right of the grid).
        Perform ->
          HH.div
            [ style "height:100%;display:flex;align-items:stretch;overflow-x:auto;overflow-y:hidden" ]
            [ scopePanel s
            , playheadsPanel s
            , gridPanel s
            , generatePanel s
            , cellParamsPanel s
            ]
        Review -> replayPanel s
    ]

-- | Every polyphonic instrument the rack can drive, with whichever calibration
-- | tables the Amphora fetch turned up.
-- |
-- | The one place that says which instruments exist, which labels correct them,
-- | and — via `Poly.saichRig` / `Poly.ringsRig` — where they are patched. Called
-- | with `[]` before the fetch returns, which yields the same instruments
-- | playing uncorrected rather than no instruments at all.
polyInit :: Array Amphora.LibItem -> Array PolyInst
polyInit items =
  [ { inst: RM.Saich
    , rig: Poly.saichRig (Poly.tablesFor [ "saich-1", "saich-2", "saich-3", "saich-4" ] items)
    , voices: RV.empty RV.saich
    }
  -- One table, because Rings presents one pitch input however many voices it
  -- holds. Its correction is its CV input's own error, which every note shares.
  , { inst: RM.Rings
    , rig: Poly.ringsRig (Poly.tablesFor [ "rings-1" ] items)
    , voices: RV.empty RV.rings
    }
  ]

-- | Run one step's notes through ONE instrument's allocator.
-- |
-- | Retires elapsed notes FIRST, so a note arriving this step can take a voice
-- | that just freed up rather than being dropped as overflow against a stale
-- | picture. The seating policy is re-read from the routing table each step, so
-- | toggling it in the router takes effect without a reload.
playPoly
  :: RM.Table
  -> Array { headIdx :: Int, pitch :: Int, gateMs :: Number }
  -> Number
  -> PolyInst
  -> { poly :: PolyInst, emits :: Array RV.Emit }
playPoly tbl notes atMs p =
  let
    seated = p.voices { inst = (Poly.withOrder (polyOrder tbl p.inst) p.rig).inst }
    expired = RV.expireAt atMs seated
    mine = filter (\n -> headGoesPoly tbl p.inst n.headIdx) notes
    step acc n =
      let r = RV.noteOn atMs n.pitch n.gateMs acc.voices
      in { voices: r.voices, emits: acc.emits <> r.emits }
    played = foldl step { voices: expired.voices, emits: expired.emits } mine
  in
    { poly: p { voices = played.voices }, emits: played.emits }

-- | Whether this head has a live leg into THIS instrument. A head can route to
-- | MIDI and to a poly instrument at once — that is the point of fan-out — so
-- | this is a filter on the poly pass, not an alternative to the MIDI one, and
-- | it is per-instrument because two of them allocate independently.
headGoesPoly :: RM.Table -> RM.InstrumentId -> Int -> Boolean
headGoesPoly tbl inst h =
  any isMine (filter _.on (RM.legsFor tbl (RM.SOdonusHead h)))
  where
  isMine lg = case lg.dest of
    RM.DPoly d -> d.inst == inst
    _ -> false

-- ---------------------------------------------------------------------------
-- The Rample as ONE instrument
-- ---------------------------------------------------------------------------

-- | The Rample's settle, taken from the profile that measured it rather than
-- | restated here — 40 ms between the start-point CC and the trigger.
rampleSettleMs :: Number
rampleSettleMs = case RV.rample.silencing of
  RV.PerVoiceStrike ps -> ps.settleMs
  _ -> 40.0

type RampleCfg =
  { port :: String
  , channel :: Int
  , triggers :: Array Int
  , slots :: Int
  , pitchOfSlot0 :: Int
  }

-- | The Rample-as-instrument route, if the table has a live one.
-- |
-- | There is ONE allocator, so there is one configuration: if two legs
-- | disagreed they could not each be right, and the first live one winning is
-- | at least a rule. Same reconciliation argument as `polyOrder`.
ramplePolyOf :: RM.Table -> Maybe RampleCfg
ramplePolyOf tbl = head (mapMaybe pick (filter _.on (concatMap _.legs tbl)))
  where
  pick lg = case lg.dest of
    RM.DRamplePoly d -> Just d
    _ -> Nothing

-- | The card's layout as `Reef.Rample` wants it. A chromatic run from
-- | `pitchOfSlot0`; a card in some other order would carry `slotPitches`.
rampleLayer :: RampleCfg -> Rample.Layer
rampleLayer d =
  { velocity: Nothing, slots: d.slots
  , pitchOfSlot0: Just d.pitchOfSlot0, slotPitches: Nothing }

-- | Whether this head has a live leg into the Rample-as-instrument. A head can
-- | route here AND to plain MIDI at once, so this filters the Rample pass
-- | rather than replacing the MIDI one.
headGoesRamplePoly :: RM.Table -> Int -> Boolean
headGoesRamplePoly tbl h =
  any isMine (filter _.on (RM.legsFor tbl (RM.SOdonusHead h)))
  where
  isMine lg = case lg.dest of
    RM.DRamplePoly _ -> true
    _ -> false

-- | Hand this step's notes to the allocator and report what it decided.
-- |
-- | Notes the card cannot play are refused BEFORE allocation, not after: a
-- | pitch with no slice must not consume a voice, and it must not be allowed to
-- | reach the trigger half on its own — a trigger whose start-point CC was
-- | dropped plays the PREVIOUS slice, which is a wrong note rather than a
-- | missing one.
-- |
-- | `Reef.Voices` emits `Pitch` at the moment it is given and `Trigger` a
-- | settle later, so the pass runs a settle EARLY to land the trigger on the
-- | beat. Velocity is carried alongside because `RV.Emit` has no room for it
-- | and the Rample needs it: velocity is what picks the dynamic layer.
playRample
  :: RampleCfg
  -> RM.Table
  -> Array { headIdx :: Int, pitch :: Int, gateMs :: Number, vel :: Int }
  -> Number
  -> RV.Voices
  -> { voices :: RV.Voices
     , emits :: Array { emit :: RV.Emit, vel :: Int, gateMs :: Number }
     , refused :: Array Int
     }
playRample cfg tbl notes atMs v0 =
  let
    mine = filter (\n -> headGoesRamplePoly tbl n.headIdx) notes
    split = partition (\n -> isJust (Rample.slotFor (rampleLayer cfg) n.pitch)) mine
    -- Emits nothing for a struck instrument: a sample rings out on its own
    -- envelope, so this only frees slots for allocation.
    start = atMs - rampleSettleMs
    -- Expire at the same instant we allocate at, not at `atMs`: freeing a slot
    -- 40 ms early would let a voice be stolen a beat before it was due.
    expired = RV.expireAt start v0
    step acc n =
      let r = RV.noteOn start n.pitch n.gateMs acc.voices
      in { voices: r.voices
         , emits: acc.emits <> map (\e -> { emit: e, vel: n.vel, gateMs: n.gateMs }) r.emits
         }
    played = foldl step { voices: expired.voices, emits: [] } split.yes
  in
    { voices: played.voices, emits: played.emits, refused: map _.pitch split.no }

-- | Render the allocator's decisions as the two messages a Rample hears.
emitRample
  :: RO.Outs -> RampleCfg
  -> Array { emit :: RV.Emit, vel :: Int, gateMs :: Number }
  -> Effect Unit
emitRample outs cfg es = case RO.outFor outs cfg.port of
  Nothing -> pure unit
  Just o -> for_ es \x -> case x.emit.action of
    -- The allocator counts voices from 0; the Rample's panel counts from 1.
    RV.Pitch i pitch -> case Rample.slotFor (rampleLayer cfg) pitch of
      Nothing -> pure unit   -- refused upstream; unreachable
      Just slot -> Midi.sendCCAtMs o
        { channel: cfg.channel - 1
        , controller: Rample.startCC (i + 1)
        , value: Rample.ccForSlot slot cfg.slots
        , atMs: x.emit.atMs
        }
    RV.Trigger i _ -> case cfg.triggers !! i of
      Nothing -> pure unit
      Just n -> Midi.scheduleNoteAtMs o
        { channel: cfg.channel - 1, note: n, velocity: x.vel
        , atMs: x.emit.atMs, durMs: x.gateMs
        }
    -- A struck instrument emits nothing else: no gate to close, no mix to move.
    _ -> pure unit

-- | How one instrument's allocator should seat notes, reconciled across every
-- | route into it.
-- |
-- | ANY live leg asking for pitch order wins. The allocator has one state that
-- | all routes into that instrument share, so they cannot each have their own
-- | answer — and a disjunction is the only reconciliation that does not depend
-- | on which route you happen to read first.
polyOrder :: RM.Table -> RM.InstrumentId -> RV.Order
polyOrder tbl inst =
  if any wants (concatMap _.legs tbl) then RV.ByPitch else RV.Arrival
  where
  wants lg = lg.on && case lg.dest of
    RM.DPoly d -> d.inst == inst && d.sortByPitch
    _ -> false
