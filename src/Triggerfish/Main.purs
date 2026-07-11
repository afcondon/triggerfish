-- | Triggerfish shell. Triggerfish is a rack of direct-manipulation
-- | instruments over the BEAM-native modules; this root holds a small
-- | instrument selector and shows one at a time.
-- |
-- | All four instruments stay MOUNTED at once — the selector only toggles which
-- | is VISIBLE (`display:none` for the rest). So each module keeps playing when
-- | you switch away: start Odonus, switch to Balistes and start it too, and
-- | they run together as a four-module rig jam (each on its own MIDI channel /
-- | CV, so no conflict). With the rig's Link clock all four phase-lock to the
-- | same anchor; in free-run (no rig) they each run at 120 from their own mount
-- | time and can drift, so a tight multi-module jam wants the rig.
-- |
-- | A fifth selector, TIDAL, is not an instrument but a read-only aggregate: it
-- | pulls each module's current source (via the SourceQuery every instrument
-- | answers) and concatenates them into one document — a one-stop copy of the
-- | whole playing surface, for pasting into Calypso or an editor. Because every
-- | module stays mounted, the music keeps playing while you read or copy it.
module Triggerfish.Main where

import Prelude

import Data.Array (any, elem, filter, find, findIndex, length, mapWithIndex, null, replicate, uncons)
import Data.Foldable (for_)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Const (Const)
import Data.Set (Set)
import Data.Set as Set
import Data.Map (Map)
import Data.Map as Map
import Data.Int as Int
import Data.String as String
import Data.String.Common (joinWith)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (attempt)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Timer (setInterval)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Type.Proxy (Proxy(..))
import Binnacle.Audio (armAudioKeepAlive)
import Binnacle.Time (dateNow)
import Triggerfish.Odonus.Grid as Odonus
import Triggerfish.Balistes.Component as Balistes
import Triggerfish.Selene.Component as Selene
import Triggerfish.Sufflamen.Component as Sufflamen
import Triggerfish.Stellatus.Component as Stellatus
import Triggerfish.SourceQuery as SQ
import Triggerfish.Midi.Routing as Routing
import Triggerfish.Amphora as Amphora
import Triggerfish.Macro (Cell(Quiet, Load), Step, ResolvedMod, laneFormNames, parseLane, resolveStep, stepLabel)
import Triggerfish.Scale (rootNames, scaleTypes)
import Vetula.App as Vetula
import Vetula.Clipboard (copyText)
import Triggerfish.Transport (Which(..), Mode(..), Sounding(..), soundingOf, anyArmed, allMachines)

-- One free-run tempo for the whole rack with no rig. (On the rig the forwarded
-- Link anchor overrides it.) A shell BPM control could drive this later.
freeTempo :: Number
freeTempo = 120.0

main :: Effect Unit
main = HA.runHalogenAff do
  liftEffect armAudioKeepAlive   -- keep the tab audible so background play survives
  body <- HA.awaitBody
  void $ runUI root unit body

-- `Which`, `Mode`, and the `Sounding` authority model now live in the pure
-- Triggerfish.Transport module (the MISU core — see docs/DESIGN-transport-misu.md).
-- The shell's entire transport state is `mode :: Mode` + `armed :: Set Which`;
-- each machine's `Sounding` is `soundingOf mode armed w`, pushed via SetSounding.

data RAction
  = Init | SyncTick | PollVetula | Pick Which | RefreshTidal | CopyTidal | ToggleMaster
  | SetMode Mode                -- flip the SOLO⟷ATLANTIS authority
  | ArmTab Which                -- toggle one instrument's ARM from the switcher dot
  | JumpVetula Int              -- nav strip: jump Vetula's progression to a chord (live)
  | LoadFromLib Which Int       -- A5: make a saved entry active in its instrument
  | CopyEntry String            -- copy one entry's eDSL text
  | SetImportText String
  | ImportInto Which            -- route the paste box to one instrument's library
  | SetBinding String String    -- Tidal-page channel map: bind a Vetula voice name → channel
  | PickEntry LibRow            -- workbench: put a shelf entry on the bench
  | ToggleSource               -- workbench: slide the raw-source drawer open/shut
  | ToggleDig                  -- workbench: expand/collapse the full archive
  | ToggleStar LibRow          -- workbench: add/remove a setup from the go-to wall
  | RefreshGoTo                -- workbench: re-fetch the triggerfish-goto collection
  | PreviewEntry LibRow        -- workbench: load + Local-audition a setup (rig untouched)
  | StopPreview                -- workbench: end the preview, restore its sounding
  | VetulaArmed Boolean        -- Vetula's self-arm/disarm EVENT (replaces the poll)
  -- macro-tidal (Slice 1): the arrangement layer on the TIDAL page. One lane of
  -- Odonus scene-names, sequenced over bar-quantized steps.
  | SetMacroText String        -- edit the lane pattern
  | SetMacroBars String        -- edit bars-per-step
  | ToggleMacro                -- run / stop the macro sequencer
  | MacroTick                  -- the bar-quantized clock poll (step-boundary driver)
  | AppendMacroName String     -- palette: append an available scene name to the lane

-- One saved preset gathered from an instrument, for the cross-instrument LIBRARY
-- manager on the TIDAL page. `text` is the entry rendered to Lepidoptera eDSL
-- (the transferable form); `idx` is its position in that instrument's library.
type LibRow = { inst :: Which, idx :: Int, name :: String, text :: String }

-- The shell's ENTIRE transport state (MISU refactor): the authority `mode` plus
-- the set of armed machines. A machine's behaviour is `soundingOf mode armed w`
-- — no separate master/playing/audible/rigOn booleans that can contradict it.
-- "Master playing" is derived (`anyArmed armed`); rig-voice running is derived
-- (`soundingOf … == Rig`) and each instrument edge-detects its own transitions.
type RState =
  { which :: Which, tidalDoc :: String, freeT0 :: Number
  , library :: Array LibRow, importText :: String, importMsg :: String
  -- Workbench (TIDAL page): the shelf entry currently on the bench, whether the
  -- raw-source drawer is slid open, whether the archive ("dig") is expanded, and
  -- the fetched go-to collection (the curated wall — starred setups across every
  -- instrument, matched to shelf rows by payload equality since same payload =
  -- same content hash).
  , picked :: Maybe LibRow, sourceOpen :: Boolean, digOpen :: Boolean
  , goTo :: Array Amphora.LibItem
  -- The setups currently being previewed — one per instrument (an instrument
  -- sounds one setup at a time), each loaded into its editor and forced to Local
  -- sound (the rig + the others untouched). Multiple instruments can audition at
  -- once — that's how you hear a combination — so each gets its own stop control.
  , previewing :: Array LibRow
  -- The authority mode + the live harmonic-context strip shown in the top nav.
  -- `harm` is polled from Vetula: voice-0's bars-per-chord dwell schedule and the
  -- current playhead (-1 = none). Rendered as a glyph visible in every pane.
  , mode :: Mode
  , harm :: { durs :: Array Int, active :: Int, chord :: String }
  -- Vetula auto-resync (ATLANTIS): the shell polls Vetula's rig payload and, when
  -- it settles on a new value, re-pushes (SetSounding Rig re-voices) — so the
  -- progression re-voices live with no manual button. `brushSent` = last value
  -- pushed; `brushPrev` = last poll's value (a one-tick settle coalesces a drag).
  , brushSent :: String
  , brushPrev :: String
  -- The armed set: the single source of truth for the switcher's per-tab dots and
  -- the master button label. Reconciled from the instruments on SyncTick (a machine
  -- can self-disarm, e.g. Vetula unloading a progression).
  , armed :: Set Which
  -- MIDI routing (Tidal-page channel map). `routing` is the shell-owned name →
  -- canonical-channel table, pushed to Vetula (SetRouting); `vetulaNames` is the
  -- set of → midi voice names in use, polled from Vetula so the page can list them.
  , routing :: Map String Int
  , vetulaNames :: Array String
  -- macro-tidal (Slice 1): the Odonus arrangement lane. `macroText` is the
  -- mini-notation pattern of scene-names; `macroBars` = bars per step; `macroOn`
  -- runs the sequencer. `macroStep` is the last GLOBAL step index applied (-1 =
  -- none yet) so we only load at a boundary; `macroCell` is the label last loaded
  -- (for the readout — "" none, "~" a rest, "name ?" an unresolved name).
  , macroText :: String, macroBars :: Int, macroOn :: Boolean
  , macroStep :: Int, macroCell :: String
  -- Harmonic-authority bridge: the last resting-context scale pushed from Vetula
  -- into Odonus (serialised for dedup, so the 100ms poll only re-pushes on change).
  , ctxScaleKey :: String }

type Slots =
  ( odo :: H.Slot SQ.Query Void Unit
  , bal :: H.Slot SQ.Query Void Unit
  , sel :: H.Slot SQ.Query Void Unit
  , vet :: H.Slot Vetula.SourceQuery Vetula.Output Unit
  , suf :: H.Slot (Const Void) Void Unit
  , ste :: H.Slot (Const Void) Void Unit
  )

_odo :: Proxy "odo"
_odo = Proxy

_bal :: Proxy "bal"
_bal = Proxy

_sel :: Proxy "sel"
_sel = Proxy

_vet :: Proxy "vet"
_vet = Proxy

_suf :: Proxy "suf"
_suf = Proxy

_ste :: Proxy "ste"
_ste = Proxy

root :: forall q i o m. MonadAff m => H.Component q i o m
root =
  H.mkComponent
    { initialState: \_ ->
        { which: Bal, tidalDoc: "", freeT0: 0.0
        , library: [], importText: "", importMsg: ""
        , picked: Nothing, sourceOpen: false, digOpen: false, goTo: [], previewing: []
        , mode: Solo, harm: { durs: [], active: -1, chord: "" }
        , brushSent: "", brushPrev: ""
        , armed: Set.empty
        , routing: Map.empty
        , vetulaNames: []
        , macroText: "", macroBars: 4, macroOn: false
        , macroStep: -1, macroCell: "", ctxScaleKey: "" }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Init }
    }

handleAction :: forall o m. MonadAff m => RAction -> H.HalogenM RState RAction Slots o m Unit
handleAction = case _ of
  -- Pick one shared free-run epoch for the rack, then keep re-asserting it on a
  -- slow timer so every (mounted, possibly late-initialised) module shares the
  -- same downbeat. Idempotent; a no-op on any module currently Link-locked.
  Init -> do
    now <- liftEffect dateNow
    H.modify_ _ { freeT0 = now * 1000.0 }
    { emitter, listener } <- liftEffect HS.create
    _ <- H.subscribe emitter
    _ <- liftEffect $ setInterval 1500 (HS.notify listener SyncTick)
    -- Poll Vetula's Odonus-bound performance voices ~10×/s and feed each one's
    -- current block chord to Odonus, so its quantiser follows the live conductor.
    _ <- liftEffect $ setInterval 100 (HS.notify listener PollVetula)
    -- The macro clock: poll ~8×/s and act only when a bar-quantized step boundary
    -- is crossed (MacroTick is a no-op while the sequencer is stopped).
    _ <- liftEffect $ setInterval 120 (HS.notify listener MacroTick)
    handleAction SyncTick
    -- One source of truth: push each machine its derived Sounding (all Silent now —
    -- nothing armed). Arm/mode changes re-derive and re-push; the instruments
    -- edge-detect their own local-mute / rig-handoff transitions.
    pushAll
  -- Master ▶/■ = arm ALL / disarm ALL: arm every machine if none is armed, else
  -- disarm every machine. The button label is `anyArmed`. pushAll re-derives each
  -- machine's Sounding (in ATLANTIS that hands off / stops rig voices too).
  ToggleMaster -> do
    a <- H.gets _.armed
    H.modify_ _ { armed = if anyArmed a then Set.empty else allMachines }
    pushAll
  -- Flip the SOLO⟷ATLANTIS authority. Re-deriving every machine's Sounding IS the
  -- whole transition: an armed rig machine goes Local⟷Rig (its SetSounding handler
  -- hands off on entering Rig, stops its voice on leaving), Selene stays Local,
  -- unarmed stay Silent. No manual handoff/hush ordering to get wrong.
  SetMode m -> do
    H.modify_ _ { mode = m }
    pushAll
  -- The switcher's per-tab play/pause dot: toggle just this machine's arm, then
  -- push its (re-derived) Sounding.
  ArmTab w -> do
    a <- H.gets _.armed
    H.modify_ _ { armed = if Set.member w a then Set.delete w a else Set.insert w a }
    pushSounding w
  -- Nav harmonic strip: jump Vetula's progression to a chord live. Playing → the
  -- ensemble advances there; stopped → the → odo feed moves, re-quantising Odonus.
  JumpVetula i -> void $ H.query _vet unit (Vetula.JumpChord i unit)
  SyncTick -> do
    t0 <- H.gets _.freeT0
    _ <- H.query _odo unit (SQ.SyncFree t0 freeTempo unit)
    _ <- H.query _bal unit (SQ.SyncFree t0 freeTempo unit)
    _ <- H.query _sel unit (SQ.SyncFree t0 freeTempo unit)
    _ <- H.query _vet unit (Vetula.SyncFree t0 freeTempo unit)
    -- No armed-reconcile poll here anymore: `armed` is written only by the user
    -- (ArmTab / ToggleMaster) and by Vetula's self-disarm EVENT (VetulaArmed). Odo/
    -- Bal/Sel never self-disarm, so nothing needs observing. Sounding is now purely
    -- one-directional (shell state → instruments) — no two-way binding to fight.
    pure unit
  -- Opening TIDAL pulls a fresh aggregate + library; the modules keep playing.
  Pick Tid -> do
    H.modify_ _ { which = Tid }
    refreshTidal
    refreshLibrary
    fetchGoTo
  Pick w -> H.modify_ _ { which = w }
  RefreshTidal -> refreshTidal *> refreshLibrary *> fetchGoTo
  CopyTidal -> H.gets _.tidalDoc >>= (liftEffect <<< copyText)
  -- A5 manager: load a saved entry into its instrument, and switch to it so the
  -- change is visible. Copy exports one entry's eDSL text.
  LoadFromLib w i -> do
    _ <- queryLoad w i
    H.modify_ _ { which = w }
  CopyEntry txt -> liftEffect (copyText txt)
  SetImportText t -> H.modify_ _ { importText = t }
  -- Route the paste box to one instrument; it accepts iff the text is one of its
  -- own presets. On success, clear the box and re-gather the library.
  ImportInto w -> do
    txt <- H.gets _.importText
    accepted <- queryImport w txt
    let ok = fromMaybe false accepted
    H.modify_ _
      { importMsg = if ok then "✓ imported into " <> whichName w
                    else "✗ not a valid " <> whichName w <> " preset" }
    when ok do
      H.modify_ _ { importText = "" }
      refreshLibrary
  -- Tidal-page channel map: bind a Vetula voice name → channel (blank/invalid = unbind,
  -- back to the default). Update the shell table, then push it to Vetula.
  SetBinding name v -> do
    case Int.fromString v of
      Just ch | ch >= 1 && ch <= 16 -> H.modify_ \st -> st { routing = Map.insert name ch st.routing }
      _ -> H.modify_ \st -> st { routing = Map.delete name st.routing }
    pushRouting
  -- Workbench: put a shelf entry on the bench (or clear it if re-clicked).
  PickEntry r -> H.modify_ \st ->
    st { picked = if isPicked st.picked r then Nothing else Just r }
  ToggleSource -> H.modify_ \st -> st { sourceOpen = not st.sourceOpen }
  ToggleDig -> H.modify_ \st -> st { digOpen = not st.digOpen }
  RefreshGoTo -> fetchGoTo
  -- Preview toggle: load the setup into its editor and force ONLY that instrument
  -- to Local sound — the rig and every other instrument keep their derived
  -- Sounding, so nothing goes to the modular. Re-clicking the same row stops it.
  -- Several instruments can preview at once (hear a combination); one setup per
  -- instrument, so previewing a second setup on the same instrument swaps it.
  PreviewEntry r -> do
    st <- H.get
    if isPreviewing st r
      then do
        H.modify_ \s -> s { previewing = filter (not <<< sameRow r) s.previewing }
        pushSounding r.inst   -- re-derives its resting Sounding (no longer previewing)
      else do
        _ <- queryLoad r.inst r.idx
        -- one setup per instrument: drop any other row already previewing on it
        H.modify_ \s -> s { previewing = filter (\p -> p.inst /= r.inst) s.previewing <> [ r ] }
        pushSounding r.inst   -- re-derives Local (now in the preview set)
  -- Stop every preview at once (the header "stop all").
  StopPreview -> do
    insts <- H.gets (map _.inst <<< _.previewing)
    H.modify_ _ { previewing = [] }
    for_ insts pushSounding
  -- Vetula self-armed / self-disarmed (its own play/stop/unload). The shell owns
  -- `armed`, so update its membership and re-derive Vetula's Sounding — the event
  -- that replaces the old poll-and-reconcile loop.
  VetulaArmed on -> do
    H.modify_ \st -> st { armed = if on then Set.insert Vet st.armed else Set.delete Vet st.armed }
    pushSounding Vet
  -- macro-tidal: edit the Odonus lane / bars-per-step.
  SetMacroText t -> H.modify_ _ { macroText = t }
  SetMacroBars v -> case Int.fromString v of
    Just n | n >= 1 -> H.modify_ _ { macroBars = n }
    _ -> pure unit
  -- Palette click: append a scene name to the lane (with a separating space).
  -- A name containing a space is quoted so it stays one token.
  AppendMacroName name -> do
    let tok = if String.contains (String.Pattern " ") name then "\"" <> name <> "\"" else name
    H.modify_ \st -> st { macroText = if st.macroText == "" then tok else st.macroText <> " " <> tok }
  -- Run / stop the sequencer. Turning ON re-gathers the library (so names resolve)
  -- and resets `macroStep` to -1 so the next tick applies the current step at once.
  ToggleMacro -> do
    on <- H.gets _.macroOn
    if on
      then H.modify_ _ { macroOn = false }
      else do
        refreshLibrary
        H.modify_ _ { macroOn = true, macroStep = -1 }
  -- The bar-quantized clock. Compute the current global step from the shared
  -- free-run epoch; when it crosses a boundary, resolve the cell and apply it.
  -- Rig-locked timing (reading the Link anchor instead of freeTempo) is a later
  -- slice — this drives the Solo/standalone case.
  MacroTick -> do
    st <- H.get
    when st.macroOn do
      let toks = parseLane st.macroText
          n = length toks
      when (n > 0 && st.macroBars > 0) do
        now <- liftEffect dateNow
        let barMs = 4.0 * 60000.0 / freeTempo
            epochMs = st.freeT0 / 1000.0
            barIdx = max 0 (Int.floor ((now - epochMs) / barMs))
            stepGlobal = barIdx `div` st.macroBars
        when (stepGlobal /= st.macroStep) do
          H.modify_ _ { macroStep = stepGlobal }
          applyCell (resolveStep toks (stepGlobal `mod` n) (stepGlobal `div` n))
  -- Star / unstar a setup: promote it onto the go-to wall (publish its content and
  -- favourite it into `triggerfish-goto`) or take it off (unpublish that favourite).
  -- Content stays addressable either way; the wall is pure curation. Then re-fetch.
  ToggleStar r -> do
    st <- H.get
    case find (\g -> g.payload == r.text) st.goTo of
      Just g -> void (liftAff (attempt (Amphora.unpublish goToCollection g.hash)))
      Nothing -> void (liftAff (attempt (Amphora.publish
        { kind: kindOf r.inst, collection: goToCollection
        , name: r.name, source: "workbench", payload: r.text, tags: [] })))
    fetchGoTo
  -- The live Vetula→Odonus bridge. Odonus quantises to ONE harmonic-context set
  -- (chord-when-progression, else lens scale) pushed below as its pitchSet — no
  -- separate chord overlay, so the old per-voice chord feed is retired.
  PollVetula -> do
    -- Pull Vetula's progression + playhead for the nav harmonic-context strip.
    mharm <- H.query _vet unit (Vetula.AskHarmonic identity)
    case mharm of
      Just h -> H.modify_ _ { harm = h }
      Nothing -> pure unit
    -- Harmonic authority: pull Vetula's resting context scale and, when it CHANGES,
    -- install it as Odonus's pitchSet (RI.SetPitchSet, lockstep-safe). Vetula owns
    -- the scale; Odonus follows. Deduped so the 100ms poll doesn't flood the input.
    mctx <- H.query _vet unit (Vetula.AskContextScale identity)
    for_ mctx \ctx -> do
      let key = show ctx.root <> ":" <> show ctx.offsets
      prev <- H.gets _.ctxScaleKey
      when (key /= prev) do
        H.modify_ _ { ctxScaleKey = key }
        void $ H.query _odo unit (SQ.SetContextPitchSet ctx.root ctx.offsets unit)
    -- Vetula auto-resync (ATLANTIS only): Vetula has no incremental rig path, so the
    -- shell diffs its payload and re-pushes on a SETTLED change (payload stable for
    -- one poll AND different from what was last sent). A drag coalesces into one push
    -- ~one tick after it stops; glitchless because the rig re-push phase-aligns.
    -- Only when the Vetula voice is actually running on the rig (armed in ATLANTIS).
    msig <- H.query _vet unit (Vetula.AskBrushSig identity)
    for_ msig \sig -> do
      st <- H.get
      -- Re-push (SetSounding Rig re-voices) only when Vetula is actually rig-
      -- authoritative — `soundingOf … Vet == Rig` already implies armed + ATLANTIS.
      when (soundingOf st.mode st.armed (previewSet st) Vet == Rig && sig == st.brushPrev && sig /= st.brushSent) do
        _ <- H.query _vet unit (Vetula.SetSounding Rig unit)
        H.modify_ _ { brushSent = sig }
      H.modify_ _ { brushPrev = sig }

-- Push one machine its DERIVED Sounding (soundingOf mode armed). The instrument
-- edge-detects the transition itself: local-mute on leaving Local, rig handoff on
-- entering Rig, rig-stop on leaving Rig. This one call replaces the old
-- broadcastMaster / broadcastAudible / broadcastSyncToRig / broadcastStopRig /
-- reconcileRig — the shell no longer tracks a separate rig-running mirror.
pushSounding :: forall o m. MonadAff m => Which -> H.HalogenM RState RAction Slots o m Unit
pushSounding w = do
  st <- H.get
  void $ querySounding w (soundingOf st.mode st.armed (previewSet st) w)

-- macro-tidal: enact one resolved step on the Odonus lane. A named form loads its
-- scene and arms Odonus so it sounds; a rest (or the silent branch of an
-- alternation) disarms it. An unresolved name is held (Odonus keeps playing what
-- it had) and flagged in the readout. The sequencer thus owns Odonus's arm — the
-- generalisation of scene-scheduling up out of the instrument.
applyCell :: forall o m. MonadAff m => Cell -> H.HalogenM RState RAction Slots o m Unit
applyCell = case _ of
  Quiet -> do
    a <- H.gets _.armed
    when (Set.member Odo a) do
      H.modify_ _ { armed = Set.delete Odo a }
      pushSounding Odo
    H.modify_ _ { macroCell = "~" }
  Load name mods -> do
    lib <- H.gets _.library
    case find (\r -> r.inst == Odo && r.name == name) lib of
      Just row -> do
        _ <- queryLoad Odo row.idx
        a <- H.gets _.armed
        when (not (Set.member Odo a)) (H.modify_ _ { armed = Set.insert Odo a })
        for_ mods applyMod   -- apply the transform stack to the freshly loaded form
        pushSounding Odo
        H.modify_ _ { macroCell = name <> joinWith "" (map (\md -> " #" <> md.verb) mods) }
      Nothing -> H.modify_ _ { macroCell = name <> " ?" }

-- Interpret one resolved modifier. `scale` re-quantises the whole rig by setting
-- VETULA's resting scale (Vetula is the single harmonic authority; the poll bridge
-- then pushes it into Odonus's pitchSet). So `# scale` is a rig-global harmonic
-- verb, not an Odonus-local edit. Other verbs are no-ops for now — the seam is here
-- for fast / bass / transpose. An unparseable scale arg is silently skipped.
applyMod :: forall o m. MonadAff m => ResolvedMod -> H.HalogenM RState RAction Slots o m Unit
applyMod md = case md.verb of
  "scale" -> case parseScaleArg md.arg of
    Just s -> void $ H.query _vet unit (Vetula.SetRestingScale s.root s.offsets unit)
    Nothing -> pure unit
  _ -> pure unit

-- Parse a `# scale` argument ("F# lydian dominant", "G major") into a root pitch
-- class + the scale's intervals (from Reef.Scale). Forgiving: the root matches
-- rootNames case-insensitively (with flat aliases); the type is normalised
-- (lowercased, spaces removed) against scaleTypes; a bare root defaults to major.
parseScaleArg :: String -> Maybe { root :: Int, offsets :: Array Int }
parseScaleArg s = case uncons (filter (_ /= "") (String.split (String.Pattern " ") s)) of
  Nothing -> Nothing
  Just { head: rootTok, tail: scaleWords } -> do
    rootPc <- matchRoot rootTok
    offsets <-
      if null scaleWords then Just [ 0, 2, 4, 5, 7, 9, 11 ]
      else matchScaleIvls (String.toLower (joinWith "" scaleWords))
    Just { root: rootPc, offsets }

matchRoot :: String -> Maybe Int
matchRoot tok =
  let u = String.toUpper tok
  in case findIndex (\n -> String.toUpper n == u) rootNames of
       Just i -> Just i
       Nothing -> case find (\(Tuple a _) -> a == u) rootFlatAliases of
         Just (Tuple _ canon) -> findIndex (_ == canon) rootNames
         Nothing -> Nothing

-- Flat spellings → their sharp equivalent in rootNames (keys uppercased).
rootFlatAliases :: Array (Tuple String String)
rootFlatAliases =
  [ Tuple "DB" "C#", Tuple "EB" "D#", Tuple "GB" "F#", Tuple "AB" "G#", Tuple "BB" "A#" ]

matchScaleIvls :: String -> Maybe (Array Int)
matchScaleIvls norm = map _.intervals (find (\t -> String.toLower t.name == norm) scaleTypes)

-- The set of machines currently auditioning — the third input to `soundingOf`, so
-- preview is part of the derivation. Ending a preview is just removing it here and
-- re-deriving; there is no separate "force Local / restore" pathway to get wrong.
previewSet :: RState -> Set Which
previewSet st = Set.fromFoldable (map _.inst st.previewing)

-- Row identity (instrument + index) — the key both picking and previewing use.
sameRow :: LibRow -> LibRow -> Boolean
sameRow a b = a.inst == b.inst && a.idx == b.idx

-- Is this shelf row currently auditioning?
isPreviewing :: RState -> LibRow -> Boolean
isPreviewing st r = any (sameRow r) st.previewing

querySounding :: forall o m. Which -> Sounding -> H.HalogenM RState RAction Slots o m (Maybe Unit)
querySounding w s = case w of
  Odo -> H.query _odo unit (SQ.SetSounding s unit)
  Bal -> H.query _bal unit (SQ.SetSounding s unit)
  Sel -> H.query _sel unit (SQ.SetSounding s unit)
  Vet -> H.query _vet unit (Vetula.SetSounding s unit)
  Tid -> pure Nothing
  Suf -> pure Nothing
  Ste -> pure Nothing

-- Re-derive and push every machine's Sounding (on arm-all / mode flip / init).
pushAll :: forall o m. MonadAff m => H.HalogenM RState RAction Slots o m Unit
pushAll = for_ [ Odo, Bal, Sel, Vet ] pushSounding

-- Query each mounted instrument for its current source and stitch the four
-- into one labelled document.
refreshTidal :: forall o m. H.HalogenM RState RAction Slots o m Unit
refreshTidal = do
  o <- H.query _odo unit (SQ.AskSource identity)
  b <- H.query _bal unit (SQ.AskSource identity)
  s <- H.query _sel unit (SQ.AskSource identity)
  v <- H.query _vet unit (Vetula.AskSource identity)
  H.modify_ _
    { tidalDoc = assemble
        [ Tuple "ODONUS" o, Tuple "BALISTES" b, Tuple "SELENE" s, Tuple "VETULA" v ] }
  -- Refresh the channel-map: which → midi voice names are in use, and re-push the
  -- current bindings so Vetula stays in sync when the page reopens.
  mnames <- H.query _vet unit (Vetula.AskVoiceNames identity)
  for_ mnames \names -> H.modify_ _ { vetulaNames = names }
  pushRouting

-- Push the shell's name → channel table to Vetula (it resolves each voice's channel
-- from it). Called on every binding edit and on Tidal-page refresh.
pushRouting :: forall o m. H.HalogenM RState RAction Slots o m Unit
pushRouting = do
  routing <- H.gets _.routing
  let binds = map (\(Tuple name ch) -> { name, ch }) (Map.toUnfoldable routing)
  void $ H.query _vet unit (Vetula.SetRouting binds unit)

assemble :: Array (Tuple String (Maybe String)) -> String
assemble = joinWith "\n\n\n" <<< map section
  where
  section (Tuple name msrc) =
    "-- ═══════════════  " <> name <> "  ═══════════════\n\n" <> fromMaybe "(no source)" msrc

-- Gather every instrument's saved presets (via AskLibrary) into one flat list,
-- tagged by instrument + index — the data the LIBRARY manager renders.
refreshLibrary :: forall o m. H.HalogenM RState RAction Slots o m Unit
refreshLibrary = do
  o <- H.query _odo unit (SQ.AskLibrary identity)
  b <- H.query _bal unit (SQ.AskLibrary identity)
  s <- H.query _sel unit (SQ.AskLibrary identity)
  v <- H.query _vet unit (Vetula.AskLibrary identity)
  H.modify_ _ { library = rows Odo o <> rows Bal b <> rows Sel s <> rows Vet v }
  where
  rows w m = mapWithIndex (\i e -> { inst: w, idx: i, name: e.name, text: e.text }) (fromMaybe [] m)

-- The cross-instrument curated wall — one favourite collection holding starred
-- setups from every instrument (the Cianni shelf of go-to's).
goToCollection :: String
goToCollection = "triggerfish-goto"

-- Each instrument's Amphora content kind (used when a star publishes a setup).
kindOf :: Which -> String
kindOf = case _ of
  Odo -> "odonus-scene"
  Bal -> "balistes-pattern"
  Sel -> "selene-rack"
  Vet -> "vetula-progression"
  _ -> "misc"

-- Re-fetch the go-to collection from Amphora (offline → keep what we have).
fetchGoTo :: forall o m. MonadAff m => H.HalogenM RState RAction Slots o m Unit
fetchGoTo = do
  res <- liftAff (attempt (Amphora.fetchCollection goToCollection))
  case res of
    Right items -> H.modify_ _ { goTo = items }
    Left _ -> pure unit

-- Is this shelf row on the go-to wall? Content-address identity: same payload =
-- same hash, so an exact payload match is a hash match without hashing here.
isStarred :: RState -> LibRow -> Boolean
isStarred st r = any (\g -> g.payload == r.text) st.goTo

-- Dispatch a LoadEntry / ImportText to the right slot (the two query types — the
-- shared SourceQuery and Vetula's own — agree on these constructors' shapes).
queryLoad :: forall o m. Which -> Int -> H.HalogenM RState RAction Slots o m (Maybe Unit)
queryLoad w i = case w of
  Odo -> H.query _odo unit (SQ.LoadEntry i unit)
  Bal -> H.query _bal unit (SQ.LoadEntry i unit)
  Sel -> H.query _sel unit (SQ.LoadEntry i unit)
  Vet -> H.query _vet unit (Vetula.LoadEntry i unit)
  Tid -> pure Nothing
  Suf -> pure Nothing
  Ste -> pure Nothing

queryImport :: forall o m. Which -> String -> H.HalogenM RState RAction Slots o m (Maybe Boolean)
queryImport w txt = case w of
  Odo -> H.query _odo unit (SQ.ImportText txt identity)
  Bal -> H.query _bal unit (SQ.ImportText txt identity)
  Sel -> H.query _sel unit (SQ.ImportText txt identity)
  Vet -> H.query _vet unit (Vetula.ImportText txt identity)
  Tid -> pure Nothing
  Suf -> pure Nothing
  Ste -> pure Nothing

render :: forall m. MonadAff m => RState -> H.ComponentHTML RAction Slots m
render st =
  HH.div_
    [ shellBar st
    -- All four are always in the tree (hence always mounted + running); the
    -- active one is shown, the rest are display:none but keep playing. On the
    -- TIDAL tab all four are hidden but still alive (and queryable). The three
    -- machine instruments inset their own root below the bar (position:fixed
    -- top:var(--tf-bar)); the in-flow Vetula pane is padded down to clear it.
    , pane (st.which == Odo) "" (HH.slot_ _odo unit Odonus.component unit)
    , pane (st.which == Bal) "" (HH.slot_ _bal unit Balistes.component unit)
    , pane (st.which == Sel) "" (HH.slot_ _sel unit Selene.component unit)
    , pane (st.which == Vet) "padding-top:var(--tf-bar)"
        (HH.slot _vet unit Vetula.component unit (\(Vetula.ArmChanged on) -> VetulaArmed on))
    , pane (st.which == Suf) "" (HH.slot_ _suf unit Sufflamen.component unit)
    , pane (st.which == Ste) "" (HH.slot_ _ste unit Stellatus.component unit)
    , if st.which == Tid then tidalView st else HH.text ""
    ]

-- A mounted-but-maybe-hidden pane. `display:none` keeps the component alive
-- (and its scheduler/MIDI running) while removing it from layout. `extra` adds
-- per-pane style (the in-flow Vetula pane pads itself below the shell bar; the
-- fixed-root machine instruments need nothing).
pane :: forall m. Boolean -> String -> H.ComponentHTML RAction Slots m -> H.ComponentHTML RAction Slots m
pane visible extra content =
  HH.div [ style ((if visible then "" else "display:none;") <> extra) ] [ content ]

-- Is shelf row `r` the one currently on the bench? (identity = instrument + idx).
isPicked :: Maybe LibRow -> LibRow -> Boolean
isPicked mp r = case mp of
  Just p -> p.inst == r.inst && p.idx == r.idx
  Nothing -> false

-- The TIDAL page is the WORKBENCH: a curated shelf of saved setups (left) feeding
-- a bench (right) where one is picked, previewed, transformed, and committed back
-- to its instrument. The raw-source aggregate is demoted to a slide-out drawer
-- (⟨ source ⟩) so the workbench owns the canvas. Content comes from Amphora via
-- each instrument's merged library (refreshLibrary → AskLibrary aggregate).
tidalView :: forall m. RState -> H.ComponentHTML RAction Slots m
tidalView st =
  HH.div
    [ style $ "max-width:1440px;margin:calc(var(--tf-bar) + 18px) auto 40px;padding:0 20px;font-family:Georgia,serif" ]
    [ channelMapPanel st
    , macroPanel st
    , workbenchHeader st
    , HH.div
        [ style "display:flex;gap:26px;align-items:flex-start" ]
        [ HH.div [ style "flex:0 0 380px;min-width:0" ] [ shelfPanel st ]
        , HH.div [ style "flex:1 1 auto;min-width:0" ] [ benchPanel st ]
        ]
    , if st.sourceOpen then sourceDrawer st else HH.text ""
    ]

-- The workbench title bar: heading + the source-drawer toggle on the right.
workbenchHeader :: forall m. RState -> H.ComponentHTML RAction Slots m
workbenchHeader st =
  HH.div
    [ style "display:flex;align-items:baseline;justify-content:space-between;gap:14px;margin-bottom:16px" ]
    [ HH.span
        [ style "font-size:15px;letter-spacing:0.16em;text-transform:uppercase;color:#4a463b" ]
        [ HH.text "Workbench — the go-to shelf" ]
    , HH.div [ style "display:flex;align-items:center;gap:8px" ]
        [ if null st.previewing then HH.text ""
          else HH.span
            [ HP.title "stop every local preview"
            , style $ "cursor:pointer;padding:3px 11px;border:1px solid #7aa07a;border-radius:4px;"
                <> "font-size:10px;letter-spacing:0.08em;text-transform:uppercase;color:#2f5a2f;background:#e4f0e2"
            , HE.onClick \_ -> StopPreview ]
            [ HH.text ("■ stop all previews (" <> show (length st.previewing) <> ")") ]
        , barBtn "refresh" RefreshTidal
        , barBtn (if st.sourceOpen then "source ▾" else "source ▸") ToggleSource
        ]
    ]

-- The rig's MIDI channel map — the config surface where channel assignment lives
-- (identity → destination; see docs/PLAN-midi-routing.md). The fixed defaults ARE
-- the standard Ableton project template; named Vetula voices are editable (bind a
-- name to a channel; blank = the ch5 default).
channelMapPanel :: forall m. RState -> H.ComponentHTML RAction Slots m
channelMapPanel st =
  HH.div [ style "margin-bottom:26px;padding:14px 16px;background:#f3efe4;border:1px solid #e3dfd2;border-radius:6px" ]
    [ HH.div
        [ style "font-size:13px;letter-spacing:0.14em;text-transform:uppercase;color:#5a564b;margin-bottom:10px" ]
        [ HH.text "MIDI output — channel map (the Ableton template)" ]
    , HH.div [ style "display:flex;flex-wrap:wrap;gap:6px 22px" ]
        (map fixedRow Routing.defaultRouting)
    , if null st.vetulaNames then HH.text ""
      else HH.div [ style "margin-top:12px;padding-top:10px;border-top:1px dashed #d8d0bd" ]
        [ HH.div [ style "font-size:10px;letter-spacing:0.14em;text-transform:uppercase;color:#8a7a4a;margin-bottom:7px" ]
            [ HH.text "named Vetula voices" ]
        , HH.div [ style "display:flex;flex-wrap:wrap;gap:6px 22px" ] (map nameRow st.vetulaNames)
        ]
    , HH.div [ style "margin-top:11px;font-size:11px;color:#8a8576;font-style:italic" ]
        [ HH.text "Selene → modular (FH-2 / ES-9) · Stellatus + Sufflamen → OSC. Name a Vetula → midi voice to route it off the ch5 default." ]
    ]
  where
  fixedRow r =
    HH.div [ style "display:flex;align-items:baseline;gap:8px;min-width:190px;flex:0 0 auto" ]
      [ HH.span [ style "font-size:12px;color:#2a271e" ] [ HH.text (Routing.sourceLabel r.source) ]
      , HH.span [ style "flex:1 1 auto;border-bottom:1px dotted #cdbb96;height:9px;min-width:14px" ] []
      , HH.span
          [ style "font-size:11px;letter-spacing:0.05em;color:#7a6a3a;font-family:'SF Mono',Menlo,Consolas,monospace" ]
          (map (\d -> HH.text (Routing.destLabel d)) r.dests)
      ]
  -- Editable: a named voice → its bound channel (blank input = the default).
  nameRow nm =
    let bound = Map.lookup nm st.routing
    in HH.div [ style "display:flex;align-items:baseline;gap:8px;min-width:190px;flex:0 0 auto" ]
        [ HH.span [ style "font-size:12px;color:#2a271e" ] [ HH.text ("Vetula · " <> nm) ]
        , HH.span [ style "flex:1 1 auto;border-bottom:1px dotted #cdbb96;height:9px;min-width:14px" ] []
        , HH.input
            [ HP.value (maybe "" show bound)
            , HE.onValueInput (SetBinding nm)
            , HP.placeholder (show Routing.vetulaDefaultChannel)
            , style "width:42px;font-family:'SF Mono',Menlo,Consolas,monospace;font-size:11px;padding:2px 5px;border-radius:4px;border:1px solid #cdbb96;background:#fffdf8;text-align:center"
            ]
        ]

-- macro-tidal (Slice 1): the ARRANGEMENT lane. A mini-notation string of Odonus
-- scene-names sequenced over bar-quantized steps — "midnight ~ <descent drift>".
-- Space-separated tokens divide the macro-cycle into equal steps; `~` is a rest;
-- `<a b c>` alternates one inner form per cycle. Run it and the sequencer loads
-- the resolved scene into Odonus at each step boundary (arming it to sound; a
-- rest disarms). The palette lists the available Odonus scenes to click into the
-- lane; unresolved names show in red and are held.
macroPanel :: forall m. RState -> H.ComponentHTML RAction Slots m
macroPanel st =
  let toks = parseLane st.macroText
      n = length toks
      odoNames = map _.name (filter (\r -> r.inst == Odo) st.library)
      curStep = if st.macroOn && st.macroStep >= 0 && n > 0 then Just (st.macroStep `mod` n) else Nothing
      unknown = filter (\nm -> not (nm `elem` odoNames)) (laneFormNames toks)
  in HH.div [ style "margin-bottom:26px;padding:14px 16px;background:#eef1ec;border:1px solid #d6ddd2;border-radius:6px" ]
    [ HH.div [ style "display:flex;align-items:baseline;justify-content:space-between;gap:14px;margin-bottom:11px" ]
        [ HH.span [ style "font-size:13px;letter-spacing:0.14em;text-transform:uppercase;color:#3f5a3f" ]
            [ HH.text "Arrangement — macro-tidal · Odonus lane" ]
        , HH.div [ style "display:flex;align-items:center;gap:12px" ]
            [ HH.span [ style "font-size:10px;letter-spacing:0.08em;text-transform:uppercase;color:#6a7a6a" ]
                [ HH.text "bars/step" ]
            , HH.input
                [ HP.value (show st.macroBars)
                , HE.onValueInput SetMacroBars
                , style "width:44px;font-family:'SF Mono',Menlo,Consolas,monospace;font-size:11px;padding:2px 5px;border-radius:4px;border:1px solid #b8c4b0;background:#fffdf8;text-align:center" ]
            , HH.span
                [ HE.onClick \_ -> ToggleMacro
                , style $ "cursor:pointer;padding:4px 14px;border-radius:5px;font-size:11px;letter-spacing:0.14em;"
                    <> "text-transform:uppercase;border:1px solid " <> (if st.macroOn then "#7aa07a" else "#b8c4b0") <> ";"
                    <> (if st.macroOn then "color:#eaf3ea;background:linear-gradient(#4a7a4a,#3a6a3a)" else "color:#3f5a3f;background:linear-gradient(#e4ece0,#d6ddd2)") ]
                [ HH.text (if st.macroOn then "■ stop" else "▶ run") ]
            ]
        ]
    , HH.div [ style "font-size:10px;color:#7a8a7a;margin-bottom:7px;font-family:'SF Mono',Menlo,Consolas,monospace" ]
        [ HH.text "~ rest · <a b> alternate per cycle · # scale <\"F# lydian dominant\" \"G major\"> re-quantise" ]
    , HH.input
        [ HP.value st.macroText
        , HE.onValueInput SetMacroText
        , HP.placeholder "\"bopping along\" # scale <\"F# lydian dominant\" \"G major\">"
        , HP.spellcheck false
        , style $ "width:100%;box-sizing:border-box;padding:9px 11px;border:1px solid #b8c4b0;border-radius:5px;"
            <> "background:#fffdf8;font-family:'SF Mono',Menlo,Consolas,monospace;font-size:13px;letter-spacing:0.02em;color:#22301f" ]
    -- The parsed step readout: one chip per top-level step, the running step lit,
    -- unresolved names ringed red. Plus the live cell + cycle when running.
    , if n == 0 then HH.text ""
      else HH.div [ style "display:flex;flex-wrap:wrap;align-items:center;gap:6px;margin-top:10px" ]
        ( mapWithIndex (stepChip odoNames curStep) toks
            <> [ if st.macroOn
                   then HH.span [ style "margin-left:8px;font-size:11px;color:#3d6b3d;font-style:italic" ]
                          [ HH.text ("♪ " <> (if st.macroCell == "" then "…" else st.macroCell)
                                      <> "  · cycle " <> show (if n > 0 then st.macroStep `div` n else 0)) ]
                   else HH.text "" ] )
    -- The palette: the Odonus scenes you can name in the lane (click to append).
    , HH.div [ style "margin-top:11px;padding-top:9px;border-top:1px dashed #c8d2c0" ]
        [ HH.span [ style "font-size:10px;letter-spacing:0.12em;text-transform:uppercase;color:#7a8a7a;margin-right:8px" ]
            [ HH.text "scenes" ]
        , if null odoNames
            then HH.span [ style "font-size:11px;color:#9aa89a;font-style:italic" ]
                   [ HH.text "save / refresh to gather Odonus scenes" ]
            else HH.span [ style "display:inline-flex;flex-wrap:wrap;gap:5px" ] (map paletteChip odoNames)
        ]
    , if null unknown then HH.text ""
      else HH.div [ style "margin-top:8px;font-size:11px;color:#a03028" ]
        [ HH.text ("unresolved (held): " <> joinWith ", " unknown) ]
    ]
  where
  paletteChip nm =
    HH.span
      [ HE.onClick \_ -> AppendMacroName nm
      , style $ "cursor:pointer;padding:2px 9px;border:1px solid #b8c4b0;border-radius:11px;background:#fbfdf9;"
          <> "font-family:'SF Mono',Menlo,Consolas,monospace;font-size:11px;color:#2f4a2f" ]
      [ HH.text nm ]

-- One step in the arrangement readout: its source label (a name, `~`, or a
-- `<…>` group), lit when it's the running step, ringed red if it names a form
-- that doesn't resolve against the loaded Odonus library.
stepChip :: forall m. Array String -> Maybe Int -> Int -> Step -> H.ComponentHTML RAction Slots m
stepChip odoNames curStep i step =
  let live = curStep == Just i
      resolvable = null (filter (\nm -> not (nm `elem` odoNames)) (laneFormNames [ step ]))
      border = if not resolvable then "#c85a50" else if live then "#4a7a4a" else "#c8d2c0"
      bg = if live then "linear-gradient(#dcecd6,#cde3c4)" else "#fbfdf9"
  in HH.span
    [ style $ "padding:3px 10px;border:1px solid " <> border <> ";border-radius:4px;background:" <> bg <> ";"
        <> "font-family:'SF Mono',Menlo,Consolas,monospace;font-size:12px;"
        <> "color:" <> (if not resolvable then "#a03028" else "#2f4a2f") ]
    [ HH.text (stepLabel step) ]

-- The SHELF: the curated ★ GO-TO wall leads (starred setups across every
-- instrument — the Cianni shelf), then the full archive sits behind a "dig"
-- toggle, grouped by instrument. Save-everything is the cheap substrate; the wall
-- is what you reach for. The paste-import box is the manual add path at the foot.
shelfPanel :: forall m. RState -> H.ComponentHTML RAction Slots m
shelfPanel st =
  let starred = filter (isStarred st) st.library
  in HH.div [ style "margin-bottom:26px" ]
    [ HH.div
        [ style "font-size:11px;letter-spacing:0.16em;text-transform:uppercase;color:#8a6a2a;margin-bottom:7px" ]
        [ HH.text "★ Go-to" ]
    , if null starred
        then HH.div [ style "color:#a89b78;font-size:11px;font-style:italic;margin-bottom:12px" ]
               [ HH.text "star a setup (☆) to pin it to your go-to wall" ]
        else HH.div_ (map (entryRow st true) starred)
    , HH.div
        [ style "margin:14px 0 8px;cursor:pointer;font-size:10px;letter-spacing:0.14em;text-transform:uppercase;color:#8a7a4a"
        , HE.onClick \_ -> ToggleDig ]
        [ HH.text (if st.digOpen then "▾ archive — all setups" else "▸ dig the archive — all setups") ]
    , if not st.digOpen then HH.text ""
      else if null st.library
        then HH.div [ style "color:#8a8576;font-size:12px;font-style:italic;margin-bottom:14px" ]
               [ HH.text "(refresh to gather each instrument's saved setups)" ]
        else HH.div_ (map (groupSection st) [ Odo, Bal, Sel, Vet ])
    , importBox st
    ]

groupSection :: forall m. RState -> Which -> H.ComponentHTML RAction Slots m
groupSection st w =
  let rows = filter (\r -> r.inst == w) st.library
  in if null rows then HH.text ""
     else HH.div [ style "margin-bottom:12px" ]
       ( [ HH.div
             [ style "font-size:10px;letter-spacing:0.16em;text-transform:uppercase;color:#8a7a4a;margin-bottom:5px" ]
             [ HH.text (whichName w) ]
         ] <> map (entryRow st false) rows )

-- One shelf tile: a ★/☆ star toggle (curation), the name (click to pick onto the
-- bench), and a ▶/■ preview toggle that auditions it locally right there — so any
-- running preview can be stopped from the same row that started it. The picked one
-- is brass-highlighted; on the go-to wall the row shows its instrument (the wall is
-- cross-instrument). Star / name / preview are separate gestures — they never
-- collide. (Copy lives on the bench.)
entryRow :: forall m. RState -> Boolean -> LibRow -> H.ComponentHTML RAction Slots m
entryRow st showInst r =
  let on = isPicked st.picked r
      starred = isStarred st r
      auditioning = isPreviewing st r
  in HH.div
    [ style $ "display:flex;align-items:center;gap:8px;padding:6px 10px;margin-bottom:3px;"
        <> "border-radius:5px;border:1px solid " <> (if on then "#b5832b" else "#e3dfd2") <> ";"
        <> "background:" <> (if on then "linear-gradient(#f6ecd4,#efe2c2)" else "#ffffff") ]
    [ HH.span
        [ HP.title (if starred then "on the go-to wall — click to remove" else "star → pin to the go-to wall")
        , style $ "cursor:pointer;font-size:13px;color:" <> (if starred then "#c8a02a" else "#c8c4b8")
        , HE.onClick \_ -> ToggleStar r ]
        [ HH.text (if starred then "★" else "☆") ]
    , HH.span
        [ style "flex:1 1 auto;cursor:pointer;font-size:12px;color:#2a271e"
        , HE.onClick \_ -> PickEntry r ]
        [ if showInst
            then HH.span [ style "color:#8a7a4a;font-size:9px;letter-spacing:0.1em;text-transform:uppercase;margin-right:6px" ]
                   [ HH.text (whichName r.inst) ]
            else HH.text ""
        , HH.text r.name
        ]
    , HH.span
        [ HP.title (if auditioning then "stop this local preview" else "preview locally (rig untouched)")
        , style $ "cursor:pointer;font-size:12px;padding:0 4px;color:"
            <> (if auditioning then "#2f7a2f" else "#9a9484")
        , HE.onClick \_ -> PreviewEntry r ]
        [ HH.text (if auditioning then "■" else "▶") ]
    ]

-- The BENCH: the picked setup, with preview (local audition), transforms, and a
-- commit back to its instrument. Preview + transforms are stubbed this slice
-- (the shelf→pick→commit loop is live); commit = LoadFromLib, which already loads
-- the entry into its editor and switches to it.
benchPanel :: forall m. RState -> H.ComponentHTML RAction Slots m
benchPanel st = case st.picked of
  Nothing ->
    HH.div [ style benchShell ]
      [ HH.div [ style "color:#8a8576;font-size:13px;font-style:italic;padding:30px 6px;text-align:center" ]
          [ HH.text "pick a setup from the shelf to work on it" ] ]
  Just r ->
    HH.div [ style benchShell ]
      [ HH.div [ style "display:flex;align-items:baseline;gap:10px;margin-bottom:4px" ]
          [ HH.span [ style "font-size:10px;letter-spacing:0.16em;text-transform:uppercase;color:#8a7a4a" ]
              [ HH.text (whichName r.inst) ]
          , HH.span [ style "font-size:16px;color:#2a271e" ] [ HH.text r.name ]
          ]
      , HH.div [ style "display:flex;align-items:center;gap:8px;margin:12px 0 16px" ]
          [ barBtn (if isPreviewing st r then "■ stop preview" else "▶ preview (local)") (PreviewEntry r)
          , barBtn "commit → editor" (LoadFromLib r.inst r.idx)
          , barBtn "copy" (CopyEntry r.text)
          , barBtn (if isStarred st r then "★ keep" else "☆ keep") (ToggleStar r)
          , if isPreviewing st r
              then HH.span [ style "font-size:11px;color:#3d6b3d;font-style:italic;margin-left:4px" ]
                     [ HH.text "♪ auditioning locally · rig untouched" ]
              else HH.text ""
          ]
      , transformRow
      , HH.pre
          [ style $ "margin-top:14px;padding:10px 12px;border-radius:6px;"
              <> "background:#fbf9f2;border:1px solid #e3dfd2;white-space:pre-wrap;word-break:break-word;"
              <> "font-family:'SF Mono',Menlo,Consolas,monospace;font-size:11px;line-height:1.5;color:#3a352a" ]
          [ HH.text r.text ]
      ]
  where
  benchShell = "padding:16px 18px;background:#f3efe4;border:1px solid #e3dfd2;border-radius:8px"

-- The transform rack — the heart of the workbench. Stubbed controls this slice;
-- next slice each becomes a morphism that yields new content + an edge to source.
transformRow :: forall m. H.ComponentHTML RAction Slots m
transformRow =
  HH.div [ style "display:flex;flex-wrap:wrap;gap:14px 22px;padding:12px 14px;background:#efe9db;border:1px solid #e3dfd2;border-radius:6px" ]
    [ tGroup "speed" [ "½", "¾", "1", "2" ]
    , tGroup "transpose" [ "−5", "+0", "+7" ]
    , tGroup "bass" [ "F#2" ]
    , tGroup "scale" [ "swap →" ]
    , tGroup "layer +" [ "breakbeat ▾" ]
    ]
  where
  tGroup label opts =
    HH.div [ style "display:flex;align-items:baseline;gap:7px" ]
      ( [ HH.span [ style "font-size:10px;letter-spacing:0.08em;text-transform:uppercase;color:#8a7a4a" ] [ HH.text label ] ]
          <> map (\o -> stubBtn o "transforms land next slice") opts )

-- A disabled placeholder control (tooltip explains it's coming), so the workbench
-- layout is fully legible before the seams behind it exist.
stubBtn :: forall m. String -> String -> H.ComponentHTML RAction Slots m
stubBtn label hint =
  HH.span
    [ HP.title hint
    , style $ "padding:3px 10px;border:1px dashed #cdbb96;border-radius:4px;opacity:0.55;cursor:not-allowed;"
        <> "font-size:10px;letter-spacing:0.08em;text-transform:uppercase;color:#8a7a4a;background:#00000005" ]
    [ HH.text label ]

importBox :: forall m. RState -> H.ComponentHTML RAction Slots m
importBox st =
  HH.div
    [ style "margin-top:16px;padding:12px;background:#f3efe4;border:1px solid #e3dfd2;border-radius:6px" ]
    [ HH.div
        [ style "font-size:10px;letter-spacing:0.12em;text-transform:uppercase;color:#7a7363;margin-bottom:7px" ]
        [ HH.text "Import — paste Lepidoptera eDSL, then choose its instrument" ]
    , HH.textarea
        [ HP.value st.importText
        , HE.onValueInput SetImportText
        , HP.placeholder "balistesPattern \"…\"  ·  odonusPatch \"…\"  ·  a Selene rack  ·  a Vetula  note \"<…>\""
        , HP.spellcheck false
        , style $ "width:100%;box-sizing:border-box;min-height:84px;resize:vertical;padding:8px 10px;"
            <> "border:1px solid #cdbb96;border-radius:5px;background:#fffdf8;"
            <> "font-family:'SF Mono',Menlo,Consolas,monospace;font-size:11px;line-height:1.5;color:#2a271e" ]
    , HH.div
        [ style "display:flex;align-items:center;gap:7px;margin-top:8px" ]
        [ HH.span [ style "font-size:10px;letter-spacing:0.1em;text-transform:uppercase;color:#7a7363" ] [ HH.text "import →" ]
        , barBtn "Odonus" (ImportInto Odo)
        , barBtn "Balistes" (ImportInto Bal)
        , barBtn "Selene" (ImportInto Sel)
        , barBtn "Vetula" (ImportInto Vet)
        , if st.importMsg == "" then HH.text ""
          else HH.span [ style "font-size:11px;color:#5a4a22;margin-left:4px" ] [ HH.text st.importMsg ]
        ]
    ]

-- The read-only aggregate of all four modules' source — demoted from a column to
-- a slide-out drawer below the workbench (toggled by ⟨ source ⟩). Still the copy /
-- paste-into-Calypso surface; just no longer eating half the canvas.
sourceDrawer :: forall m. RState -> H.ComponentHTML RAction Slots m
sourceDrawer st =
  HH.div [ style "margin-top:22px;padding-top:18px;border-top:1px solid #d8d0bd" ]
    [ HH.div
        [ style "display:flex;align-items:baseline;gap:14px;margin-bottom:12px" ]
        [ HH.span
            [ style "font-size:13px;letter-spacing:0.14em;text-transform:uppercase;color:#5a564b" ]
            [ HH.text "Source — the whole playing surface" ]
        , barBtn "copy" CopyTidal
        , barBtn "refresh" RefreshTidal
        ]
    , HH.pre
        [ style $ "margin:0;padding:16px 18px;background:#ffffff;border:1px solid #e3dfd2;"
            <> "border-radius:6px;box-shadow:0 1px 4px #00000012;overflow-x:auto;"
            <> "font-family:'SF Mono',Menlo,Consolas,monospace;font-size:12px;line-height:1.55;"
            <> "color:#2a271e;white-space:pre;-webkit-user-select:text;user-select:text" ]
        [ HH.text (if st.tidalDoc == "" then "(refresh to gather the four modules)" else st.tidalDoc) ]
    ]

barBtn :: forall m. String -> RAction -> H.ComponentHTML RAction Slots m
barBtn label act =
  HH.button
    [ HE.onClick \_ -> act
    , style $ "padding:3px 11px;border:1px solid #cdbb96;border-radius:4px;cursor:pointer;"
        <> "font-size:10px;letter-spacing:0.1em;text-transform:uppercase;color:#5a4a22;"
        <> "background:linear-gradient(#f3ecd9,#e9e0c6)" ]
    [ HH.text label ]

-- The shared shell bar: one fixed strip across every tab — master transport
-- (left), the rack/instrument nameplate (centred), the switcher (right). It
-- reserves `--tf-bar` of height so no instrument's own top content collides
-- with it, and gives the rack one identity over both the machine and oracle
-- aesthetics underneath.
shellBar :: forall m. RState -> H.ComponentHTML RAction Slots m
shellBar st =
  HH.div
    [ style $ "position:fixed;top:0;left:0;right:0;height:var(--tf-bar);z-index:50;box-sizing:border-box;"
        <> "display:flex;align-items:center;justify-content:space-between;gap:16px;padding:0 12px;overflow:hidden;"
        <> "background:linear-gradient(#d4cfc0,#c2bcab);border-bottom:1px solid #00000026;"
        <> "box-shadow:0 1px 4px #00000018;font-family:Georgia,serif" ]
    -- LEFT: wordmark + the instrument switcher. Everything in this bar is a flat
    -- flex row with NO absolute positioning — so nothing can overlap a tab. (The
    -- old absolute-centered chrome's empty 96px chord-slot used to park over ODONUS
    -- and swallow its click; a flex layout makes that impossible by construction.)
    [ HH.div
        [ style "display:flex;align-items:center;gap:16px;flex:0 0 auto" ]
        [ HH.span
            [ style "font-size:11px;letter-spacing:0.22em;text-transform:uppercase;color:#4a463b" ]
            [ HH.text "Triggerfish" ]
        , switcher st
        ]
    -- MIDDLE: the SOLO⟷ATLANTIS authority toggle + the live harmonic-context glyph.
    -- Shrinkable + clipped, so a long progression compresses here rather than
    -- pushing into its neighbours.
    , HH.div
        [ style "display:flex;align-items:center;gap:16px;flex:0 1 auto;min-width:0;overflow:hidden" ]
        [ modeToggle st
        , harmStrip st
        ]
    -- RIGHT: the master transport (arm-all / stop-all).
    , HH.button
        [ HE.onClick \_ -> ToggleMaster
        , style $ "flex:0 0 auto;padding:6px 18px;border:1px solid #00000033;border-radius:6px;cursor:pointer;"
            <> "font-size:11px;letter-spacing:0.16em;text-transform:uppercase;box-shadow:0 1px 3px #00000022;"
            <> "color:" <> (if anyArmed st.armed then "#fbeae7" else "#1c1a12")
            <> ";background:" <> (if anyArmed st.armed then "linear-gradient(#b23b28,#9a3120)" else "linear-gradient(#c8a86a,#b8975a)") ]
        [ HH.text (if anyArmed st.armed then "■ STOP" else "▶ PLAY") ]
    ]

-- The instrument switcher: one segmented control. Odo/Bal/Sel/Vet are armable
-- (dot + name); Suf/Ste/Tid are plain (no arm dot — rig-only prototypes / the
-- read-only aggregate).
switcher :: forall m. RState -> H.ComponentHTML RAction Slots m
switcher st =
  HH.div
    [ style $ "display:flex;gap:0;flex:0 0 auto;border:1px solid #00000033;border-radius:6px;overflow:hidden;"
        <> "box-shadow:0 1px 3px #00000022" ]
    [ armSeg st Odo "ODONUS"
    , armSeg st Bal "BALISTES"
    , armSeg st Sel "SELENE"
    , armSeg st Vet "VETULA"
    , seg "SUFFLAMEN" (st.which == Suf) (Pick Suf)
    , seg "STELLATUS" (st.which == Ste) (Pick Ste)
    , seg "TIDAL" (st.which == Tid) (Pick Tid)
    ]

whichName :: Which -> String
whichName = case _ of
  Odo -> "Odonus"
  Bal -> "Balistes"
  Sel -> "Selene"
  Vet -> "Vetula"
  Tid -> "Tidal"
  Suf -> "Sufflamen"
  Ste -> "Stellatus"

seg :: forall m. String -> Boolean -> RAction -> H.ComponentHTML RAction Slots m
seg label active act =
  HH.button
    [ HE.onClick \_ -> act
    , style $ "padding:6px 14px;border:0;cursor:pointer;font-size:11px;letter-spacing:0.12em;"
        <> "text-transform:uppercase;color:" <> (if active then "#1c1a12" else "#5a564b")
        <> ";background:" <> (if active then "linear-gradient(#c8a86a,#b8975a)" else "linear-gradient(#e9e5d9,#dcd8c9)") ]
    [ HH.text label ]

-- An instrument tab: a play/pause dot + the name, as two independent click targets
-- (like a browser tab's close button). The dot toggles that instrument's ARM
-- WITHOUT switching panes (▶ = armed-off, click to start; ❚❚ = armed, click to
-- stop); the name switches to the pane. The dot glows when armed.
armSeg :: forall m. RState -> Which -> String -> H.ComponentHTML RAction Slots m
armSeg st w label =
  let active = st.which == w
      isArmed = Set.member w st.armed
      bg = if active then "linear-gradient(#c8a86a,#b8975a)" else "linear-gradient(#e9e5d9,#dcd8c9)"
  in HH.div
      [ style ("display:flex;align-items:center;background:" <> bg) ]
      [ HH.span
          [ HE.onClick \_ -> ArmTab w
          , style $ "padding:6px 3px 6px 9px;cursor:pointer;font-size:9px;line-height:1;"
              <> "color:" <> (if isArmed then "#2f7a3f" else "#9a9488") ]
          [ HH.text (if isArmed then "❚❚" else "▶") ]
      , HH.span
          [ HE.onClick \_ -> Pick w
          , style $ "padding:6px 13px 6px 5px;cursor:pointer;font-size:11px;letter-spacing:0.12em;"
              <> "text-transform:uppercase;color:" <> (if active then "#1c1a12" else "#5a564b") ]
          [ HH.text label ]
      ]

-- The SOLO⟷ATLANTIS authority toggle. Each mode carries its own colour so the
-- active authority reads at a glance: SOLO warm/gold (a standalone instrument),
-- ATLANTIS deep sea-blue (the rig is the sound). Clicking a segment sets that mode.
modeToggle :: forall m. RState -> H.ComponentHTML RAction Slots m
modeToggle st =
  HH.div
    [ style $ "display:flex;border:1px solid #00000033;border-radius:5px;overflow:hidden;"
        <> "box-shadow:0 1px 2px #00000022" ]
    [ modeSeg "SOLO" (st.mode == Solo) "#1c1a12" "linear-gradient(#c8a86a,#b8975a)" (SetMode Solo)
    , modeSeg "ATLANTIS" (st.mode == Atlantis) "#eaf3fa" "linear-gradient(#3a6b8a,#2d5670)" (SetMode Atlantis)
    ]

modeSeg :: forall m. String -> Boolean -> String -> String -> RAction -> H.ComponentHTML RAction Slots m
modeSeg label active onColor onBg act =
  HH.button
    [ HE.onClick \_ -> act
    , style $ "padding:5px 13px;border:0;cursor:pointer;font-size:10px;letter-spacing:0.16em;"
        <> "text-transform:uppercase;color:" <> (if active then onColor else "#5a564b")
        <> ";background:" <> (if active then onBg else "linear-gradient(#e9e5d9,#dcd8c9)") ]
    [ HH.text label ]

-- The live harmonic-context glyph, polled from Vetula: one mark per chord in the
-- performance progression — ● the chord the playhead is on, ○ the others — with a
-- trailing ‑ per extra bar of dwell and a · for a skipped chord. Visible in every
-- pane, so the progression is always in view (the palette animation, surfaced).
harmStrip :: forall m. RState -> H.ComponentHTML RAction Slots m
harmStrip st =
  HH.div
    [ style "display:flex;align-items:baseline;gap:12px" ]
    -- The progress row: where the playhead is, as PER-CHORD click targets — click a
    -- chord to jump Vetula's progression there live (advance the playhead when
    -- playing; move the Odonus feed when stopped). See RAction.JumpVetula.
    [ HH.div
        [ style $ "display:flex;align-items:baseline;font-family:'SF Mono',Menlo,Consolas,monospace;"
            <> "font-size:13px;line-height:1;letter-spacing:0.14em" ]
        (if null st.harm.durs
           then [ HH.span [ style "color:#4a463b" ] [ HH.text "—" ] ]
           else mapWithIndex (harmChip st.harm.active) st.harm.durs)
    -- The content row: the notes of the chord under the playhead (bass-up), so the
    -- strip shows both WHERE we are and WHAT is sounding — the progression view's
    -- pitch content, time-multiplexed through the playhead.
    -- Reserve the 96px chord slot ONLY when a chord is showing, so an empty
    -- progression collapses to nothing instead of leaving a dead gap.
    , HH.span
        [ style $ "font-family:Georgia,serif;font-size:12px;letter-spacing:0.1em;color:#2d5670;"
            <> (if st.harm.chord == "" then "" else "min-width:96px") ]
        [ HH.text st.harm.chord ]
    ]

-- One chord in the nav strip: its glyph (● at the playhead, ○ elsewhere) plus its
-- dwell dashes, as a click target that jumps the progression to that chord. A
-- skipped chord (dwell 0) is a dim, non-interactive dot.
harmChip :: forall m. Int -> Int -> Int -> H.ComponentHTML RAction Slots m
harmChip active i d =
  if d <= 0
    then HH.span [ style "color:#b0aa9c" ] [ HH.text "·" ]
    else HH.span
      [ HE.onClick \_ -> JumpVetula i
      , style $ "cursor:pointer;color:" <> (if i == active then "#b23b28" else "#4a463b") ]
      [ HH.text ((if i == active then "●" else "○") <> joinWith "" (replicate (d - 1) "‑")) ]

style :: forall r i. String -> HP.IProp r i
style = HP.attr (H.AttrName "style")
