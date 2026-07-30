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

import Data.Array (any, deleteAt, filter, find, findIndex, last, length, mapMaybe, mapWithIndex, modifyAt, null, replicate, uncons, unsnoc, (!!))
import Data.FoldableWithIndex (forWithIndex_)
import Data.Foldable (for_)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
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
import Effect.Aff (attempt, delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Data.Time.Duration (Milliseconds(..))
import Effect.Timer (setInterval)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Halogen.Query.Event (eventListener)
import Type.Proxy (Proxy(..))
import Web.Event.Event as E
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.HTML.HTMLInputElement as HInput
import Web.HTML.HTMLTextAreaElement as HTextArea
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Binnacle.Audio (armAudioKeepAlive)
import Binnacle.Time (dateNow)
import Triggerfish.Odonus.Grid as Odonus
import Triggerfish.Balistes.Component as Balistes
import Triggerfish.Selene.Component as Selene
import Triggerfish.Sufflamen.Component as Sufflamen
import Triggerfish.Stellatus.Component as Stellatus
import Triggerfish.SourceQuery as SQ
import Triggerfish.Glyph as G
import Triggerfish.GlyphView (chipIcons, faIcon)
import Triggerfish.Scenes as Scenes
import Triggerfish.Scenes.Store as ScenesStore
import Triggerfish.Macro.Store as MacroStore
import Triggerfish.Midi.Routing as Routing
import Triggerfish.Amphora as Amphora
import Triggerfish.Macro (Cell(Quiet, Load), Form(..), Step, ResolvedMod, parseLane, resolveStep, stepLabel)
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
  | BalChipChanged (Maybe G.ChipView)  -- Balistes' identity-chip view, for the status board
  | SelChipChanged (Maybe G.ChipView)  -- Selene's identity-chip view, for the status board
  | OdoChipChanged (Maybe G.ChipView)  -- Odonus's identity-chip view, for the status board
  | CaptureKey                 -- the global CAPTURE hotkey → bank a preset on the active machine
  | OpenChipMenu Which         -- click a status-board glyph → open (or close) its recall menu
  | CloseChipMenu
  | RecallFrom Which Int        -- recall bank slot i on machine w, then close the menu
  | StarFrom Which Int          -- toggle a preset's star (menu stays open, refreshed)
  | DeleteFrom Which Int        -- delete a preset (menu stays open, refreshed)
  -- macro-tidal (Slice 1): the arrangement layer on the TIDAL page. One lane of
  -- Odonus scene-names, sequenced over bar-quantized steps.
  | SetLaneText Which String   -- edit one machine's mini-notation lane
  | SetMacroBars String        -- edit bars-per-step
  | ToggleMacro                -- run / stop the macro sequencer
  | MacroTick                  -- the bar-quantized clock poll (step-boundary driver)
  -- Scene grid (Ableton-like sequencer, docs/DESIGN-scene-modal.md). A rig-wide
  -- grid: rows = scenes, columns = the live machines; a cell is a machine's glyph.
  | AddSceneFromRig            -- snapshot every machine's current chip glyph → a new scene
  | LaunchScene Int            -- recall a scene's tuple across machines (manual fire)
  | DeleteScene Int
  | SetSceneName Int String    -- name (promote) a scene; "" leaves it unnamed
  | OpenCellPick Int Int       -- click a cell (scene, machine col) → open its bank picker
  | CloseCellPick
  | SetSceneCell Int Int (Maybe String)  -- assign/clear a cell (scene, machine col, alias)
  | ToggleSceneRun             -- run / stop the bar-quantized auto-advance
  | SetSceneBars String        -- bars per scene
  | SceneTick                  -- the scene bar-clock poll (advance at a boundary)
  | AcceptCompletion Which String  -- accept a `:`-completion: insert the glyph alias
  | CloseCompletion            -- dismiss the `:`-completion popup (blur / Esc)

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
  -- macro-tidal — the Tidal-like sequencer (docs/DESIGN-scene-modal.md): one
  -- mini-notation LANE per machine, over glyph ALIASES (`"owl-bomb star-ambulance
  -- ~"`). Each lane resolves against its machine's preset bank (recall by alias),
  -- and — unlike the scene grid's leave-as-is — a `~` step is a REST = silence
  -- (the machine disarms). Lanes are polymetric: they share the bar pulse but each
  -- cycles at its own token count. `macroLanes` holds each lane's text; `macroBars`
  -- = bars per step; `macroOn` runs it; `macroStep` is the last global step applied
  -- (-1 = none, so we only act at a boundary); `macroReadout` is each lane's live
  -- current-token label ("~" rest, "alias" resolved, "alias ?" unresolved).
  , macroLanes :: Map Which String
  , macroReadout :: Map Which String
  , macroBars :: Int, macroOn :: Boolean, macroStep :: Int
  -- The `:`-completion popup for a lane: which machine, the `:`-prefix being typed
  -- (the trailing token), and its bank's matching glyphs. Nothing = closed. Typing
  -- a `:foo` token opens it (emoji-picker over that machine's bank); click inserts.
  , laneComplete :: Maybe { w :: Which, prefix :: String, items :: Array MenuItem }
  -- Harmonic-authority bridge: the last resting-context scale pushed from Vetula
  -- into Odonus (serialised for dedup, so the 100ms poll only re-pushes on change).
  , ctxScaleKey :: String
  -- the six-machine status board: each machine's identity-chip view. Odonus /
  -- Balistes / Selene PUSH theirs via Output (change-gated from their Frame loop);
  -- Vetula has no continuous frame loop, so the shell PULLS its chip in PollVetula
  -- (AskChip) and parks it in `vetChip`. Suf/Ste report Nothing (prototypes).
  , balChip :: Maybe G.ChipView
  , selChip :: Maybe G.ChipView
  , odoChip :: Maybe G.ChipView
  , vetChip :: Maybe G.ChipView
  -- brief true after the CAPTURE hotkey fires, so the active tab pulses — a visible
  -- "key registered" cue (the hotkey needs page focus; the pulse tells you it got it).
  , captureFlash :: Boolean
  -- single-flight guard for the 100ms Vetula poll, so it can't pile up queries
  -- against a still-initialising Vetula (see PollVetula).
  , pollBusy :: Boolean
  -- true once an Amphora fetch has failed (store unreachable) — drives the shell's
  -- "no favourites / backend not running" banner. Probed once on Init.
  , amphoraDown :: Boolean
  -- the status-board chip's recall menu: which machine's bank is open + its slots
  -- (each an alias the shell renders via glyphFromAlias). Nothing = closed. Fetched
  -- on open (AskBank), so it's a snapshot of the bank at click time.
  , chipMenu :: Maybe { w :: Which, items :: Array MenuItem }
  -- Scene grid (Ableton-like sequencer): the rig-wide grid + its transport. `scenes`
  -- is the ordered list of scenes (each a tuple of glyph aliases across machines);
  -- `sceneRun` runs the bar-quantized auto-advance; `scenePos` is the last-launched
  -- row (-1 = none, for the highlight); `sceneBars` = bars per scene; `sceneStep`
  -- is the last global bar-step applied (advance only at a boundary). `scenePick`
  -- is the open per-cell bank picker (Nothing = closed).
  , scenes :: Array Scenes.Scene
  , sceneRun :: Boolean
  , scenePos :: Int
  , sceneBars :: Int
  , sceneStep :: Int
  , scenePick :: Maybe { scene :: Int, machine :: Int, items :: Array MenuItem } }

-- One preset in the recall menu: its glyph alias + optional name + star flag.
type MenuItem = { slot :: Int, alias :: String, name :: String, starred :: Boolean }

type Slots =
  ( odo :: H.Slot SQ.Query Odonus.Output Unit
  , bal :: H.Slot SQ.Query Balistes.Output Unit
  , sel :: H.Slot SQ.Query Selene.Output Unit
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
        , macroLanes: Map.empty, macroReadout: Map.empty, macroBars: 4, macroOn: false, macroStep: -1, laneComplete: Nothing
        , ctxScaleKey: "", balChip: Nothing, selChip: Nothing, odoChip: Nothing, vetChip: Nothing, captureFlash: false
        , pollBusy: false, amphoraDown: false, chipMenu: Nothing
        , scenes: [], sceneRun: false, scenePos: -1, sceneBars: 4, sceneStep: -1, scenePick: Nothing }
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
    -- The scene clock: same ~8×/s bar-boundary poll for the Ableton-like grid's
    -- auto-advance (SceneTick is a no-op while the grid isn't running).
    _ <- liftEffect $ setInterval 120 (HS.notify listener SceneTick)
    -- Restore the saved scene grid (rig-wide). Playback is NOT restored (sceneRun
    -- stays false) — a reload never auto-plays, mirroring the machines.
    msc <- liftEffect ScenesStore.load
    for_ msc \sv -> H.modify_ _ { scenes = sv.scenes }
    -- Restore the saved macro-tidal lanes + bars-per-step (macroOn stays false).
    mmac <- liftEffect MacroStore.load
    for_ mmac \sv -> H.modify_ _
      { macroLanes = Map.fromFoldable (mapMaybe (\e -> (\w -> Tuple w e.text) <$> whichFromLane e.machine) sv.lanes)
      , macroBars = if sv.bars >= 1 then sv.bars else 4 }
    -- The global CAPTURE hotkey: one window-level keydown listener (the "same key
    -- on every pane" binding) → CaptureKey, which routes to the active machine.
    -- Guarded so it never fires while typing in a text field.
    target <- liftEffect $ Window.toEventTarget <$> window
    _ <- H.subscribe $ eventListener KET.keydown target keyToAction
    handleAction SyncTick
    -- One source of truth: push each machine its derived Sounding (all Silent now —
    -- nothing armed). Arm/mode changes re-derive and re-push; the instruments
    -- edge-detect their own local-mute / rig-handoff transitions. Forked so a slow
    -- child initialize can't block the shell's action queue during startup.
    void $ H.fork pushAll
    -- Probe Amphora once so the offline banner appears within the fetch timeout if
    -- the store is down. Forked — the shell must not wait on it.
    void $ H.fork fetchGoTo
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
  -- Forked so a still-initialising child (Vetula's lattice build can take tens of
  -- seconds) can't stall the shell's action queue on the `H.query`. A blocked queue
  -- means keydowns/clicks don't register until every child is ready — the CAPTURE
  -- hotkey "dead for a minute" bug. The queries land whenever the children answer.
  SyncTick -> void $ H.fork do
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
  -- Balistes pushed a new identity-chip view (capture / recall / divergence) — park it
  -- for the status board. Cheap: Balistes only raises this when the view changed.
  BalChipChanged cv -> H.modify_ _ { balChip = cv }
  SelChipChanged cv -> H.modify_ _ { selChip = cv }
  OdoChipChanged cv -> H.modify_ _ { odoChip = cv }
  -- The CAPTURE hotkey: tell the active machine to bank its current state as a
  -- preset. Only the SQ.Query machines answer; Balistes is the only live one so far.
  CaptureKey -> do
    -- Pulse FIRST, and independent of the child query: a child `H.query` blocks
    -- until that child has finished initializing (tens of seconds at cold start
    -- for the heavy panes), and doing it before the flash made the hotkey look
    -- dead for ~a minute after load. Pulse now (visible "key registered" cue),
    -- then fork the actual capture so it lands whenever the machine is ready.
    H.modify_ _ { captureFlash = true }
    void $ H.fork do
      H.liftAff (delay (Milliseconds 260.0))
      H.modify_ _ { captureFlash = false }
    w <- H.gets _.which
    void $ H.fork case w of
      Odo -> void $ H.query _odo unit (SQ.Capture unit)
      Bal -> void $ H.query _bal unit (SQ.Capture unit)
      Sel -> void $ H.query _sel unit (SQ.Capture unit)
      Vet -> void $ H.query _vet unit (Vetula.Capture unit)
      _ -> pure unit
  -- Click a status-board glyph: toggle its recall menu. On open, snapshot the
  -- machine's bank (AskBank) so the menu lists its presets as glyphs.
  OpenChipMenu w -> do
    open <- H.gets _.chipMenu
    case open of
      Just m | m.w == w -> H.modify_ _ { chipMenu = Nothing }
      _ -> refreshChipMenu w
  CloseChipMenu -> H.modify_ _ { chipMenu = Nothing }
  RecallFrom w slot -> do
    _ <- queryRecall w slot
    H.modify_ _ { chipMenu = Nothing }
  StarFrom w slot -> do
    _ <- queryStar w slot
    refreshChipMenu w
  DeleteFrom w slot -> do
    _ <- queryDelete w slot
    refreshChipMenu w
  -- macro-tidal: edit one machine's lane / the shared bars-per-step.
  SetLaneText w t -> do
    H.modify_ \s -> s { macroLanes = Map.insert w t s.macroLanes }
    persistMacro
    -- `:`-completion: if the trailing token is a `:prefix`, open a scoped popup of
    -- that machine's bank glyphs whose alias matches; otherwise close it.
    case String.stripPrefix (String.Pattern ":") (trailingToken t) of
      Just prefix -> do
        items <- fromMaybe [] <$> queryBank w
        let matched = filter (\it -> String.contains (String.Pattern prefix) it.alias) items
        H.modify_ _ { laneComplete = Just { w, prefix, items: matched } }
      Nothing -> H.modify_ _ { laneComplete = Nothing }
  -- Accept a completion: replace the trailing `:prefix` token with the glyph alias
  -- (plus a trailing space so typing flows on), and close the popup.
  AcceptCompletion w alias -> do
    H.modify_ \s ->
      let cur = fromMaybe "" (Map.lookup w s.macroLanes)
      in s { macroLanes = Map.insert w (replaceTrailingToken cur alias <> " ") s.macroLanes, laneComplete = Nothing }
    persistMacro
  CloseCompletion -> H.modify_ _ { laneComplete = Nothing }
  SetMacroBars v -> case Int.fromString v of
    Just n | n >= 1 -> do
      H.modify_ _ { macroBars = n }
      persistMacro
    _ -> pure unit
  -- Run / stop the sequencer. Turning ON resets `macroStep` to -1 so the next tick
  -- applies the current step at once. (No library re-gather — lanes now resolve
  -- against the local preset banks by glyph alias, not Amphora scene names.)
  ToggleMacro -> do
    on <- H.gets _.macroOn
    if on
      then H.modify_ _ { macroOn = false }
      else H.modify_ _ { macroOn = true, macroStep = -1 }
  -- The bar-quantized clock. Compute the current global step from the shared
  -- free-run epoch; when it crosses a boundary, resolve + apply EACH machine's lane
  -- at its own token count (so lanes of different lengths phase polymetrically).
  -- Rig-locked timing (reading the Link anchor) is a later slice — this drives Solo.
  MacroTick -> do
    st <- H.get
    when (st.macroOn && st.macroBars > 0) do
      now <- liftEffect dateNow
      let barMs = 4.0 * 60000.0 / freeTempo
          epochMs = st.freeT0 / 1000.0
          barIdx = max 0 (Int.floor ((now - epochMs) / barMs))
          stepGlobal = barIdx `div` st.macroBars
      when (stepGlobal /= st.macroStep) do
        H.modify_ _ { macroStep = stepGlobal }
        for_ Scenes.sceneMachines \w -> do
          let toks = parseLane (fromMaybe "" (Map.lookup w st.macroLanes))
              n = length toks
          when (n > 0) (applyLaneCell w (resolveStep toks (stepGlobal `mod` n) (stepGlobal `div` n)))
  -- Scene grid (Ableton-like). Snapshot the rig: read every machine's CURRENT chip
  -- glyph (the alias it's parked on) into a new scene row. A machine with no chip
  -- (nothing captured) contributes a leave-as-is cell. The capture-hotkey ethos at
  -- rig level — get it sounding right, bank the whole tuple in one gesture.
  AddSceneFromRig -> do
    st <- H.get
    let cells = map (\w -> _.alias <<< _.glyph <$> chipOf st w) Scenes.sceneMachines
    H.modify_ \s -> s { scenes = s.scenes <> [ { name: Nothing, cells } ] }
    persistScenes
  LaunchScene i -> launchScene i
  DeleteScene i -> do
    H.modify_ \s -> s { scenes = fromMaybe s.scenes (deleteAt i s.scenes), scenePos = if s.scenePos == i then -1 else s.scenePos }
    persistScenes
  SetSceneName i name -> do
    H.modify_ \s -> s
      { scenes = fromMaybe s.scenes
          (modifyAt i (_ { name = if name == "" then Nothing else Just name }) s.scenes) }
    persistScenes
  -- click a cell → open that machine's bank as a picker, so a glyph can be assigned.
  OpenCellPick sceneIx machineIx -> case Scenes.sceneMachines !! machineIx of
    Nothing -> pure unit
    Just w -> do
      items <- fromMaybe [] <$> queryBank w
      H.modify_ _ { scenePick = Just { scene: sceneIx, machine: machineIx, items } }
  CloseCellPick -> H.modify_ _ { scenePick = Nothing }
  SetSceneCell sceneIx machineIx mAlias -> do
    H.modify_ \s -> s
      { scenes = fromMaybe s.scenes (modifyAt sceneIx (Scenes.setCellAt machineIx mAlias) s.scenes)
      , scenePick = Nothing }
    persistScenes
  ToggleSceneRun ->
    H.modify_ \s -> if s.sceneRun then s { sceneRun = false } else s { sceneRun = true, sceneStep = -1 }
  SetSceneBars v -> case Int.fromString v of
    Just n | n >= 1 -> H.modify_ _ { sceneBars = n }
    _ -> pure unit
  -- The scene bar-clock: like MacroTick, compute the current global bar-step from
  -- the shared free-run epoch; when it crosses a boundary, launch the next scene.
  SceneTick -> do
    st <- H.get
    when (st.sceneRun && length st.scenes > 0) do
      now <- liftEffect dateNow
      let barMs = 4.0 * 60000.0 / freeTempo
          epochMs = st.freeT0 / 1000.0
          barIdx = max 0 (Int.floor ((now - epochMs) / barMs))
          stepGlobal = barIdx `div` max 1 st.sceneBars
      when (stepGlobal /= st.sceneStep) do
        H.modify_ _ { sceneStep = stepGlobal }
        launchScene (stepGlobal `mod` length st.scenes)
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
  -- Forked + single-flighted (`pollBusy`): the 100ms poll queries Vetula, which
  -- blocks until Vetula finishes initialising (its lattice build can take tens of
  -- seconds). Running it inline held the shell's action queue that whole time, so
  -- keydowns/clicks didn't register until Vetula was ready — the CAPTURE-hotkey
  -- "dead for a minute" bug. The fork frees the queue; the guard stops the poll
  -- piling up ~one query per 100ms against the not-yet-ready child.
  PollVetula -> do
    busy <- H.gets _.pollBusy
    unless busy do
      H.modify_ _ { pollBusy = true }
      void $ H.fork do
        -- Pull Vetula's progression + playhead for the nav harmonic-context strip.
        mharm <- H.query _vet unit (Vetula.AskHarmonic identity)
        case mharm of
          Just h -> H.modify_ _ { harm = h }
          Nothing -> pure unit
        -- Pull Vetula's identity chip for the status board (Vetula has no continuous
        -- frame loop to push it, so it rides this existing 100ms poll). `Nothing` (no
        -- answer) leaves the last chip; an answer of Nothing clears it (nothing parked).
        mchip <- H.query _vet unit (Vetula.AskChip identity)
        for_ mchip \cv -> H.modify_ _ { vetChip = cv }
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
        H.modify_ _ { pollBusy = false }

-- Push one machine its DERIVED Sounding (soundingOf mode armed). The instrument
-- edge-detects the transition itself: local-mute on leaving Local, rig handoff on
-- entering Rig, rig-stop on leaving Rig. This one call replaces the old
-- broadcastMaster / broadcastAudible / broadcastSyncToRig / broadcastStopRig /
-- reconcileRig — the shell no longer tracks a separate rig-running mirror.
pushSounding :: forall o m. MonadAff m => Which -> H.HalogenM RState RAction Slots o m Unit
pushSounding w = do
  st <- H.get
  void $ querySounding w (soundingOf st.mode st.armed (previewSet st) w)

-- macro-tidal: enact one resolved lane step on machine `w`. A glyph-alias token
-- recalls that preset (by alias, from the machine's bank) and ARMS the machine so
-- it sounds; a `~` rest (or the silent branch of an alternation) DISARMS it —
-- silence, the Tidal-like reading (vs the scene grid's leave-as-is). An unresolved
-- alias (its preset was deleted) is held and flagged in the readout. The sequencer
-- thus owns each machine's arm — scene-scheduling lifted up out of the instrument.
applyLaneCell :: forall o m. MonadAff m => Which -> Cell -> H.HalogenM RState RAction Slots o m Unit
applyLaneCell w = case _ of
  Quiet -> do
    a <- H.gets _.armed
    when (Set.member w a) do
      H.modify_ _ { armed = Set.delete w a }
      pushSounding w
    setLaneReadout w "~"
  Load alias mods -> do
    ok <- recallAlias w alias
    if ok then do
      a <- H.gets _.armed
      when (not (Set.member w a)) (H.modify_ _ { armed = Set.insert w a })
      for_ mods applyMod   -- apply the transform stack (e.g. `# scale`) to the form
      pushSounding w
      setLaneReadout w (alias <> joinWith "" (map (\md -> " #" <> md.verb) mods))
    else setLaneReadout w (alias <> " ?")

-- Record one lane's current-token label for its live readout.
setLaneReadout :: forall o m. Which -> String -> H.HalogenM RState RAction Slots o m Unit
setLaneReadout w s = H.modify_ \st -> st { macroReadout = Map.insert w s st.macroReadout }

-- Recall a machine's preset by glyph alias: ask its bank for the matching slot
-- (alias = stable content identity), then RecallSlot it. `true` iff it resolved.
-- Shared by the macro lanes and the scene grid launch.
recallAlias :: forall o m. Which -> String -> H.HalogenM RState RAction Slots o m Boolean
recallAlias w alias = do
  mbank <- queryBank w
  case mbank >>= (\bank -> _.slot <$> find (\it -> it.alias == alias) bank) of
    Just slot -> queryRecall w slot $> true
    Nothing -> pure false

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

-- Ask a machine for its bank (recall menu contents). Only the SQ.Query machines
-- answer; Balistes is the only one with real presets so far.
queryBank :: forall o m. Which -> H.HalogenM RState RAction Slots o m (Maybe (Array MenuItem))
queryBank = case _ of
  Odo -> H.query _odo unit (SQ.AskBank identity)
  Bal -> H.query _bal unit (SQ.AskBank identity)
  Sel -> H.query _sel unit (SQ.AskBank identity)
  Vet -> H.query _vet unit (Vetula.AskBank identity)
  _ -> pure Nothing

-- Recall bank slot i on a machine (switch to it + restore).
queryRecall :: forall o m. Which -> Int -> H.HalogenM RState RAction Slots o m (Maybe Unit)
queryRecall w i = case w of
  Odo -> H.query _odo unit (SQ.RecallSlot i unit)
  Bal -> H.query _bal unit (SQ.RecallSlot i unit)
  Sel -> H.query _sel unit (SQ.RecallSlot i unit)
  Vet -> H.query _vet unit (Vetula.RecallSlot i unit)
  _ -> pure Nothing

-- Toggle a preset's star / delete a preset on a machine.
queryStar :: forall o m. Which -> Int -> H.HalogenM RState RAction Slots o m (Maybe Unit)
queryStar w i = case w of
  Odo -> H.query _odo unit (SQ.StarSlot i unit)
  Bal -> H.query _bal unit (SQ.StarSlot i unit)
  Sel -> H.query _sel unit (SQ.StarSlot i unit)
  Vet -> H.query _vet unit (Vetula.StarSlot i unit)
  _ -> pure Nothing

queryDelete :: forall o m. Which -> Int -> H.HalogenM RState RAction Slots o m (Maybe Unit)
queryDelete w i = case w of
  Odo -> H.query _odo unit (SQ.DeleteSlot i unit)
  Bal -> H.query _bal unit (SQ.DeleteSlot i unit)
  Sel -> H.query _sel unit (SQ.DeleteSlot i unit)
  Vet -> H.query _vet unit (Vetula.DeleteSlot i unit)
  _ -> pure Nothing

-- (Re)load a machine's bank into the open recall menu — after open / star / delete.
refreshChipMenu :: forall o m. Which -> H.HalogenM RState RAction Slots o m Unit
refreshChipMenu w = do
  items <- fromMaybe [] <$> queryBank w
  H.modify_ _ { chipMenu = Just { w, items } }

-- Launch scene `i`: recall each non-empty cell on its machine. For a cell's glyph
-- alias, ask that machine's bank for the matching slot (aliases are stable content
-- identity), then RecallSlot it — content only (arming stays the tab-dots). An
-- alias with no match (its preset was deleted) is skipped. Sets scenePos for the
-- row highlight. Reuses queryBank/queryRecall — no new per-machine wiring.
launchScene :: forall o m. MonadAff m => Int -> H.HalogenM RState RAction Slots o m Unit
launchScene i = do
  st <- H.get
  case st.scenes !! i of
    Nothing -> pure unit
    Just sc -> do
      H.modify_ _ { scenePos = i }
      forWithIndex_ sc.cells \mIx mAlias -> case mAlias, Scenes.sceneMachines !! mIx of
        Just alias, Just w -> void (recallAlias w alias)
        _, _ -> pure unit

-- Persist the rig-wide scene grid after any edit.
persistScenes :: forall o m. MonadAff m => H.HalogenM RState RAction Slots o m Unit
persistScenes = do
  scs <- H.gets _.scenes
  liftEffect (ScenesStore.save { scenes: scs })

-- Persist the macro-tidal lanes (keyed by lane tag) + bars-per-step.
persistMacro :: forall o m. MonadAff m => H.HalogenM RState RAction Slots o m Unit
persistMacro = do
  s <- H.get
  let lanes = map (\(Tuple w t) -> { machine: laneLabel w, text: t }) (Map.toUnfoldable s.macroLanes)
  liftEffect (MacroStore.save { lanes, bars: s.macroBars })

-- The machine a lane tag names (inverse of `laneLabel`), for restoring saved lanes.
whichFromLane :: String -> Maybe Which
whichFromLane = case _ of
  "odo" -> Just Odo
  "bal" -> Just Bal
  "sel" -> Just Sel
  "vet" -> Just Vet
  _ -> Nothing

-- The last space-separated token of a lane (the one the caret is completing).
trailingToken :: String -> String
trailingToken t = fromMaybe "" (last (String.split (String.Pattern " ") t))

-- Replace a lane's trailing token with `alias` (the accepted completion).
replaceTrailingToken :: String -> String -> String
replaceTrailingToken cur alias =
  case unsnoc (String.split (String.Pattern " ") cur) of
    Just { init } -> joinWith " " (init <> [ alias ])
    Nothing -> alias

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
    Right items -> H.modify_ _ { goTo = items, amphoraDown = false }
    -- a failed fetch (now a ~2.5s timeout, not a 30s hang) means the store is
    -- unreachable → raise the shell banner so the user knows favourites are offline.
    Left _ -> H.modify_ _ { amphoraDown = true }

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
    , chipMenuPanel st
    , sceneCellPickPanel st
    -- All four are always in the tree (hence always mounted + running); the
    -- active one is shown, the rest are display:none but keep playing. On the
    -- TIDAL tab all four are hidden but still alive (and queryable). The three
    -- machine instruments inset their own root below the bar (position:fixed
    -- top:var(--tf-bar)); the in-flow Vetula pane is padded down to clear it.
    , pane (st.which == Odo) ""
        (HH.slot _odo unit Odonus.component unit (\(Odonus.IdentityChanged cv) -> OdoChipChanged cv))
    , pane (st.which == Bal) ""
        (HH.slot _bal unit Balistes.component unit (\(Balistes.IdentityChanged cv) -> BalChipChanged cv))
    , pane (st.which == Sel) ""
        (HH.slot _sel unit Selene.component unit (\(Selene.IdentityChanged cv) -> SelChipChanged cv))
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
    [ sceneGridPanel st
    , channelMapPanel st
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

-- macro-tidal — the Tidal-like sequencer: one mini-notation LANE per machine, over
-- glyph ALIASES ("owl-bomb star-ambulance ~"). Space-separated tokens divide the
-- lane's cycle into equal steps; `~` is a REST = silence (the machine disarms);
-- `<a b c>` alternates one inner form per cycle; `# scale <…>` re-quantises the rig
-- (via Vetula). Run it and each lane recalls its resolved preset into its machine
-- at each bar-step boundary (arming it), all sharing one pulse but each cycling at
-- its own token count — polymetric. Type a `:prefix` for a scoped glyph-completion
-- popup over that machine's bank (click to insert); each lane's steps render as a
-- PICTOGRAPHIC MIRROR of coloured glyph-pairs below the input.
macroPanel :: forall m. RState -> H.ComponentHTML RAction Slots m
macroPanel st =
  HH.div [ style "margin-bottom:26px;padding:14px 16px;background:#eef1ec;border:1px solid #d6ddd2;border-radius:6px" ]
    [ HH.div [ style "display:flex;align-items:baseline;justify-content:space-between;gap:14px;margin-bottom:9px" ]
        [ HH.span [ style "font-size:13px;letter-spacing:0.14em;text-transform:uppercase;color:#3f5a3f" ]
            [ HH.text "Arrangement — macro-tidal · per-machine lanes" ]
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
    , HH.div [ style "font-size:10px;color:#7a8a7a;margin-bottom:4px;font-family:'SF Mono',Menlo,Consolas,monospace" ]
        [ HH.text "~ rest = silence · <a b> alternate per cycle · # scale <\"F# lydian dominant\" \"G major\"> re-quantise" ]
    , HH.div_ (map (laneRow st) Scenes.sceneMachines)
    ]

-- One machine's lane: its tag + the mini-notation input + (when running) a step
-- readout with the current step lit and the live resolved token.
laneRow :: forall m. RState -> Which -> H.ComponentHTML RAction Slots m
laneRow st w =
  let
    text = fromMaybe "" (Map.lookup w st.macroLanes)
    toks = parseLane text
    n = length toks
    curStep = if st.macroOn && st.macroStep >= 0 && n > 0 then Just (st.macroStep `mod` n) else Nothing
    readout = fromMaybe "" (Map.lookup w st.macroReadout)
  in
    HH.div [ style "display:flex;align-items:flex-start;gap:10px;margin-top:9px" ]
      [ HH.span
          [ style "flex:0 0 38px;padding-top:8px;font-family:'SF Mono',Menlo,Consolas,monospace;font-size:12px;letter-spacing:0.06em;color:#5a6a5a" ]
          [ HH.text (laneLabel w) ]
      , HH.div [ style "flex:1 1 auto;min-width:0" ]
          [ HH.input
              [ HP.value text
              , HE.onValueInput (SetLaneText w)
              , HP.placeholder "owl-bomb star-ambulance ~ <owl-bomb star-ambulance>"
              , HP.spellcheck false
              , style $ "width:100%;box-sizing:border-box;padding:8px 11px;border:1px solid #b8c4b0;border-radius:5px;"
                  <> "background:#fffdf8;font-family:'SF Mono',Menlo,Consolas,monospace;font-size:13px;letter-spacing:0.02em;color:#22301f" ]
          -- the `:`-completion popup for THIS lane (emoji-picker over its bank)
          , case st.laneComplete of
              Just c | c.w == w -> completionPopup w c.prefix c.items
              _ -> HH.text ""
          -- the pictographic mirror: the parsed steps as GLYPHS, running step lit
          , if n == 0 then HH.text ""
            else HH.div [ style "display:flex;flex-wrap:wrap;align-items:center;gap:6px;margin-top:7px" ]
              ( mapWithIndex (laneStepChip curStep) toks
                  <> [ if st.macroOn && readout /= ""
                         then HH.span [ style "margin-left:6px;font-size:11px;color:#3d6b3d;font-style:italic" ]
                                [ HH.text ("♪ " <> readout <> "  · cycle " <> show (if n > 0 then st.macroStep `div` n else 0)) ]
                         else HH.text "" ] )
          ]
      ]

-- The `:`-completion popup: this machine's bank glyphs matching the typed prefix,
-- click to insert the alias (Tab-accept lands with the CodeMirror upgrade). Empty
-- match → a hint. Renders inline under the lane input.
completionPopup :: forall m. Which -> String -> Array MenuItem -> H.ComponentHTML RAction Slots m
completionPopup w prefix items =
  HH.div
    [ style $ "margin-top:4px;padding:5px;border:1px solid #a8b8a0;border-radius:6px;background:#f4f7f1;"
        <> "box-shadow:0 4px 12px #00000022;display:flex;flex-wrap:wrap;gap:4px;align-items:center" ]
    ( [ HH.span [ style "font-size:8px;letter-spacing:0.1em;text-transform:uppercase;color:#8a9a8a;margin-right:3px" ]
          [ HH.text (":" <> prefix) ] ]
        <>
          ( if null items
              then [ HH.span [ style "font-size:10px;color:#9aaa9a;font-style:italic" ] [ HH.text "no matching glyph in this bank" ] ]
              else map (completionItem w) items ) )

completionItem :: forall m. Which -> MenuItem -> H.ComponentHTML RAction Slots m
completionItem w item =
  let g = G.glyphFromAlias item.alias
  in HH.span
      [ HE.onClick \_ -> AcceptCompletion w item.alias
      , HP.attr (H.AttrName "title") item.alias
      , style $ "display:inline-flex;align-items:center;gap:4px;cursor:pointer;padding:2px 8px;border-radius:11px;"
          <> "border:1px solid #cbd8c4;background:#ffffff" ]
      [ HH.span [ style "display:inline-flex;align-items:center;gap:2px" ] [ faIcon g.first, faIcon g.second ]
      , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:9px;color:#4a5a4a" ] [ HH.text item.alias ] ]

-- The lowercase Tidal-style lane tag for a machine.
laneLabel :: Which -> String
laneLabel = case _ of
  Odo -> "odo"
  Bal -> "bal"
  Sel -> "sel"
  Vet -> "vet"
  _ -> "?"

-- One step in a lane's PICTOGRAPHIC MIRROR: a glyph-alias renders as its coloured
-- glyph-pair (+ the alias in small text), a `~` rest as a dash, an alternation
-- `<…>` as its text; the running step is lit. This is the arrangement-as-glyph-score
-- (docs/DESIGN-scene-modal.md). (An alias always renders SOME glyph — deterministic
-- from its text — so a typo shows a "wrong" glyph; unresolved-against-the-bank is
-- flagged at runtime in the live readout, "alias ?".)
laneStepChip :: forall m. Maybe Int -> Int -> Step -> H.ComponentHTML RAction Slots m
laneStepChip curStep i step =
  let live = curStep == Just i
      border = if live then "#4a7a4a" else "#c8d2c0"
      bg = if live then "linear-gradient(#dcecd6,#cde3c4)" else "#fbfdf9"
  in HH.span
    [ style $ "display:inline-flex;align-items:center;gap:5px;padding:3px 9px;border:1px solid " <> border
        <> ";border-radius:4px;background:" <> bg <> ";font-family:'SF Mono',Menlo,Consolas,monospace;font-size:12px;color:#2f4a2f" ]
    ( case step.form of
        FName alias ->
          let g = G.glyphFromAlias alias
          in [ HH.span [ style "display:inline-flex;align-items:center;gap:2px" ] [ faIcon g.first, faIcon g.second ]
             , HH.span [ style "font-size:10px;color:#5a6a5a" ] [ HH.text alias ] ]
        FRest -> [ HH.span [ style "color:#9aaa9a" ] [ HH.text "~" ] ]
        FAlt _ -> [ HH.text (stepLabel step) ] )

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
        ( [ modeToggle st ]
            <> (if st.amphoraDown then [ amphoraOfflinePill ] else [])
            <> [ harmStrip st ] )
    -- RIGHT: the master transport (arm-all / stop-all).
    , HH.button
        [ HE.onClick \_ -> ToggleMaster
        , style $ "flex:0 0 auto;padding:6px 18px;border:1px solid #00000033;border-radius:6px;cursor:pointer;"
            <> "font-size:11px;letter-spacing:0.16em;text-transform:uppercase;box-shadow:0 1px 3px #00000022;"
            <> "color:" <> (if anyArmed st.armed then "#fbeae7" else "#1c1a12")
            <> ";background:" <> (if anyArmed st.armed then "linear-gradient(#b23b28,#9a3120)" else "linear-gradient(#c8a86a,#b8975a)") ]
        [ HH.text (if anyArmed st.armed then "■ STOP" else "▶ PLAY") ]
    ]

-- The recall menu: a floating panel (escapes the bar's overflow via position:fixed)
-- listing the machine's banked presets as their coloured glyphs. Click one to
-- recall it. Opened by clicking a status-board glyph; closes on recall or re-click.
chipMenuPanel :: forall m. RState -> H.ComponentHTML RAction Slots m
chipMenuPanel st = case st.chipMenu of
  Nothing -> HH.text ""
  Just m ->
    HH.div
      [ style $ "position:fixed;top:calc(var(--tf-bar) + 5px);left:250px;z-index:60;box-sizing:border-box;"
          <> "background:#efece1;border:1px solid #a8a392;border-radius:8px;padding:7px;min-width:150px;"
          <> "box-shadow:0 6px 18px #00000033;display:flex;flex-direction:column;gap:3px;font-family:Georgia,serif" ]
      ( [ HH.div
            [ style "display:flex;align-items:center;justify-content:space-between;gap:12px;padding:2px 6px 5px" ]
            [ HH.span [ style "font-size:8px;letter-spacing:0.12em;color:#8a8676;text-transform:uppercase" ]
                [ HH.text ("Recall · " <> whichName m.w) ]
            , HH.span
                [ HE.onClick \_ -> CloseChipMenu
                , style "cursor:pointer;color:#8a8676;font-size:11px;line-height:1" ]
                [ HH.text "✕" ]
            ]
        ]
          <>
            ( if null m.items then
                [ HH.div [ style "padding:4px 8px;font-size:9px;color:#a09a88;font-style:italic" ]
                    [ HH.text "no presets yet" ] ]
              -- starred presets surface first (the go-to tier)
              else map (recallRow m.w) (filter _.starred m.items <> filter (not <<< _.starred) m.items)
            )
      )

recallRow :: forall m. Which -> MenuItem -> H.ComponentHTML RAction Slots m
recallRow w item =
  let
    g = G.glyphFromAlias item.alias
    label = if item.name == "" then item.alias else item.name
  in
    HH.div
      [ style "display:flex;align-items:center;gap:7px;padding:4px 6px;border-radius:5px;background:#e7e3d6" ]
      [ -- star toggle (the go-to tier)
        HH.span
          [ HE.onClick \_ -> StarFrom w item.slot
          , HP.attr (H.AttrName "title") (if item.starred then "unstar" else "star (go-to)")
          , style $ "cursor:pointer;font-size:12px;line-height:1;color:" <> (if item.starred then "#c9a23a" else "#c2beb0") ]
          [ HH.text (if item.starred then "★" else "☆") ]
      , -- glyph + label → recall
        HH.span
          [ HE.onClick \_ -> RecallFrom w item.slot
          , style "display:flex;align-items:center;gap:8px;cursor:pointer;flex:1 1 auto" ]
          [ HH.span [ style "display:inline-flex;align-items:center;gap:3px" ] [ faIcon g.first, faIcon g.second ]
          , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:9px;color:#4a463b" ] [ HH.text label ]
          ]
      , -- delete
        HH.span
          [ HE.onClick \_ -> DeleteFrom w item.slot
          , HP.attr (H.AttrName "title") "delete"
          , style "cursor:pointer;color:#b0a898;font-size:11px;line-height:1" ]
          [ HH.text "✕" ]
      ]

-- ─────────────────────────  Scene grid (Ableton-like)  ─────────────────────────
-- The rig-wide arrangement grid on the TIDAL page: rows = scenes, columns = the
-- live machines, cells = glyphs. Launching a row recalls its tuple across
-- machines (content only). Built on the same preset banks + glyph aliases the
-- chips use, so the grid IS a pictographic score. See docs/DESIGN-scene-modal.md.

-- The machine columns, aligned with Scenes.sceneMachines.
sceneColLabels :: Array String
sceneColLabels = [ "ODO", "BAL", "SEL", "VET" ]

sceneGridPanel :: forall m. RState -> H.ComponentHTML RAction Slots m
sceneGridPanel st =
  HH.div [ style "margin-bottom:26px;padding:14px 16px;background:#f3eee2;border:1px solid #e0d8c4;border-radius:6px" ]
    [ HH.div [ style "display:flex;align-items:baseline;justify-content:space-between;gap:14px;margin-bottom:12px" ]
        [ HH.span [ style "font-size:13px;letter-spacing:0.14em;text-transform:uppercase;color:#7a5c2a" ]
            [ HH.text "Scenes — the rig-wide grid" ]
        , sceneTransport st
        ]
    , if null st.scenes
        then HH.div [ style "font-size:11px;color:#9a8a6a;font-style:italic;padding:6px 2px" ]
               [ HH.text "No scenes yet — get the rig sounding how you want, then ‘+ scene from rig’ banks the whole tuple." ]
        else HH.div_ ( [ sceneHeaderRow ] <> mapWithIndex (sceneRow st) st.scenes )
    ]

-- The transport strip: the fast build gesture + the auto-advance controls.
sceneTransport :: forall m. RState -> H.ComponentHTML RAction Slots m
sceneTransport st =
  HH.div [ style "display:flex;align-items:center;gap:12px" ]
    [ HH.span
        [ HE.onClick \_ -> AddSceneFromRig
        , HP.title "snapshot every machine's current glyph into a new scene"
        , style $ "cursor:pointer;padding:4px 11px;border:1px solid #c9a23a;border-radius:5px;background:#fbf3df;"
            <> "font-size:10px;letter-spacing:0.06em;text-transform:uppercase;color:#7a5c00" ]
        [ HH.text "+ scene from rig" ]
    , HH.span [ style "font-size:9px;letter-spacing:0.08em;text-transform:uppercase;color:#9a8a6a" ] [ HH.text "bars/scene" ]
    , HH.input
        [ HP.value (show st.sceneBars)
        , HE.onValueInput SetSceneBars
        , style "width:40px;font-family:'SF Mono',Menlo,monospace;font-size:11px;padding:2px 5px;border-radius:4px;border:1px solid #d8cdb2;background:#fbf8f0;text-align:center;color:#3a3222" ]
    , HH.span
        [ HE.onClick \_ -> ToggleSceneRun
        , style $ "cursor:pointer;padding:4px 12px;border-radius:5px;font-size:10px;letter-spacing:0.06em;text-transform:uppercase;"
            <> (if st.sceneRun then "background:linear-gradient(#b8975a,#a8863f);color:#231c08;border:1px solid #8a6a20"
                else "background:#eee7d6;color:#6a5c3a;border:1px solid #cbbf9e") ]
        [ HH.text (if st.sceneRun then "❚❚ stop" else "▸ run") ]
    ]

-- The machine-label header, aligned to the cell columns below.
sceneHeaderRow :: forall m. H.ComponentHTML RAction Slots m
sceneHeaderRow =
  HH.div [ style "display:flex;align-items:center;gap:8px;padding-bottom:3px" ]
    ( [ HH.div [ style "flex:0 0 30px" ] []
      , HH.div [ style "flex:0 0 118px" ] []
      ]
        <> map (\lbl -> HH.div [ style "flex:0 0 64px;text-align:center;font-size:9px;letter-spacing:0.12em;color:#9a8a6a" ] [ HH.text lbl ]) sceneColLabels
        <> [ HH.div [ style "flex:0 0 22px" ] [] ]
    )

-- One scene row: launch caret + name field + the machine cells + delete.
sceneRow :: forall m. RState -> Int -> Scenes.Scene -> H.ComponentHTML RAction Slots m
sceneRow st i sc =
  let playing = st.scenePos == i
  in HH.div
      [ style $ "display:flex;align-items:center;gap:8px;padding:4px 0;border-top:1px solid #00000010"
          <> (if playing then ";background:#faf3df" else "") ]
      ( [ HH.span
            [ HE.onClick \_ -> LaunchScene i
            , HP.title "launch scene (recall the tuple)"
            , style $ "flex:0 0 30px;text-align:center;cursor:pointer;font-size:13px;color:"
                <> (if playing then "#b8860b" else "#a2916a") ]
            [ HH.text "▲" ]
        , HH.input
            [ HP.value (fromMaybe "" sc.name)
            , HP.placeholder ("scene " <> show (i + 1))
            , HE.onValueInput (SetSceneName i)
            , style "flex:0 0 118px;padding:3px 7px;border:1px solid #d8cdb2;border-radius:4px;background:#fbf8f0;font-family:Georgia,serif;font-size:11px;color:#3a3222" ]
        ]
          <> mapWithIndex (sceneCellView i) sc.cells
          <> [ HH.span
                 [ HE.onClick \_ -> DeleteScene i
                 , HP.title "delete scene"
                 , style "flex:0 0 22px;text-align:center;cursor:pointer;color:#b0a898;font-size:11px" ]
                 [ HH.text "✕" ] ]
      )

-- One cell: the machine's chosen glyph (or a leave-as-is dash), click to edit.
sceneCellView :: forall m. Int -> Int -> Scenes.SceneCell -> H.ComponentHTML RAction Slots m
sceneCellView sceneIx machineIx mAlias =
  HH.div
    [ HE.onClick \_ -> OpenCellPick sceneIx machineIx
    , HP.title "pick a glyph for this machine (or clear = leave as-is)"
    , style "flex:0 0 64px;height:30px;display:flex;align-items:center;justify-content:center;cursor:pointer;border-radius:5px;background:#ffffff66;border:1px solid #00000012" ]
    ( case mAlias of
        Nothing -> [ HH.span [ style "color:#c8bd9e;font-size:14px;line-height:1" ] [ HH.text "—" ] ]
        Just alias ->
          let g = G.glyphFromAlias alias
          in [ HH.span [ style "display:inline-flex;align-items:center;gap:2px" ] [ faIcon g.first, faIcon g.second ] ]
    )

-- The per-cell bank picker (floating): assign one of the machine's banked glyphs
-- to the clicked cell, or clear it back to leave-as-is. Snapshot of the bank at
-- open time (via AskBank), starred glyphs first — same idiom as the recall menu.
sceneCellPickPanel :: forall m. RState -> H.ComponentHTML RAction Slots m
sceneCellPickPanel st = case st.scenePick of
  Nothing -> HH.text ""
  Just p ->
    HH.div
      [ style $ "position:fixed;top:80px;left:50%;transform:translateX(-50%);z-index:70;box-sizing:border-box;"
          <> "background:#efece1;border:1px solid #a8a392;border-radius:8px;padding:8px;min-width:180px;max-height:70vh;overflow-y:auto;"
          <> "box-shadow:0 8px 24px #00000038;display:flex;flex-direction:column;gap:3px;font-family:Georgia,serif" ]
      ( [ HH.div
            [ style "display:flex;align-items:center;justify-content:space-between;gap:12px;padding:2px 6px 5px" ]
            [ HH.span [ style "font-size:8px;letter-spacing:0.12em;color:#8a8676;text-transform:uppercase" ]
                [ HH.text ("Set " <> fromMaybe "cell" (sceneColLabels !! p.machine)) ]
            , HH.span
                [ HE.onClick \_ -> CloseCellPick
                , style "cursor:pointer;color:#8a8676;font-size:11px;line-height:1" ]
                [ HH.text "✕" ]
            ]
        , -- leave-as-is (clear)
          HH.div
            [ HE.onClick \_ -> SetSceneCell p.scene p.machine Nothing
            , style "display:flex;align-items:center;gap:8px;padding:4px 6px;border-radius:5px;cursor:pointer;background:#e7e3d6" ]
            [ HH.span [ style "color:#a8a08c;font-size:13px;width:34px;text-align:center" ] [ HH.text "—" ]
            , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:9px;color:#7a746a" ] [ HH.text "leave as-is" ] ]
        ]
          <>
            ( if null p.items then
                [ HH.div [ style "padding:4px 8px;font-size:9px;color:#a09a88;font-style:italic" ]
                    [ HH.text "no presets on this machine yet" ] ]
              else map (scenePickRow p.scene p.machine) (filter _.starred p.items <> filter (not <<< _.starred) p.items)
            )
      )

scenePickRow :: forall m. Int -> Int -> MenuItem -> H.ComponentHTML RAction Slots m
scenePickRow sceneIx machineIx item =
  let
    g = G.glyphFromAlias item.alias
    label = if item.name == "" then item.alias else item.name
  in
    HH.div
      [ HE.onClick \_ -> SetSceneCell sceneIx machineIx (Just item.alias)
      , style "display:flex;align-items:center;gap:8px;padding:4px 6px;border-radius:5px;cursor:pointer;background:#e7e3d6" ]
      [ HH.span [ style $ "font-size:11px;width:12px;color:" <> (if item.starred then "#c9a23a" else "#d8d2c2") ] [ HH.text (if item.starred then "★" else "") ]
      , HH.span [ style "display:inline-flex;align-items:center;gap:3px" ] [ faIcon g.first, faIcon g.second ]
      , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:9px;color:#4a463b" ] [ HH.text label ]
      ]

-- A clear, non-blocking status pill shown when the Amphora store (:3024) is
-- unreachable: the app degrades to "no favourites" rather than hanging, and this
-- tells the user why (start Amphora to restore load/save of favourites).
amphoraOfflinePill :: forall m. H.ComponentHTML RAction Slots m
amphoraOfflinePill =
  HH.span
    [ HP.attr (H.AttrName "title") "Amphora artefact store (:3024) is not reachable — start it to load and save favourites"
    , style $ "flex:0 0 auto;display:flex;align-items:center;gap:6px;padding:3px 10px;border-radius:5px;white-space:nowrap;"
        <> "background:#f6e9cf;border:1px solid #d8b24a;color:#7a5c00;"
        <> "font-family:'SF Mono',Menlo,monospace;font-size:9px;letter-spacing:0.03em" ]
    [ HH.text "⚠ No favorites — Amphora backend not running" ]

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
      bg = if active
             then if st.captureFlash then "linear-gradient(#dcecc4,#b6d491)"  -- capture pulse
                  else "linear-gradient(#c8a86a,#b8975a)"
             else "linear-gradient(#e9e5d9,#dcd8c9)"
  in HH.div
      [ style ("display:flex;align-items:center;transition:background 240ms ease;background:" <> bg) ]
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
      -- the machine's identity glyph (icons coloured by content) — the six-machine
      -- status board. Clicking it opens the recall menu. Odonus + Balistes + Selene +
      -- Vetula report one; Suf/Ste are Nothing (blank, not clickable) — prototypes.
      , case chipOf st w of
          Nothing -> HH.text ""
          Just _ -> HH.span
            [ HE.onClick \_ -> OpenChipMenu w
            , HP.attr (H.AttrName "title") "recall a preset"
            , style "display:inline-flex;align-items:center;cursor:pointer" ]
            [ chipIcons (chipOf st w) ]
      ]

-- The chip view for a machine, from the shell's status-board state. Balistes is
-- the only live reporter for now; every other machine is empty until it gains the
-- glyph substrate (docs/DESIGN-scene-modal.md).
chipOf :: RState -> Which -> Maybe G.ChipView
chipOf st = case _ of
  Odo -> st.odoChip
  Bal -> st.balChip
  Sel -> st.selChip
  Vet -> st.vetChip
  _ -> Nothing

-- The CAPTURE hotkey — the same key on every pane. Modifier-free `c`, ignored while
-- a text field is focused (so it never fires mid-typing). Easy to rebind here.
captureKey :: String
captureKey = "c"

keyToAction :: E.Event -> Maybe RAction
keyToAction e = case KE.fromEvent e of
  Just ke
    | KE.key ke == captureKey
    , not (KE.ctrlKey ke || KE.metaKey ke || KE.altKey ke)
    , not (targetIsField e) -> Just CaptureKey
  _ -> Nothing

-- True when the event originated in a text input / textarea, so the hotkey yields
-- to typing (the eDSL source drawer, pattern-name fields, the channel-map inputs).
targetIsField :: E.Event -> Boolean
targetIsField e = case E.target e of
  Just t -> isJust (HInput.fromEventTarget t) || isJust (HTextArea.fromEventTarget t)
  Nothing -> false

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
