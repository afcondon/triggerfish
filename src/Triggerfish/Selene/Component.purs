-- | Triggerfish.Selene.Component — the polysignal rack, as a stack of
-- | destinations. Each destination is a group of eight signals (one generator
-- | kind) bound to a physical target; you add destinations in groups of eight
-- | and configure each in place.
-- |
-- | The visual language: every slot is drawn, not formed — LFOs as scaled,
-- | log-frequency waveforms; Euclids as step-rings; clocks + notes as numbers.
-- | The SOURCE pane is the editable authority.
-- |
-- | Selene is now the CV/gate rack only — POLYTRIG (the mini-notation drum lane)
-- | was relocated to Balistes' TIDAL tab, so this component has no audible output
-- | yet: the es9 CV/gate path (LFO/Euclid/Clock/Note → ES-9 buses) + FH-2
-- | delegation is the next increment (#142). The playhead still sweeps so the
-- | visuals stay live under the master transport.
module Triggerfish.Selene.Component (component, Output(..)) where

import Prelude

import Data.Array (deleteAt, drop, elemIndex, filter, findIndex, insertAt, length, mapMaybe, mapWithIndex, modifyAt, null, range, (!!))
import Data.Foldable (all, for_, foldl, foldr)
import Data.Tuple (Tuple(..), fst, snd)
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.Event.Event (Event, EventType(..), preventDefault, stopPropagation)
import Data.Int (round, toNumber)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.String as Str
import Data.String.Common (joinWith)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Timer (setInterval)
import Data.Either (hush)
import Data.Nullable (Nullable, toMaybe)
import Foreign.Object (Object)
import Foreign.Object as Object
import Simple.JSON (readJSON, writeJSON)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Binnacle as Binnacle
import Binnacle.Clock as Clock
import Binnacle.Midi as Midi
import Helm.Twister as HT
import Helm.Twister.Guard as Guard
import Helm.WebMIDI as WebMIDI
import Triggerfish.Selene.Twister as Tw
import Binnacle.Scheduler as Scheduler
import Binnacle.Transport as Transport
import Triggerfish.Ui.Style (engrave, style)
import Halogen.Widgets.Svg (svgAttr, svgEl, svgOn)
import Triggerfish.Ui.Euclid (Nudge(..))
import Triggerfish.Ui.Euclid as Euclid
import Triggerfish.Selene.EnvDraw as Draw
import Triggerfish.Selene.EnvLibrary as Lib
import Triggerfish.Selene.Model as M
import Triggerfish.Selene.Source as Source
import Triggerfish.Selene.Store as Store
import Triggerfish.Selene.Wire as Wire
import Triggerfish.Amphora as Amphora
import Triggerfish.Library as Library
import Triggerfish.Drag as Drop
import Triggerfish.Drag as Drag
import Control.Alt ((<|>))
import Data.Array as Array
import Reef.Selene.Line as Line
import Reef.Selene.Block as Block
import Triggerfish.Glyph as G
import Triggerfish.Browser as Browser
import Triggerfish.Preset (Preset, indexOfContent, presetAlias, newPreset)
import Triggerfish.SourceQuery (Query(..))
import Triggerfish.Transport (Sounding(..))
import Triggerfish.Rig (availableTargets, defaultRig)
import Halogen.Widgets.Select as Select
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Binnacle.Time (dateNow)

-- | The rack view hosts one cascade target-picker per destination, keyed by
-- | destination index — the same nested ES-9/FH-2/MIDI menu the routing modal
-- | uses (the machine's own copy of the shared control; see Triggerfish.Rig).
type Slots = ( selTarget :: Select.Slot Int )

-- ---------------------------------------------------------------------------
-- State / Actions
-- ---------------------------------------------------------------------------

-- | The SOURCE document is the authority for the rack: `sel` is its parsed
-- | projection. The doc is now drawn from a named **library** of racks (the
-- | active one is editable); `currentDoc` reads it. The rest is the transport
-- | (mirrors Balistes): an ARM flag that sounds only under the shell's master,
-- | the Binnacle clock + MIDI out, and the live clock readouts.
type State =
  { sel :: M.Selene
  , library :: Array Store.Rack   -- named racks; each `doc` is the rack rendered
  , active :: Int                 -- which rack is loaded + editable
  , sounding :: Sounding      -- the ONE transport value (MISU refactor): Silent | Local.
                              -- Selene has no rig voice, so it's Local whenever armed in
                              -- EITHER mode (frontend-authoritative) — never Rig.
  , playStep :: Int
  , binnacle :: Maybe Binnacle.Binnacle
  , midiOut :: Maybe Midi.MidiOut
  , midiName :: String
  , clockTempo :: Number
  , clockLocked :: Boolean
  , clockBar :: Int
  -- the daemons' replies to the last Apply → rig push, keyed "socket:bank"
  -- (e.g. "es9:main"). "…" while a push is in flight; the daemon's OK/ERR line
  -- once it answers. Drives the per-bank status readout (#142 S3).
  , replies :: Map String String
  -- what each bank was last sent (or adopted from the stage, which the rig
  -- has applied), keyed as `replies` is: an apply sends only the banks that
  -- differ. Every FH-2 apply rewrites the whole device, so re-sending the
  -- unchanged banks put three short dips on its jacks per edit (2026-10-16).
  , pushed :: Map String String
  -- when to apply the rack to the rig: a moment after the last edit, so the
  -- modular follows the page without an Apply (the rig keeps what it is given
  -- and gives it back to a daemon that returns: selene_keeper)
  , applyDue :: Maybe Number
  -- The rack on the stage (`selene/rack`), where the rig reads `selene $`
  -- lines and writes them back (docs/kb/plans/selene-in-tidal.md): whether
  -- this connection has subscribed, and the rack the stage was last seen to
  -- hold, so a frame repeating it changes nothing.
  , stageSubscribed :: Boolean
  , stageRack :: Maybe String
  -- Modules kept from a bank: a name and the line without its bank.
  , modules :: Array Module
  -- the racks and the kept modules as Amphora keeps them
  -- (`Triggerfish.Library`); Nothing until opened
  , rackShelf :: Maybe (Library.Shelf Store.Rack)
  , moduleShelf :: Maybe (Library.Shelf Module)
  -- The live rack: what the modular is given, edited by hand, by drops and
  -- by Limulus. The racks in the library are saved, and change only when
  -- saved over (AC, 2026-10-05: the rebus shows live diverging from saved).
  , live :: String
  , notice :: String
  , undoTo :: Maybe Undo
  -- a drag from the drawer over a group of outputs: which, whether it can
  -- take what is dragged, and if not, why
  , dragOver :: Maybe { wire :: String, ok :: Boolean, why :: String }
  -- the free banks drawn as two rows of four, so a module can be dropped on
  -- either half (docs/kb/plans/selene-split-banks.md); by target wire
  , splitFree :: Array String
  , publishMsg :: Maybe String   -- transient status from a publish-rack-to-Amphora click
  -- The unified glyph-chip PRESET bank (docs/DESIGN-scene-modal.md): captured rack
  -- docs, anonymous or named, freely intermixed. `identity` is the parked preset's
  -- content (the chip glyph; ghosts when the live doc diverges from it); `lastChip`
  -- guards the Frame → shell status-board emit so it only raises on change.
  , presets :: Array Preset
  , identity :: Maybe String
  , lastChip :: Maybe G.ChipView
  -- The slot under the keyboard/mouse editor: (destination index, slot 0..7),
  -- or Nothing. Clicking a drawn slot selects it (a black box); arrow keys then
  -- edit it in place. The SOURCE pane stays the read/compare surface.
  , selected :: Maybe Sel
  -- Which field of an envelope the arrow keys drive. A letter jumps to one
  -- (`a` `d` `s` `r` `t` `p` `v`); up/down cycle. Held on the component rather
  -- than per-slot so the choice survives moving between slots — you are usually
  -- adjusting the SAME parameter across several envelopes.
  , envParam :: EnvParam
  -- Cursor into `EnvLibrary.starters`, advanced by `[` / `]`. Stateful rather
  -- than derived from the slot so that cycling still walks the list once the
  -- shape has been tweaked away from any library entry.
  , envPick :: Int
  -- An in-progress breakpoint drag. Delta-based from the grab point rather than
  -- absolute-position based: the curve's horizontal EXTENT is a log function of
  -- its own duration, so an absolute mapping would move the geometry under the
  -- cursor as you dragged it — a feedback loop. Deltas have no such loop, and
  -- with the sensitivity below the handle tracks the pointer closely enough to
  -- read as direct.
  , envDrag :: Maybe EnvDrag
  -- The Twister on its bank 3 (`Triggerfish.Selene.Twister`): the port, whether
  -- the selected signal is entered, the device's last reported position and
  -- what was last lit, per encoder.
  , twister :: { out :: Maybe Midi.MidiOut, open :: Boolean, heard :: Map Int Int, lit :: Map Int Tw.Look }
  }

-- | Which breakpoint of the drawn envelope is being dragged. Three handles, two
-- | axes, four parameters — and no vocabulary to learn, which is the whole
-- | argument for replacing the letter shortcuts with this.
data EnvHandle
  = HPeak      -- x: attack
  | HCorner    -- x: decay, y: sustain
  | HTail      -- x: release

derive instance eqEnvHandle :: Eq EnvHandle

type EnvDrag =
  { handle :: EnvHandle
  , x0 :: Int, y0 :: Int
  , a0 :: Int, d0 :: Int, s0 :: Int, r0 :: Int
  }

-- | The envelope field the arrows edit. Ordered as the ADSR reading order, then
-- | the three that shape the whole envelope rather than one stage.
data EnvParam = EPAttack | EPDecay | EPSustain | EPRelease | EPTime | EPDepth | EPVel

derive instance eqEnvParam :: Eq EnvParam

envParamLabel :: EnvParam -> String
envParamLabel = case _ of
  EPAttack -> "attack"
  EPDecay -> "decay"
  EPSustain -> "sustain"
  EPRelease -> "release"
  EPTime -> "time"
  EPDepth -> "depth"
  EPVel -> "vel"

envParamOrder :: Array EnvParam
envParamOrder = [ EPAttack, EPDecay, EPSustain, EPRelease, EPTime, EPDepth, EPVel ]

-- | The letter that jumps to each parameter. Deliberately plain letters with
-- | SHIFT as the only modifier anywhere in this scheme: on a Mac, option-a is
-- | `å`, option-s is `ß`, option-d is `∂`, so an alt-based scheme would fight
-- | the keyboard and need preventDefault everywhere — the same class of trap as
-- | `#` being option-3 on a UK layout (see `keyToAction` in the shell).
envParamForKey :: String -> Maybe EnvParam
envParamForKey = case _ of
  "a" -> Just EPAttack
  "d" -> Just EPDecay
  "s" -> Just EPSustain
  "r" -> Just EPRelease
  "t" -> Just EPTime
  "p" -> Just EPDepth
  "v" -> Just EPVel
  _ -> Nothing

-- | Which drawn slot the direct-manipulation editor is aimed at.
type Sel = { dest :: Int, slot :: Int }

data Action
  = Initialize
  | Step Scheduler.Tick
  | Frame
  | MidiReady (Maybe Midi.MidiOut) String
  | TwisterMsg HT.TwisterMsg      -- a gesture, after Helm's press-nudge guard
  | TwisterOut Midi.MidiOut
  | AddDest M.GenKind         -- append a template block (comment-safe)
  | SetDoc String             -- the whole editable document, verbatim
  | RetargetDest Int String   -- re-route destination i to a target wire (cascade menu)
  | SelectSlot Int Int        -- click a drawn slot (dest, slot): select it — or, if it's an
                              -- already-selected LFO, cycle its waveform
  | SlotKeyDown KeyboardEvent -- a keystroke on the focused slot cell (arrows nudge the field)
  | SelectRack Int            -- load a library rack into the editor
  | NewRack                   -- append a fresh empty rack + select it
  | SetRackName String        -- rename the active rack
  | ApplyToRig                -- push each changed modular destination to its daemon
  | SeleneReply String        -- a raw `selene-reply …` frame from the rig
  | DropOn Place Event        -- a module dropped on a group of outputs (or a run of them)
  | DragOverTarget Place Event
  | SplitDest Int             -- bank i, a whole bank, becomes its jacks 1-4; 5-8 go free
  | JoinDest Int              -- bank i, a run, grows back to the whole bank
  | SplitFree String          -- a free bank (by wire) drawn as two free rows of four
  | JoinFree String           -- and back to one
  | DragLeaveTarget
  | DragRackStart Event
  | KeepModule Int            -- keep bank i as a module
  | ToggleMute Int            -- mute bank i (its jacks to 0 V, the bank kept) or unmute it
  | SetRange Int String       -- bank i's output range, by its wire word ("" = the rig's default)
  | ToggleSlotMute Int Int ME.MouseEvent  -- mute output j of bank i (kept, sent silent) or unmute it
  | RemoveDest Int            -- take bank i off the rack and free its jacks (`selene $ off`)
  | RacksChanged              -- the racks changed in Amphora (another page)
  | ModulesChanged            -- the kept modules, likewise
  | PickStarter Int           -- apply library shape i to the selected envelope slot
  | EnvGrab EnvHandle Int Int -- mousedown on a breakpoint (handle, clientX, clientY)
  | EnvDragMove Int Int Int Boolean -- mousemove over the editing cell (x, y, buttons, shift)

-- | The upward message to the shell: Selene's identity-chip view (or `Nothing` when
-- | nothing is parked), for the six-machine status board. Raised from the Frame loop
-- | only when the view changes (see `chipViewOf`). Mirrors Balistes' Output.
data Output = IdentityChanged (Maybe G.ChipView)

component :: forall i m. MonadAff m => H.Component Query i Output m
component =
  H.mkComponent
    { initialState: \_ ->
        let doc = Source.printRack M.defaultSelene
        in
          { sel: Source.parseRack doc
          , library: [ { name: "rack 1", doc, id: "" } ], active: 0, live: doc
          , sounding: Silent, playStep: 0
          , binnacle: Nothing, midiOut: Nothing, midiName: "…"
          , clockTempo: 120.0, clockLocked: false, clockBar: 0
          , replies: Map.empty
          , pushed: Map.empty
          , applyDue: Nothing
          , stageSubscribed: false
          , stageRack: Nothing
          , rackShelf: Nothing
          , moduleShelf: Nothing
          , modules: []
          , notice: ""
          , undoTo: Nothing
          , dragOver: Nothing
          , splitFree: []
          , publishMsg: Nothing
          , presets: [], identity: Nothing, lastChip: Nothing
          , selected: Nothing
          , envParam: EPDecay
          , envPick: 0
          , envDrag: Nothing
          , twister: { out: Nothing, open: false, heard: Map.empty, lit: Map.empty }
          }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction, handleQuery = handleQuery, initialize = Just Initialize }
    }

handleQuery :: forall m a. MonadAff m => Query a -> H.HalogenM State Action Slots Output m (Maybe a)
handleQuery = case _ of
  -- The browser drawer (docs/kb/plans/the-deck.md, 2026-10-05): the racks,
  -- by name, the loaded one current. A rack is edited in place, so keeping
  -- copies the loaded rack as a new one, to carry on from.
  --
  -- Two kinds of thing (AC, 2026-10-05): RACKS, whole configurations tied to
  -- their outputs (a click replaces the live rack, with an undo); and
  -- MODULES, one bank's worth free of any outputs, dropped on a bank: the
  -- built-in blocks (Reef.Selene.Block) and the ones kept from a bank.
  AskBrowser reply -> do
    s <- H.get
    let
      -- the live rack cannot be deleted here: load another first
      -- a saved rack's rebus is its saved self; the live one diverging from it
      -- is said on its row (edited, and save) and in the page's header
      edited i = i == s.active && diverged s
      rack i r = { slot: i, name: r.name, icons: rackIcons r.doc, tag: if edited i then "edited" else "", current: i == s.active, section: "Racks", builtin: false, drag: "", hint: "Click: make it the live rack. Double-click: rename it."
                 , actions: (if edited i then [ "save" ] else []) <> [ "duplicate" ] <> (if i == s.active then [] else [ "delete" ]) }
      plain k kind = { slot: plainSlot + 1 + k, name: M.kindLabel kind, icons: [], tag: signalMark kind, current: false, section: "Modules", builtin: true, drag: moduleDrag s (Source.kindKeyword (M.freshBank kind)), hint: moduleHint, actions: [] }
        # \r -> r { icons = settingIcons (Source.kindKeyword (M.freshBank kind)) }
      -- the null signal: dropped on a group, frees it (`selene $ off`)
      free = { slot: plainSlot, name: "free", icons: [], tag: "plain", current: false, section: "Modules", builtin: true, drag: Drag.dragOf "selene-module" "off", hint: "Drag it onto a bank to free it, or into Limulus onto a selene line to free that bank.", actions: [] }
      block k b = { slot: builtinSlot + k, name: b.name, icons: settingIcons b.name, tag: signalMark b.kind, current: false, section: "Modules", builtin: true, drag: moduleDrag s b.name, hint: moduleHint, actions: [] }
      moduleHint = "Drag it onto a bank. Drag it into Limulus: onto a selene line it replaces what that bank plays; anywhere else it starts a line on a free bank that can take it."
      kept k m = { slot: keptSlot + k, name: m.name, icons: settingIcons m.line, tag: maybe "" signalMark (Line.moduleKind m.line), current: false, section: "Modules", builtin: false, drag: moduleDrag s m.line, hint: moduleHint, actions: [ "delete" ] }
    pure (Just (reply
      { title: "Selene", modes: false, keep: "keep rack", notice: s.notice
      , rows: mapWithIndex rack s.library <> [ free ] <> mapWithIndex plain M.allKinds <> mapWithIndex block Block.blocks <> mapWithIndex kept s.modules }))
  BrowserRecall i _ next -> do
    settleUndo
    s <- H.get
    -- a rack replaces the live one, which an undo gives back; a module is
    -- dragged onto a bank, not clicked
    when (i < builtinSlot && i /= s.active) do
      H.modify_ _ { undoTo = Just (UndoReplace { rack: s.active, live: s.live }), notice = "Replaced the rack with " <> fromMaybe "?" (map _.name (s.library !! i)) }
      handleAction (SelectRack i)
    pure (Just next)
  BrowserRename i name next -> do
    settleUndo
    if i < builtinSlot then do
      H.modify_ \s -> s { library = fromMaybe s.library (modifyAt i (_ { name = name }) s.library) }
      persist
    else when (i >= keptSlot) do
      s <- H.get
      keepModules (fromMaybe s.modules (modifyAt (i - keptSlot) (_ { name = name }) s.modules))
    pure (Just next)
  BrowserAction i act next -> do
    settleUndo
    s <- H.get
    case act of
      -- save the live rack over its saved one
      "save" | i == s.active -> do
        H.modify_ _ { library = setDocAt s.active s.live s.library }
        persist
      "duplicate" | i < builtinSlot -> do
        for_ (s.library !! i) \r -> H.modify_ _ { library = s.library <> [ { name: r.name <> " copy", doc: r.doc, id: "" } ] }
        persist
      "delete" | i < builtinSlot && i /= s.active -> for_ (s.library !! i) \r -> do
        H.modify_ _ { library = fromMaybe s.library (deleteAt i s.library), active = if i < s.active then s.active - 1 else s.active
                    , undoTo = Just (UndoRack i r), notice = "Deleted " <> r.name }
        persist
      "delete" | i >= keptSlot -> for_ (s.modules !! (i - keptSlot)) \m -> do
        keepModules (fromMaybe s.modules (deleteAt (i - keptSlot) s.modules))
        H.modify_ _ { undoTo = Just (UndoModule (i - keptSlot) m), notice = "Deleted " <> m.name }
      _ -> pure unit
    pure (Just next)
  -- the rack's rebus, dragged from the page onto the drawer: kept as a new rack
  -- no controls of its own in the shared bar (yet)
  AskBar _ -> pure Nothing
  BarAction _ next -> pure (Just next)
  BrowserDrop t next -> do
    when (t == "keep:rack") (void (handleQuery (BrowserKeep unit)))
    pure (Just next)
  BrowserUndo next -> do
    s <- H.get
    H.modify_ _ { undoTo = Nothing, notice = "" }
    for_ s.undoTo case _ of
      -- back to the rack it was, as it was live (edits and all)
      UndoReplace back -> do
        handleAction (SelectRack back.rack)
        H.modify_ _ { live = back.live, sel = Source.parseRack back.live }
        persist
      UndoRack at r -> do
        H.modify_ \t -> t { library = fromMaybe (t.library <> [ r ]) (insertAt at r t.library), active = if at <= t.active then t.active + 1 else t.active }
        persist
      UndoModule at m -> do
        t <- H.get
        keepModules (fromMaybe (t.modules <> [ m ]) (insertAt at m t.modules))
    pure (Just next)
  BrowserKeep next -> do
    settleUndo
    at <- liftEffect Browser.stamp
    H.modify_ \s ->
      let n = length s.library
      in s { library = s.library <> [ { name: "Selene · " <> at, doc: currentDoc s, id: "" } ], active = n }
    persist
    pure (Just next)
  -- No marks here: asked for its text, the shell falls back to AskSource.
  AskMarkText _ -> pure Nothing
  AddMarkSnapshot _ _ _ next -> pure (Just next)
  -- Selene doesn't emit notes: it PUBLISHES config to the daemons, and where a
  -- polysignal lands is its own doc's target. So the note-routing table is not
  -- its business — but folding Selene's targets into the same table is step 4b
  -- of docs/DESIGN-routing.md, at which point this stops being a no-op.
  SetRouting _ k -> pure (Just k)
  AskSource reply -> do
    s <- H.get
    pure (Just (reply (currentDoc s)))
  PutSource doc next -> do
    -- The write mirror of AskSource — same effect as the editor's SetDoc, so an
    -- edit from the routing modal round-trips through the active rack exactly as
    -- a keystroke in the Selene tab would.
    H.modify_ \s -> s { live = doc, sel = Source.parseRack doc }
    persist
    pure (Just next)
  -- Selene has no clock of its own (it ignores SyncFree); answer the default so
  -- the shell's BPM poll stays total.
  -- No stage axis yet, so URL routing to this machine stops at the machine
  -- segment (`#selene`). When it grows one, parse the segments here.
  SetStagePath _ next -> pure (Just next)
  -- No in-machine lane view yet; the rack-wide TIDAL page still owns this one.
  PutLane _ _ next -> pure (Just next)
  AskClock reply -> pure (Just (reply { tempo: 120.0, locked: false }))
  SyncFree startMicros tempo next -> do
    s <- H.get
    for_ s.binnacle \bin ->
      liftEffect (Clock.setFreeBaseline (Binnacle.clock bin) { startMicros, tempo })
    pure (Just next)
  -- The ONE transport query (control-surface MISU refactor). Selene has no rig
  -- voice, so it's only ever Silent or Local (the shell never sends Rig); no held
  -- notes to silence. Emission gates on `sounding == Local`.
  SetSounding s next -> do
    H.modify_ _ { sounding = s }
    pure (Just next)
  AskSounding reply -> do
    s <- H.get
    pure (Just (reply s.sounding))
  -- A5 library manager: each rack's `doc` IS its transferable eDSL text.
  AskLibrary reply -> do
    s <- H.get
    pure (Just (reply (map (\r -> { name: r.name, text: r.doc }) s.library)))
  LoadEntry i next -> do
    H.modify_ \s ->
      let doc = fromMaybe "" (map _.doc (s.library !! i))
      in s { active = i, live = doc, sel = Source.parseRack doc }
    persist
    pure (Just next)
  -- Import always succeeds (parseRack is total) — the manager routes here by an
  -- explicit target, so the user already chose Selene.
  ImportText txt reply -> do
    H.modify_ \s ->
      let n = length s.library
      in s { library = s.library <> [ { name: "imported " <> show (n + 1), doc: txt, id: "" } ], active = n
           , sel = Source.parseRack txt }
    persist
    pure (Just (reply true))
  -- The shell's CAPTURE hotkey: bank the active rack's doc as a preset and park
  -- identity on it (the chip shows the freshly-minted glyph, held). See captureNow.
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
    persist
    pure (Just next)
  DeleteSlot i next -> do
    H.modify_ \s -> s { presets = fromMaybe s.presets (deleteAt i s.presets) }
    persist
    pure (Just next)

-- | Bank the active rack's doc as a preset — the CAPTURE hotkey. DEDUPS by content
-- | (identical doc ⇒ identical glyph): already banked ⇒ just re-park `identity`;
-- | otherwise append an anonymous preset. Either way the chip shows the glyph held,
-- | and we persist. No-op only when the active rack doc is empty.
captureNow :: forall o m. MonadAff m => H.HalogenM State Action Slots o m Unit
captureNow = do
  s <- H.get
  let text = currentDoc s
  when (text /= "") do
    case indexOfContent text s.presets of
      Just _ -> H.modify_ _ { identity = Just text }
      Nothing -> H.modify_ \st -> st
        { presets = st.presets <> [ newPreset text Nothing ]
        , identity = Just text
        }
    persist

-- | Recall preset `i`: load its rack-doc content into the active rack (re-derive the
-- | rack from it), and park the chip on the preset's text (glyph SOLID; ghosts on
-- | later divergence). Mirrors Balistes' `recallPreset` for the SetDoc-shaped state.
recallPreset :: forall o m. MonadAff m => Int -> H.HalogenM State Action Slots o m Unit
recallPreset i = do
  st <- H.get
  case st.presets !! i of
    Nothing -> pure unit
    Just p -> do
      H.modify_ \s -> s
        { live = p.content
        , sel = Source.parseRack p.content
        , identity = Just p.content
        }
      persist

-- | The identity-chip view Selene reports to the shell's status board: the glyph of
-- | the parked rack-doc + whether the live doc has diverged from it (edited away).
-- | `Nothing` when nothing is parked.
chipViewOf :: State -> Maybe G.ChipView
chipViewOf s = case s.identity of
  Nothing -> Nothing
  Just text -> Just { glyph: G.glyphOf text, diverged: currentDoc s /= text }

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action Slots Output m Unit
handleAction = case _ of
  Initialize -> do
    bin <- liftEffect $ Binnacle.connect { url: rigUrl, tempo: 120.0 }
    -- Rig replies to the Apply → rig push (`selene-reply …`) arrive as non-anchor
    -- frames; route them through Halogen so the per-bank status can update.
    { emitter: replyE, listener: replyL } <- liftEffect HS.create
    _ <- H.subscribe replyE
    liftEffect $ Binnacle.onAppMessage bin \msg -> HS.notify replyL (SeleneReply msg)
    { emitter: stepE, listener: stepL } <- liftEffect HS.create
    _ <- H.subscribe stepE
    _ <- liftEffect $ Scheduler.startGrid (Binnacle.clock bin) gridCfg \tick ->
      HS.notify stepL (Step tick)
    { emitter: frameE, listener: frameL } <- liftEffect HS.create
    _ <- H.subscribe frameE
    _ <- liftEffect $ setInterval 33 (HS.notify frameL Frame)
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
        -- The Twister, through Helm's guard: a push always nudges its encoder,
        -- and COLUMN's push must not also move the column.
        feed <- Guard.guarded Guard.timing (HS.notify midiL <<< TwisterMsg)
        twIn <- Midi.findInput access twisterPortName
        for_ twIn \inp -> void $ Midi.onMessage inp \m ->
          for_ (HT.parse [ m.status, m.data1, m.data2 ]) feed
        twOut <- Midi.findOutput access twisterPortName
        for_ twOut (HS.notify midiL <<< TwisterOut)
      Nothing -> HS.notify midiL (MidiReady Nothing "unavailable")
    -- restore the saved rack library + preset bank (falls back to the default rack).
    msaved <- liftEffect Store.loadLibrary
    for_ msaved \sv -> do
      H.modify_ _ { presets = sv.presets }
      when (not (null sv.library)) do
        let a = if sv.active >= 0 && sv.active < length sv.library then sv.active else 0
            doc = fromMaybe "" (map _.doc (sv.library !! a))
        mlive <- liftEffect Store.loadLive
        let live = fromMaybe doc mlive
        H.modify_ _ { library = sv.library, active = a, live = live, sel = Source.parseRack live }
    H.modify_ _ { binnacle = Just bin }
    -- The racks and kept modules are what Amphora keeps (`Triggerfish.Library`):
    -- this browser's copy at once, Amphora's when it answers or changes
    -- elsewhere; the first time, what this browser kept and the old
    -- `selene-rack` / `selene-module` collections. Which rack is live stays
    -- in this browser.
    { emitter: rackE, listener: rackL } <- liftEffect HS.create
    _ <- H.subscribe (RacksChanged <$ rackE)
    -- (what this browser saved, never the default rack it opens with)
    racks <- liftEffect $ Library.open rackDoc (maybe [] _.library msaved) (HS.notify rackL unit)
    H.modify_ \s -> rerack racks.items s { rackShelf = Just racks.shelf }
    { emitter: moduleE, listener: moduleL } <- liftEffect HS.create
    _ <- H.subscribe (ModulesChanged <$ moduleE)
    modules <- liftEffect $ Library.open moduleDoc [] (HS.notify moduleL unit)
    H.modify_ _ { modules = modules.items, moduleShelf = Just modules.shelf }

  -- The step is not drawn (the rings have no playhead): writing it on every
  -- tick redrew the whole page for nothing seen (2026-10-05, AC: CPU).
  Step _ -> pure unit

  Frame -> do
    st <- H.get
    -- subscribe to the stage's texts once per connection
    for_ st.binnacle \bin -> do
      up <- liftEffect $ Transport.isConnected (Binnacle.socket bin)
      when (up && not st.stageSubscribed) do
        H.modify_ _ { stageSubscribed = true }
        liftEffect $ Transport.send (Binnacle.socket bin) "stage-text-subscribe"
      when (not up && st.stageSubscribed) (H.modify_ _ { stageSubscribed = false, stageRack = Nothing, pushed = Map.empty })
    -- an edit settled: apply it, as the Apply button does
    now <- liftEffect dateNow
    for_ st.applyDue \due -> when (now >= due && isJust st.binnacle) do
      H.modify_ _ { applyDue = Nothing }
      handleAction ApplyToRig
    -- the clock, written only when the bar it shows moves: written on every
    -- frame it redrew the whole page some 30 times a second
    for_ st.binnacle \bin -> do
      r <- liftEffect $ Clock.read (Binnacle.clock bin)
      when (r.bar /= st.clockBar) $
        H.modify_ _ { clockTempo = r.tempo, clockLocked = r.locked, clockBar = r.bar }
    -- Report the identity chip up to the shell's status board, but only when it
    -- actually changed (this fires ~30×/s) — capture/recall/divergence all land here.
    s2 <- H.get
    let cv = chipViewOf s2
    when (cv /= s2.lastChip) do
      H.modify_ _ { lastChip = cv }
      H.raise (IdentityChanged cv)
    paintTwister

  MidiReady mout nm -> H.modify_ _ { midiOut = mout, midiName = nm }

  -- The Twister's bank 3 is this page's; the other banks belong to Odonus.
  TwisterMsg msg -> case msg of
    HT.Turn k v | k.layer == Tw.layer -> do
      H.modify_ \s -> s { twister = s.twister { heard = Map.insert k.index v s.twister.heard } }
      st <- H.get
      for_ (join (twisterControls st !! k.index)) \c -> for_ (c.turn v) twisterAct
    HT.Press k | k.layer == Tw.layer -> do
      -- The guard swallowed the push's nudge, so the device sits a click from
      -- its ring: forget both, and the next frame puts the ring back.
      H.modify_ \s -> s { twister = s.twister { heard = Map.delete k.index s.twister.heard, lit = Map.delete k.index s.twister.lit } }
      st <- H.get
      for_ (join (twisterControls st !! k.index)) \c -> for_ c.press twisterAct
    _ -> pure unit

  TwisterOut out -> do
    H.modify_ \s -> s { twister = s.twister { out = Just out, lit = Map.empty } }
    paintTwister

  -- append a fresh block to the active rack's doc (so existing comments
  -- survive), then re-derive the rack from the new text.
  AddDest k -> do
    H.modify_ \s ->
      let block = Source.printDest { target: M.defaultTargetFor k, jacks: M.allJacks, range: Nothing, bank: M.freshBank k, retrig: Nothing, muted: false, mutedSlots: [] }
          doc = currentDoc s <> "\n\n" <> block
      in s { live = doc, sel = Source.parseRack doc }
    persist
  SetDoc doc -> do
    H.modify_ \s -> s { live = doc, sel = Source.parseRack doc }
    persist
  -- Re-route one destination from its cascade menu: retarget in the parsed rack,
  -- reprint, and re-derive — the same doc-as-authority round-trip as SetDoc (and
  -- as the routing modal's PutSource), so both surfaces stay in sync.
  RetargetDest i wire -> do
    H.modify_ \s ->
      let rack = Source.parseRack (currentDoc s)
          rack' = rack
            { destinations = mapWithIndex
                (\j d -> if j == i then d { target = Source.parseTarget wire } else d)
                rack.destinations }
          doc = Source.printRack rack'
      in s { live = doc, sel = Source.parseRack doc }
    persist
  -- Click a drawn slot. First click selects (the black box); re-clicking an
  -- already-selected LFO slot cycles its waveform (the one edit that reads best
  -- as a click rather than a key).
  SelectSlot d j -> do
    s <- H.get
    if s.selected == Just { dest: d, slot: j } && kindAt d s == Just M.KLfo
      then editDest d (onBank (cycleWaveBank j))
      else H.modify_ _ { selected = Just { dest: d, slot: j } }
  -- Arrow keys nudge the selected slot's field, per kind (see nudgeSlot). Printable
  -- keys (typed note/clock entry) are a later increment; ignored for now.
  SlotKeyDown ev -> do
    s <- H.get
    let k = KE.key ev
        lower = Str.toLower k
    for_ s.selected \{ dest, slot } -> case unit of
      _
        -- A letter aims the arrows at one envelope field. Harmless on the other
        -- kinds, which ignore `envParam` entirely.
        | Just p <- envParamForKey lower -> do
            liftEffect (preventDefault (KE.toEvent ev))
            H.modify_ _ { envParam = p }
        -- `[` / `]` walk the curated starter library on this slot. The list is
        -- ordered percussive → sustained → swell → inverted, so flicking through
        -- it is itself a continuum rather than a bag of presets.
        | k == "[" || k == "]" -> do
            liftEffect (preventDefault (KE.toEvent ev))
            let i = s.envPick + (if k == "]" then 1 else -1)
            H.modify_ _ { envPick = i }
            for_ (Lib.starterAt i) \st ->
              editDest dest (onBank (setEnvSlot slot st.slot))
        -- Alt (Option) + ←/→ slides an LFO's phase by a sixteenth of a cycle
        -- (Shift: a quarter); → moves the drawn wave right. Each apply resets
        -- an FH-2 LFO bank, so the jack moves by exactly what is drawn.
        | KE.altKey ev, Just dir <- dirOf k, dir == NLeft || dir == NRight, kindAt dest s == Just M.KLfo -> do
            liftEffect (preventDefault (KE.toEvent ev))
            let step = (if KE.shiftKey ev then 0.25 else 0.0625) * (if dir == NRight then -1.0 else 1.0)
            editDest dest (onBank (slidePhase slot step))
        | otherwise ->
            for_ (dirOf k) \dir -> do
              liftEffect (preventDefault (KE.toEvent ev))
              case dir, s.selected of
                -- Up/down cycle WHICH field the arrows drive, for envelopes only;
                -- every other kind keeps its existing two-axis nudge.
                _, _ | isEnvSlot s dest, dir == NUp || dir == NDown ->
                  H.modify_ _ { envParam = cycleParam (dir == NUp) s.envParam }
                _, _ ->
                  editDest dest (onBank (nudgeSlot s.envParam dir (KE.shiftKey ev) slot))
  -- Click a shape on the library wall: it becomes the selected slot. Keeps
  -- `envPick` in step so `[` / `]` carry on from where you clicked rather than
  -- jumping back to wherever the cursor had drifted to.
  PickStarter i -> do
    st <- H.get
    for_ st.selected \{ dest, slot } ->
      for_ (Lib.starterAt i) \lib -> do
        H.modify_ _ { envPick = i }
        editDest dest (onBank (setEnvSlot slot lib.slot))
  EnvGrab h x y -> do
    st <- H.get
    for_ st.selected \{ dest, slot } ->
      for_ (envSlotAt st dest slot) \sl ->
        H.modify_ _ { envDrag = Just
          { handle: h, x0: x, y0: y
          , a0: sl.attack, d0: sl.decay, s0: sl.sustain, r0: sl.release } }
  -- Buttons released ends the drag. Tracked on move rather than with a
  -- document-level mouseup listener, the same way Odonus's XY pad does it: one
  -- handler, no subscription to leak, and releasing outside the cell still ends
  -- it because the next move that arrives reports no buttons held.
  EnvDragMove x y buttons shift -> do
    st <- H.get
    if buttons == 0 then H.modify_ _ { envDrag = Nothing }
    else for_ st.envDrag \dg -> for_ st.selected \{ dest, slot } -> do
      let dx = x - dg.x0
          dy = y - dg.y0
          -- The drawing box is 96px wide and ~34 tall above the baseline;
          -- mapping that span onto the full 0..127 range makes a pixel worth
          -- ~1.3 units, so the handle sits about under the pointer. Shift
          -- quarters the sensitivity for placing an exact value.
          k = if shift then 4 else 1
          sx v = clamp 0 127 (v + (dx * 127) / (96 * k))
          sy v = clamp 0 127 (v - (dy * 127) / (34 * k))
      editDest dest (onBank (overEnvAt slot (applyDrag dg.handle { sx, sy } dg)))

  SelectRack i -> do
    H.modify_ \s ->
      let doc = fromMaybe "" (map _.doc (s.library !! i))
      in s { active = i, live = doc, sel = Source.parseRack doc }
    persist
  NewRack -> do
    H.modify_ \s ->
      let doc = Source.printRack M.defaultSelene
          n = length s.library
      in s { library = s.library <> [ { name: "rack " <> show (n + 1), doc, id: "" } ], active = n, live = doc, sel = Source.parseRack doc }
    persist
  SetRackName name -> do
    H.modify_ \s -> s { library = fromMaybe s.library (modifyAt s.active (_ { name = name }) s.library) }
    persist

  -- Apply → rig (#142 S3): push every modular destination's apply-polysignal
  -- envelope to its daemon over the rig WS. es9-daemon / fh2-config generate the
  -- CV autonomously from here (install-once, not per-tick). Each frame is
  -- `selene-apply <socket> <bank> <json>`; the reply lands async in SeleneReply.
  ApplyToRig -> do
    st <- H.get
    let envs = filter (\e -> Map.lookup (replyKey e.socket e.bank) st.pushed /= Just e.json)
                 (mapMaybe Wire.destinationEnvelope st.sel.destinations)
    H.modify_ \s -> s { pushed = foldr (\e m -> Map.insert (replyKey e.socket e.bank) e.json m) s.pushed envs }
    for_ st.binnacle \bin -> do
      for_ envs \e ->
        liftEffect $ Transport.send (Binnacle.socket bin)
          ("selene-apply " <> e.socket <> " " <> e.bank <> " " <> e.json)
      -- and the rack goes on the stage, where `selene $` lines read it
      publishRack
    -- mark every pushed bank pending; the daemon's OK/ERR replaces it.
    H.modify_ \s -> s { replies = foldr (\e m -> Map.insert (replyKey e.socket e.bank) "…" m) s.replies envs }

  -- A `selene-reply <socket> <bank> <status…>` frame — record the status against
  -- its bank. Non-matching frames (other verbs) are ignored.
  -- The stage's rack: on subscribing, the whole table (adopt the rack it
  -- holds, or give it this page's); later, a write elsewhere (a `selene $`
  -- line on the rig): adopt it. Adopting does not apply: the rig already has.
  -- A drop on a bank: the module (or block) goes to the rig as a line on
  -- that bank, applied there in one step; the page follows by the stage.
  -- Over a group of outputs: green if it can take what is dragged (the drop
  -- is let through), red with the reason if not (it is not).
  DragOverTarget place e -> do
    text <- moduleOfDrag <$> liftEffect Drop.currentDrag
    st0 <- H.get
    let
      target = place.target
      wire = M.placeWire place.target place.jacks
      used = Array.any (M.samePlace place) st0.sel.destinations
      verdict = case Line.moduleKind text of
        _ | text == "off" -> if used then { wire, ok: true, why: "" } else { wire, ok: false, why: "already free" }
        Just kind | Line.acceptsAt target place.jacks kind -> { wire, ok: true, why: "" }
        Just kind -> { wire, ok: false, why: Line.refusalAt target place.jacks kind }
        Nothing -> { wire, ok: false, why: "drag a module from the drawer" }
    when verdict.ok (liftEffect (Drop.allowDrop e))
    st <- H.get
    when (st.dragOver /= Just verdict) (H.modify_ _ { dragOver = Just verdict })
  DragLeaveTarget -> H.modify_ _ { dragOver = Nothing }
  DragRackStart e -> liftEffect (Drop.startDrag e "keep:rack")
  -- Dropped: the module goes to the rig as a line on that bank, replacing
  -- what was there (`# fresh`). A block always sets the whole bank. Option to
  -- merge is off (`mergeOnOption`) until merge means something.
  -- A drop on a run of a bank (a row of four) fills that run: the module's
  -- first slots, one per jack.
  DropOn place e -> do
    text <- liftEffect (Drop.dropText e)
    H.modify_ _ { dragOver = Nothing }
    st <- H.get
    let
      target = place.target
      wire = M.placeWire place.target place.jacks
    case Line.moduleKind text of
      -- the null signal: free the group (its outputs silenced on the rig)
      _ | text == "off" -> for_ st.binnacle \bin -> do
            liftEffect $ Transport.send (Binnacle.socket bin) ("tidal selene $ off " <> wire)
            H.modify_ _ { publishMsg = Just ("freeing " <> wire) }
      Nothing -> pure unit
      Just kind
        | not (Line.acceptsAt target place.jacks kind) -> H.modify_ _ { publishMsg = Just (wire <> ": " <> Line.refusalAt target place.jacks kind) }
        | otherwise -> for_ st.binnacle \bin -> do
            let
              isBlock = isJust (Array.head (Str.split (Str.Pattern " ") (Str.trim text)) >>= Block.blockNamed)
              merge = mergeOnOption && Drop.altHeld e
              line = Line.onPlace text target place.jacks <> (if isBlock || merge then "" else " # fresh")
            liftEffect $ Transport.send (Binnacle.socket bin) ("tidal selene $ " <> line)
            H.modify_ _ { publishMsg = Just ((if merge && not isBlock then "merged into " else "→ ") <> wire <> ": " <> text) }
  -- Split and join go to the rig as one line each (`selene $ split fh2_0`),
  -- which releases the old claim before applying the new one; the page
  -- follows by the stage. With no rig, they only change the text.
  SplitDest i -> reshape i "split" \d -> d
    { jacks = { first: 1, count: 4 }, bank = M.takeSlots 4 d.bank, mutedSlots = filter (_ < 4) d.mutedSlots }
  JoinDest i -> reshape i "join" \d -> d
    { jacks = M.allJacks, bank = M.widenBank d.jacks d.bank, mutedSlots = map (\k -> k + d.jacks.first - 1) d.mutedSlots }
  SplitFree wire -> H.modify_ \s -> s { splitFree = Array.nub (Array.snoc s.splitFree wire) }
  JoinFree wire -> H.modify_ \s -> s { splitFree = filter (_ /= wire) s.splitFree }
  ToggleMute i -> editDest i (\d -> d { muted = not d.muted })
  SetRange i w -> editDest i (\d -> d { range = M.rangeOfWire w })
  -- the badge sits on the cell, whose click selects it: this click is the mute's
  ToggleSlotMute i j e -> do
    liftEffect (stopPropagation (ME.toEvent e))
    editDest i \d -> d { mutedSlots = if Array.elem j d.mutedSlots then Array.delete j d.mutedSlots else Array.sort (Array.snoc d.mutedSlots j) }
  -- The same `off` the "free" module drops: the rig takes the bank off the
  -- rack, its daemon releases the claim (which silences the jacks), and the
  -- page follows by the stage. With no rig to tell, it only leaves the text.
  RemoveDest i -> do
    st <- H.get
    for_ (st.sel.destinations !! i) \d -> case st.binnacle of
      Just bin -> do
        liftEffect $ Transport.send (Binnacle.socket bin) ("tidal selene $ off " <> M.placeWire d.target d.jacks)
        H.modify_ _ { publishMsg = Just ("freeing " <> M.placeWire d.target d.jacks) }
      Nothing -> do
        H.modify_ \s ->
          let doc = Source.printRack (s.sel { destinations = fromMaybe s.sel.destinations (deleteAt i s.sel.destinations) })
          in s { live = doc, sel = Source.parseRack doc, selected = Nothing }
        persist
  KeepModule i -> do
    st <- H.get
    for_ (st.sel.destinations !! i) \d -> do
      at <- liftEffect Browser.stamp
      let m = { name: Source.kindKeyword d.bank <> " · " <> at, line: Line.moduleOf d, id: "" }
      keepModules (st.modules <> [ m ])
      H.modify_ _ { publishMsg = Just ("kept as a module: " <> m.name) }
  RacksChanged -> do
    st <- H.get
    for_ st.rackShelf \shelf -> do
      racks <- liftEffect (Library.current shelf)
      H.modify_ (rerack racks)
  ModulesChanged -> do
    st <- H.get
    for_ st.moduleShelf \shelf -> do
      ms <- liftEffect (Library.current shelf)
      H.modify_ _ { modules = ms }
  -- the rig's answer to a dropped module
  SeleneReply raw | Just rest <- Str.stripPrefix (Str.Pattern "OK: selene: ") raw -> H.modify_ _ { publishMsg = Just ("✓ " <> rest) }
  SeleneReply raw | Just rest <- Str.stripPrefix (Str.Pattern "ERR: selene: ") raw -> H.modify_ _ { publishMsg = Just ("✗ " <> rest) }
  SeleneReply raw | Just json <- Str.stripPrefix (Str.Pattern "stage-texts ") raw -> do
    case readStageTable json of
      Just t -> adoptRack t
      Nothing -> do
        H.modify_ _ { stageRack = Nothing }
        publishRack
  SeleneReply raw | Just json <- Str.stripPrefix (Str.Pattern "stage-text ") raw -> do
    for_ (readStageWrite json) adoptRack
  SeleneReply raw -> case Str.stripPrefix (Str.Pattern "selene-reply ") raw of
    Nothing -> pure unit
    Just rest ->
      let toks = Str.split (Str.Pattern " ") rest
      in case toks !! 0, toks !! 1 of
           Just socket, Just bank ->
             H.modify_ \s -> s { replies = Map.insert (replyKey socket bank) (joinWith " " (drop 2 toks)) s.replies }
           _, _ -> pure unit

-- | The active rack's eDSL doc — the editable text + the AskSource answer.
-- | The Twister's sixteen encoders as the page stands.
twisterControls :: State -> Array (Maybe Tw.Control)
twisterControls st = Tw.surface twisterOps st.sel { sel: st.selected, open: st.twister.open, rows: rowOrder st }

-- | The page's own slot arithmetic, so a turn steps exactly as the arrows do.
twisterOps :: Tw.Ops
twisterOps =
  { clockSpeeds: length clockSpeeds
  , clockSpeed: speedIndex
  , setClockSpeed: setSpeed
  , lfoShape: activeShape
  , lfoAmp: activeAmp
  , setLfoShape: setShape
  }

twisterAct :: forall m. MonadAff m => Tw.Act -> H.HalogenM State Action Slots Output m Unit
twisterAct = case _ of
  Tw.Edit d f -> editDest d (onBank f)
  Tw.Goto sel open -> H.modify_ \s -> s { selected = Just sel, twister = s.twister { open = open } }
  Tw.Mute d -> handleAction (ToggleMute d)
  Tw.Apply -> handleAction ApplyToRig

-- | Bring the device's rings and colours into line with the page. Writes, and
-- | touches the state, only when something differs: this runs every frame.
paintTwister :: forall m. MonadAff m => H.HalogenM State Action Slots Output m Unit
paintTwister = do
  st <- H.get
  for_ st.twister.out \out -> do
    let r = Tw.paint (twisterControls st) { heard: st.twister.heard, lit: st.twister.lit }
    unless (null r.writes) do
      liftEffect $ for_ r.writes \w -> WebMIDI.send out case w of
        Tw.Ring k p -> HT.ring k p
        Tw.Colour k h -> HT.colour k h
        Tw.Light k on -> HT.ringBrightness k (if on then 30 else 0) <> HT.colourBrightness k (if on then 30 else 0)
      H.modify_ \s -> s { twister = s.twister { lit = r.lit } }

twisterPortName :: String
twisterPortName = "Twister"

currentDoc :: State -> String
currentDoc s = s.live

-- | The saved rack the live one was loaded from (or last saved to).
savedDoc :: State -> String
savedDoc s = fromMaybe "" (map _.doc (s.library !! s.active))

-- | Whether the live rack has moved away from its saved one.
diverged :: State -> Boolean
diverged s = s.live /= savedDoc s

-- | Replace one rack's doc in the library.
setDocAt :: Int -> String -> Array Store.Rack -> Array Store.Rack
setDocAt i doc lib = fromMaybe lib (modifyAt i (_ { doc = doc }) lib)

-- | A module kept from a bank: its name and its line without the bank.
type Module = { name :: String, line :: String, id :: String }

-- | The racks as Amphora keeps them, at `triggerfish/selene/racks`; the
-- | first time, with the old `selene-rack` collection.
rackDoc :: Library.Doc Store.Rack
rackDoc =
  { ref: "triggerfish/selene/racks"
  , kind: "triggerfish-selene-racks"
  , idOf: _.id
  , withId: \id r -> r { id = id }
  , encode: writeJSON
  , decode: hush <<< readJSON
  , same: \a b -> a.name == b.name
  , first: map (\it -> { name: it.name, doc: it.payload, id: "" }) <$> Amphora.fetchCollection "selene-rack"
  , tidy: identity
  }

-- | The kept modules, at `triggerfish/selene/modules`; the first time, with
-- | the old `selene-module` collection.
moduleDoc :: Library.Doc Module
moduleDoc =
  { ref: "triggerfish/selene/modules"
  , kind: "triggerfish-selene-modules"
  , idOf: _.id
  , withId: \id m -> m { id = id }
  , encode: writeJSON
  , decode: hush <<< readJSON
  , same: \a b -> a.name == b.name
  , first: map (\it -> { name: it.name, line: it.payload, id: "" }) <$> Amphora.fetchCollection "selene-module"
  , tidy: identity
  }

-- | A new rack library, the live rack's saved one followed to where it now
-- | is (the first, if it went).
rerack :: Array Store.Rack -> State -> State
rerack racks s = s
  { library = racks
  , active = fromMaybe 0 ((s.library !! s.active) >>= \r -> findIndex (\x -> if r.id /= "" then x.id == r.id else x.name == r.name) racks)
  }

-- | The kept modules now: shown, and kept in Amphora.
keepModules :: forall o m. MonadAff m => Array Module -> H.HalogenM State Action Slots o m Unit
keepModules ms = do
  st <- H.get
  case st.moduleShelf of
    Nothing -> H.modify_ _ { modules = ms }
    Just shelf -> do
      kept <- liftEffect (Library.put shelf ms)
      H.modify_ _ { modules = kept }

-- | Option-drop merges a module into a row (its line without `# fresh`, so
-- | only what it names changes). Off for the MVP (AC, 2026-10-05): a real
-- | merge makes an output a stack of signals, a conceptual and UI question
-- | of its own, and a half-merge met first would shape it.
mergeOnOption :: Boolean
mergeOnOption = false


-- | Put this page's rack on the stage, if it is not what the stage holds.
publishRack :: forall o m. MonadAff m => H.HalogenM State Action Slots o m Unit
publishRack = do
  st <- H.get
  let doc = currentDoc st
  when (st.stageSubscribed && st.stageRack /= Just doc) do
    for_ st.binnacle \bin -> liftEffect $ Transport.send (Binnacle.socket bin) ("stage-text selene/rack " <> doc)
    H.modify_ _ { stageRack = Just doc }

-- | Take the stage's rack as the loaded one: the rig changed it (a `selene $`
-- | line), or it was there when this page opened. Kept locally, not applied.
adoptRack :: forall o m. MonadAff m => String -> H.HalogenM State Action Slots o m Unit
adoptRack doc = do
  H.modify_ _ { stageRack = Just doc, pushed = pushedOf (Source.parseRack doc) }
  st <- H.get
  when (doc /= currentDoc st) do
    H.modify_ \s -> s { live = doc, sel = Source.parseRack doc }
    s <- H.get
    liftEffect (Store.saveLibrary { active: s.active, library: s.library, presets: s.presets })

-- | Each bank of a rack as the rig has it applied, keyed as `replies` is.
pushedOf :: M.Selene -> Map String String
pushedOf sel = Map.fromFoldable (map (\e -> Tuple (replyKey e.socket e.bank) e.json) (mapMaybe Wire.destinationEnvelope sel.destinations))

-- | The rack in a `stage-texts` table, if it has one.
readStageTable :: String -> Maybe String
readStageTable json = do
  table :: Object { text :: String } <- hush (readJSON json)
  _.text <$> Object.lookup "selene/rack" table

-- | The rack in a `stage-text` write, if that is what was written.
readStageWrite :: String -> Maybe String
readStageWrite json = do
  w :: { key :: String, text :: Nullable String } <- hush (readJSON json)
  if w.key == "selene/rack" then toMaybe w.text else Nothing

-- | A setting's rebus: the coloured pair of the setting a module makes (its
-- | module form, Line.moduleText), the same in the drawer and on a row.
settingIcons :: String -> Array G.GlyphIcon
settingIcons m = maybe [] (\t -> (G.glyphOf t).icons) (Line.moduleText m)

-- | A rack's rebus: three icons in one dark ink, as Vetula draws a session.
rackIcons :: String -> Array G.GlyphIcon
rackIcons doc = map (_ { color = "#2a2a2a" }) (G.rackGlyphOf doc).icons

-- | What a module puts out, at a glance (AC, 2026-10-05): ∿ a continuous
-- | voltage (LFOs, notes, envelopes), ⎍ gates (rhythms, clocks).
signalMark :: M.GenKind -> String
signalMark = case _ of
  M.KEuclid -> "⎍ gate"
  M.KClock -> "⎍ gate"
  _ -> "∿ cv"

-- | Where the drawer's rows of each kind start: racks from 0, the built-in
-- | blocks, then kept modules.
builtinSlot :: Int
builtinSlot = 1000

-- | the plain modules (a fresh bank of each kind: what ADD used to give)
plainSlot :: Int
plainSlot = 1500

keptSlot :: Int
keptSlot = 2000

-- | Persist the rack library + preset bank (after any library/active/preset change).
persist :: forall o m. MonadAff m => H.HalogenM State Action Slots o m Unit
persist = do
  s0 <- H.get
  for_ s0.rackShelf \shelf -> do
    kept <- liftEffect (Library.put shelf s0.library)
    H.modify_ (rerack kept)
  s <- H.get
  liftEffect (Store.saveLibrary { active: s.active, library: s.library, presets: s.presets })
  liftEffect (Store.saveLive s.live)
  now <- liftEffect dateNow
  H.modify_ _ { applyDue = Just (now + 800.0) }

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- Transport constants (mirror Odonus/Balistes)
-- ---------------------------------------------------------------------------

rigUrl :: String
rigUrl = "ws://127.0.0.1:3012/ws"

-- | The `replies` map key for a pushed destination: "socket:bank" (e.g. "es9:main").
replyKey :: String -> String -> String
replyKey socket bank = socket <> ":" <> bank

gridCfg :: Scheduler.GridConfig
gridCfg = { stepBeats: 0.25, lookaheadMs: 120.0, tickMs: 25 }

-- | One Tidal cycle == one bar (16 sixteenths), so `x*4` is four hits on the
-- | beat. The absolute grid index is the pulse, so Selene shares the rig downbeat.
cycleSteps :: Int
cycleSteps = 16

midiPortName :: String
midiPortName = "IAC"

accent :: String
accent = "#3f6f8a"   -- steel-blue, Selene's electric accent

ink :: String
ink = "#2b2922"

-- ---------------------------------------------------------------------------
-- render
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- Direct-manipulation slot editing (mouse-select + arrow-key nudge)
-- ---------------------------------------------------------------------------

-- | Arrow direction. The convention: →/↑ increase, ←/↓ decrease. Defined once,
-- | in `Triggerfish.Ui.Euclid` — the Euclid rings are the shared control, and the
-- | other three slot kinds ride the same key vocabulary rather than a parallel one.
type NudgeDir = Euclid.Nudge

dirOf :: String -> Maybe NudgeDir
dirOf = Euclid.dirOf

-- | The generator kind of destination `d` (for the LFO re-click-to-cycle rule).
kindAt :: Int -> State -> Maybe M.GenKind
kindAt d s = map (M.bankKind <<< _.bank) (s.sel.destinations !! d)

-- | Apply a bank transform to one destination, then reprint → reparse → persist:
-- | the same doc-as-authority round-trip as RetargetDest / SetDoc, so the drawn
-- | slot, the SOURCE pane, and the routing modal stay in lockstep.
-- | Split or join bank i: on the rig, as a `selene $` line (the page follows
-- | by the stage); with no rig, the same change to the text.
reshape :: forall m. MonadAff m => Int -> String -> (M.Destination -> M.Destination) -> H.HalogenM State Action Slots Output m Unit
reshape i verb local = do
  st <- H.get
  for_ (st.sel.destinations !! i) \d -> case st.binnacle of
    Just bin -> do
      liftEffect $ Transport.send (Binnacle.socket bin) ("tidal selene $ " <> verb <> " " <> M.placeWire d.target d.jacks)
      H.modify_ _ { publishMsg = Just (verb <> " " <> M.placeWire d.target d.jacks) }
    Nothing -> editDest i local

editDest :: forall m. MonadAff m => Int -> (M.Destination -> M.Destination) -> H.HalogenM State Action Slots Output m Unit
editDest d f = do
  H.modify_ \s ->
    let dests' = mapWithIndex (\i dd -> if i == d then f dd else dd) s.sel.destinations
        doc = Source.printRack (s.sel { destinations = dests' })
    in s { live = doc, sel = Source.parseRack doc }
  persist

onBank :: (M.GenBank -> M.GenBank) -> M.Destination -> M.Destination
onBank g dd = dd { bank = g dd.bank }

overAt :: forall a. Int -> (a -> a) -> Array a -> Array a
overAt j f xs = fromMaybe xs (modifyAt j f xs)

-- | Nudge slot `j` of a bank by one arrow step; the field it moves is per-kind.
nudgeSlot :: EnvParam -> NudgeDir -> Boolean -> Int -> M.GenBank -> M.GenBank
nudgeSlot ep dir shift j = case _ of
  M.GLfo xs -> M.GLfo (overAt j (nudgeLfo dir shift) xs)
  M.GEuclid xs -> M.GEuclid (overAt j (nudgeEuclid dir shift) xs)
  M.GClock xs -> M.GClock (overAt j (nudgeClock dir shift) xs)
  M.GNote xs -> M.GNote (overAt j (nudgeNote dir shift) xs)
  M.GEnv xs -> M.GEnv (overAt j (nudgeEnv ep dir shift) xs)
  -- a voice: ↑/↓ its channel; ←/→ its envelope's field, as on an envelope
  M.GVoice vb -> M.GVoice vb { voices = overAt j (\v -> case dir of
      NUp -> v { channel = clamp 1 16 (v.channel + 1) }
      NDown -> v { channel = clamp 1 16 (v.channel - 1) }
      _ -> v { env = nudgeEnv ep dir shift v.env }) vb.voices }

-- | The envelope slot at (destination, slot), if that destination is one.
envSlotAt :: State -> Int -> Int -> Maybe M.EnvSlot
envSlotAt st dest slot = case map _.bank (st.sel.destinations !! dest) of
  Just (M.GEnv xs) -> xs !! slot
  _ -> Nothing

-- | Modify one slot of an envelope bank.
overEnvAt :: Int -> (M.EnvSlot -> M.EnvSlot) -> M.GenBank -> M.GenBank
overEnvAt j f = case _ of
  M.GEnv xs -> M.GEnv (overAt j f xs)
  other -> other

-- | Apply a drag to the slot, from the values captured when the handle was
-- | grabbed. Reading from `dg` rather than from the live slot is what makes the
-- | gesture absolute: the result depends only on how far you have moved, so
-- | dragging back to where you started restores exactly what you had.
applyDrag
  :: EnvHandle
  -> { sx :: Int -> Int, sy :: Int -> Int }
  -> EnvDrag
  -> M.EnvSlot
  -> M.EnvSlot
applyDrag h f dg sl = case h of
  HPeak -> sl { attack = f.sx dg.a0 }
  HCorner -> sl { decay = f.sx dg.d0, sustain = f.sy dg.s0 }
  HTail -> sl { release = f.sx dg.r0 }

-- | Replace one slot of an envelope bank outright (library pick).
setEnvSlot :: Int -> M.EnvSlot -> M.GenBank -> M.GenBank
setEnvSlot j sl = case _ of
  M.GEnv xs -> M.GEnv (overAt j (const sl) xs)
  other -> other

cycleParam :: Boolean -> EnvParam -> EnvParam
cycleParam up p =
  let n = length envParamOrder
      i = fromMaybe 0 (findIndex (_ == p) envParamOrder)
      j = (((if up then i - 1 else i + 1) `mod` n) + n) `mod` n
  in fromMaybe p (envParamOrder !! j)

-- | Whether the selected destination's bank is an envelope bank.
isEnvSlot :: State -> Int -> Boolean
isEnvSlot s dest = case map _.bank (s.sel.destinations !! dest) of
  Just (M.GEnv _) -> true
  _ -> false

-- LFO: ←/→ wavelength (→ stretches the wave = lower Hz, matching the eye),
-- ↑/↓ amplitude of the active shape. Shift = ×10 step.
nudgeLfo :: NudgeDir -> Boolean -> M.ModSlot -> M.ModSlot
nudgeLfo dir shift sl = case dir of
  NRight -> sl { rate = clamp 0.01 50.0 (sl.rate - rStep) }
  NLeft -> sl { rate = clamp 0.01 50.0 (sl.rate + rStep) }
  NUp -> setActiveAmp (activeAmp sl + aStep) sl
  NDown -> setActiveAmp (activeAmp sl - aStep) sl
  where
  rStep = if shift then 1.0 else 0.1
  aStep = if shift then 0.5 else 0.05

-- Euclid: ←/→ n (steps), ↑/↓ k (beats) — the shared widget's arithmetic, applied
-- to the slot's own two fields. Selene's ceiling is generous: the polysignal
-- generator takes the ring as a period, so a long cycle is a legitimate patch
-- rather than an unreadable picture.
euclidBounds :: Euclid.Bounds
euclidBounds = Euclid.boundedBy 64

nudgeEuclid :: NudgeDir -> Boolean -> M.EuclidSlot -> M.EuclidSlot
nudgeEuclid dir shift sl =
  let e = Euclid.nudge euclidBounds dir shift { beats: sl.beats, steps: sl.steps, rotation: 0 }
  in sl { beats = e.beats, steps = e.steps }

-- Clock: ←/→ walk a slow→fast ladder that runs through DIVISION (below ×1, via
-- a longer base) as well as multiplication; ↑/↓ pulse width %. Shift = coarse.
nudgeClock :: NudgeDir -> Boolean -> M.ClockSlot -> M.ClockSlot
nudgeClock dir shift sl = case dir of
  NRight -> setSpeed (speedIndex sl + step) sl   -- faster, into ×N
  NLeft -> setSpeed (speedIndex sl - step) sl    -- slower, into ÷N
  NUp -> sl { pulseWidth = clamp 1 99 (sl.pulseWidth + 5) }
  NDown -> sl { pulseWidth = clamp 1 99 (sl.pulseWidth - 5) }
  where
  step = if shift then 2 else 1

-- The ladder: division rungs (a longer base at ×1) below the multiplication
-- rungs (the multiplier over a quarter). One monotonic slow→fast axis.
clockSpeeds :: Array { base :: M.ClockBase, mult :: Int }
clockSpeeds =
  [ { base: M.ClockWhole, mult: 1 }     -- ÷4
  , { base: M.ClockHalf, mult: 1 }      -- ÷2
  , { base: M.ClockQuarter, mult: 1 }   -- ×1
  , { base: M.ClockQuarter, mult: 2 }
  , { base: M.ClockQuarter, mult: 3 }
  , { base: M.ClockQuarter, mult: 4 }
  , { base: M.ClockQuarter, mult: 6 }
  , { base: M.ClockQuarter, mult: 8 }
  , { base: M.ClockQuarter, mult: 16 }
  ]

-- Pulses per beat: 1.0 = ×1. Both base and multiplier feed it.
clockRate :: M.ClockSlot -> Number
clockRate sl = toNumber sl.multiplier / M.clockBaseBeats sl.base

-- The nearest ladder rung to the slot's current rate — so an exotic source value
-- (a triplet base, an odd multiplier) still steps to a defined neighbour.
speedIndex :: M.ClockSlot -> Int
speedIndex sl =
  let target = clockRate sl
      rateOf e = toNumber e.mult / M.clockBaseBeats e.base
      paired = mapWithIndex (\i e -> Tuple i (abs (rateOf e - target))) clockSpeeds
  in fst (foldl (\best (Tuple i dist) -> if dist < snd best then Tuple i dist else best) (Tuple 2 1.0e9) paired)

setSpeed :: Int -> M.ClockSlot -> M.ClockSlot
setSpeed i sl = case clockSpeeds !! clamp 0 (length clockSpeeds - 1) i of
  Just e -> sl { base = e.base, multiplier = e.mult }
  Nothing -> sl

-- The effective ratio shown big in the cell: ×N over a beat, or ÷N below it.
clockRatioLabel :: M.ClockSlot -> String
clockRatioLabel sl =
  let r = clockRate sl
  in if r >= 1.0 then "×" <> show (round r) else "÷" <> show (round (1.0 / r))

-- Note: arrows = ±semitone; SHIFT = ±octave (any direction; ↑/→ up, ↓/← down).
-- | Envelope: ←/→ move the SELECTED field; ↑/↓ change which field that is.
-- |
-- | This replaces the old fixed two-axis scheme (←/→ decay, ↑/↓ attack), which
-- | could only ever reach two of the eleven fields. One axis of adjustment plus
-- | one axis of selection reaches all of them without spending a key per field,
-- | and `a`/`d`/`s`/`r`/`t`/`p`/`v` jump straight to the common ones. `a` then
-- | ←/→ is exactly the old ↑/↓.
-- |
-- | `timeRange` steps by 1 because it is a 0..7 bucket index, not a 0..127
-- | value — a shared step size would make it unusable in one direction and
-- | pointless in the other.
nudgeEnv :: EnvParam -> NudgeDir -> Boolean -> M.EnvSlot -> M.EnvSlot
nudgeEnv ep dir shift sl = case ep of
  EPAttack -> sl { attack = bump sl.attack }
  EPDecay -> sl { decay = bump sl.decay }
  EPSustain -> sl { sustain = bump sl.sustain }
  EPRelease -> sl { release = bump sl.release }
  EPDepth -> sl { depth = bump sl.depth }
  EPVel -> sl { velDepth = bump sl.velDepth }
  EPTime -> sl { timeRange = clamp 0 7 (sl.timeRange + sign) }
  where
  sign = case dir of
    NRight -> 1
    NUp -> 1
    _ -> -1
  d = if shift then 16 else 4
  bump v = clamp 0 127 (v + sign * d)

nudgeNote :: NudgeDir -> Boolean -> M.PresetNoteSlot -> M.PresetNoteSlot
nudgeNote dir shift sl =
  let d = if shift then 12 else 1
  in case dir of
    NUp -> sl { note = clamp 0 127 (sl.note + d) }
    NRight -> sl { note = clamp 0 127 (sl.note + d) }
    NDown -> sl { note = clamp 0 127 (sl.note - d) }
    NLeft -> sl { note = clamp 0 127 (sl.note - d) }

-- The LFO reduced to its dominant shape: the utility view treats each slot as a
-- single waveform (index into [sin sqr tri saw rnd nse]); the SOURCE pane keeps
-- the full six-way mix for anyone who wants it.
lfoAmps :: M.ModSlot -> Array Number
lfoAmps sl = [ sl.sin, sl.sqr, sl.tri, sl.saw, sl.rnd, sl.nse ]

shapeName :: Int -> String
shapeName = case _ of
  0 -> "sin"
  1 -> "sqr"
  2 -> "tri"
  3 -> "saw"
  4 -> "rnd"
  _ -> "nse"

-- A slot turned all the way down has no dominant shape; it is the shape it
-- was last edited as (`wave`), so turning it back up gives that shape again.
activeShape :: M.ModSlot -> Int
activeShape sl =
  if all (_ == 0.0) (lfoAmps sl) then fromMaybe 0 (elemIndex sl.wave M.lfoWaves)
  else fst (foldl (\best (Tuple k v) -> if v > snd best then Tuple k v else best) (Tuple 0 (-1.0)) (mapWithIndex Tuple (lfoAmps sl)))

activeAmp :: M.ModSlot -> Number
activeAmp sl = fromMaybe 0.0 (lfoAmps sl !! activeShape sl)

setShape :: Int -> Number -> M.ModSlot -> M.ModSlot
setShape ix amp sl = sl { sin = a 0, sqr = a 1, tri = a 2, saw = a 3, rnd = a 4, nse = a 5, wave = fromMaybe sl.wave (M.lfoWaves !! ix) }
  where
  a k = if k == ix then max 0.0 amp else 0.0

setActiveAmp :: Number -> M.ModSlot -> M.ModSlot
setActiveAmp amp sl = setShape (activeShape sl) (clamp 0.0 2.0 amp) sl

cycleWave :: M.ModSlot -> M.ModSlot
cycleWave sl = setShape ((activeShape sl + 1) `mod` 6) (max 0.1 (activeAmp sl)) sl

-- | Move one LFO slot's phase by `step` cycles, kept in [0, 1). The phase is
-- | first snapped to the nearest sixteenth: the rack prints three decimals, so
-- | 0.1875 comes back as 0.188, and without the snap each press would drift.
slidePhase :: Int -> Number -> M.GenBank -> M.GenBank
slidePhase j step = case _ of
  M.GLfo xs -> M.GLfo (overAt j (\sl -> sl { phase = wrap (toNumber (round (sl.phase * 16.0)) / 16.0 + step) }) xs)
  b -> b
  where
  wrap p = let f = p - toNumber (Int.floor p) in if f >= 0.99999 then 0.0 else f

cycleWaveBank :: Int -> M.GenBank -> M.GenBank
cycleWaveBank j = case _ of
  M.GLfo xs -> M.GLfo (overAt j cycleWave xs)
  b -> b

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render s =
  HH.div
    [ style $ "position:fixed;top:var(--tf-bar);left:var(--tf-left,0px);right:var(--tf-right,0px);bottom:0;display:flex;align-items:stretch;overflow:hidden;"
        <> "user-select:none;-webkit-user-select:none;background:#b7b1a0;font-family:Georgia,serif" ]
    [ rackPanel s
    -- (the SOURCE panel went 2026-10-05: the rack is edited as text in
    -- Limulus now, `selene $ lfo es9main # rate 0.5`, applied on the rig;
    -- docs/kb/plans/selene-in-tidal.md)
    ]

panel :: forall m. String -> String -> Array (H.ComponentHTML Action Slots m) -> H.ComponentHTML Action Slots m
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
-- The rack: a stack of destinations + the add bar
-- ---------------------------------------------------------------------------

rackPanel :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
rackPanel s =
  panel "SELENE · DESTINATIONS" "flex:1 1 auto;min-width:0"
    -- every group of outputs the rig has, always, in the rig's order (AC,
    -- 2026-10-05): a free one greyed, each row a drop target for a module
    -- from the drawer (the plain modules there are the old ADD buttons);
    -- then anything on a target off the rig (a MIDI channel, a virtual bus)
    ( [ rackBar s, transportStrip s ]
        <> map row (rackRows s)
        <> envLibraryWall s
        <> [ footNote ]
    )
  where
  row = case _ of
    Placed i | Just d <- s.sel.destinations !! i ->
      dropFrame s { target: d.target, jacks: d.jacks } (map _.dest s.selected == Just i) (destinationRow s s.envParam s.selected i d)
    Placed _ -> HH.text ""
    Free target jacks -> freeRow s target jacks
    HeldRow h -> heldRow h

-- | A row of the rack as drawn: a destination, or a group of outputs with
-- | nothing on it.
data RackRow = Placed Int | Free M.Target M.Jacks | HeldRow Held

-- | A run of a bank's jacks held by something drawn elsewhere (a voice
-- | layout's gates), and what.
type Held = { target :: M.Target, jacks :: M.Jacks, by :: String }

-- | Where a row is: a bank, and the run of its jacks.
type Place = { target :: M.Target, jacks :: M.Jacks }

-- | The rows in the order the page draws them: every group of outputs the rig
-- | has, in the rig's order, then anything on a target off the rig. One
-- | function, so the Twister's ROW walks what the eye sees.
rackRows :: State -> Array RackRow
rackRows s =
  Array.concatMap onRig rigBanks
    <> Array.catMaybes (mapWithIndex (\i d -> if Array.elem d.target rigBanks then Nothing else Just (Placed i)) s.sel.destinations)
  where
  voiceGates = voiceGatesOf s.sel
  -- a bank's rows in jack order: each destination on it, and each run of
  -- jacks nothing is on; a bank with nothing on it is one free row, or two
  -- of four when it has been split to take a module on either half
  onRig target =
    let
      here = Array.filter (\(Tuple _ d) -> d.target == target) (mapWithIndex Tuple s.sel.destinations)
      heldHere = filter (\h -> h.target == target) voiceGates
      covered = Array.concatMap (\(Tuple _ d) -> M.jackNumbers d.jacks) here <> Array.concatMap (\h -> M.jackNumbers h.jacks) heldHere
      gaps = runsOf (filter (\j -> not (Array.elem j covered)) (Array.range 1 M.slotCount))
      free
        | Array.null here && Array.null heldHere && Array.elem (M.targetWire target) s.splitFree = [ { first: 1, count: 4 }, { first: 5, count: 4 } ]
        | otherwise = gaps
      rows = map (\(Tuple i d) -> Tuple d.jacks.first (Placed i)) here
        <> map (\h -> Tuple h.jacks.first (HeldRow h)) heldHere
        <> map (\j -> Tuple j.first (Free target j)) free
    in map snd (Array.sortWith fst rows)

-- | The gates the rack's voice layouts gather on gate banks, as held runs.
voiceGatesOf :: M.Selene -> Array Held
voiceGatesOf sel = Array.concatMap (\d -> case d.bank of
  M.GVoice vb -> Array.fromFoldable vb.gates <#> \g ->
    { target: g.target, jacks: g.jacks
    , by: "the gates of the POLYVOICE on " <> placeLabel { target: d.target, jacks: d.jacks } <> ", voice by voice" }
  _ -> []) sel.destinations

-- | Consecutive jack numbers as runs: [5,6,7,8] is 5-8.
runsOf :: Array Int -> Array M.Jacks
runsOf = foldl step []
  where
  step acc j = case Array.unsnoc acc of
    Just { init, last } | last.first + last.count == j -> Array.snoc init (last { count = last.count + 1 })
    _ -> Array.snoc acc { first: j, count: 1 }

-- | The destinations in drawn order, free rows skipped: what ROW steps over.
rowOrder :: State -> Array Int
rowOrder s = Array.mapMaybe (case _ of
  Placed i -> Just i
  _ -> Nothing) (rackRows s)

-- | A row as a drop target: its edge green when a drag over it can land, red
-- | with why when it cannot.
dropFrame :: forall m. State -> Place -> Boolean -> H.ComponentHTML Action Slots m -> H.ComponentHTML Action Slots m
dropFrame s place current inner =
  HH.div
    [ HE.handler (EventType "dragover") (DragOverTarget place)
    , HE.handler (EventType "dragleave") (const DragLeaveTarget)
    , HE.handler (EventType "drop") (DropOn place)
    , HP.title (placeLabel place <> ": drop a module to replace what is here" <> (if mergeOnOption then "; Option to merge into it" else ""))
    , style ("border-radius:9px;margin-bottom:10px;border:2px solid " <> edge
        <> (if current then ";background:#0000000b" else ""))
    ]
    ( [ inner ] <> case over of
        Just v | not v.ok -> [ HH.div [ style "font:11px Georgia,serif;color:#b3261e;padding:0 14px 8px" ] [ HH.text v.why ] ]
        _ -> []
    )
  where
  over = case s.dragOver of
    Just v | v.wire == M.placeWire place.target place.jacks -> Just v
    _ -> Nothing
  -- The row holding the selection is framed, so ROW on the Twister can be
  -- followed by eye; a drag's verdict outranks it.
  edge = case over of
    Just v | v.ok -> "#2e9e6a"
    Just _ -> "#b3261e"
    Nothing | current -> "#3f3c33"
    Nothing -> "transparent"

-- | A group with nothing on it: greyed, and waiting for a module.
freeRow :: forall m. State -> M.Target -> M.Jacks -> H.ComponentHTML Action Slots m
freeRow s target jacks =
  dropFrame s { target, jacks } false $
    HH.div
      [ style $ "display:flex;align-items:center;gap:12px;padding:11px 12px;border-radius:8px;"
          <> "background:#00000005;border:1px dashed #00000026" ]
      [ HH.div [ style ("display:flex;flex-direction:column;gap:5px;opacity:0.55;flex:0 0 " <> headerWidth) ]
          [ HH.div [ style $ engrave <> ";font-size:10px;letter-spacing:0.1em;color:#6a6657" ] [ HH.text "FREE" ]
          , HH.span [ style "display:flex;align-items:center;gap:8px;font:13px Georgia,serif;color:#1c1a12" ]
              ( [ HH.text (placeLabel { target, jacks }) ] <> halves )
          , HH.span [ style $ engrave <> ";font-size:8px;opacity:0.6" ] [ HH.text ("→ " <> M.placeWire target jacks) ]
          ]
      , HH.div [ style "display:flex;gap:7px;align-items:center;opacity:0.55" ]
          (map (\_ -> HH.span [ style "width:46px;height:46px;border-radius:6px;border:1px dashed #00000022" ] []) (Array.range 1 jacks.count))
      , HH.span [ style "font:italic 12px Georgia,serif;color:#6a6657;opacity:0.55" ] [ HH.text "drop a module here" ]
      ]
  where
  wire = M.targetWire target
  -- a free bank can be drawn as two rows of four, and back
  halves
    | not (hasJacks target) = []
    | M.isWholeBank jacks = [ iconButton "table-columns" "Split: draw this bank as two rows of four, to put a module on each half" (Just (SplitFree wire)) false ]
    | Array.elem wire s.splitFree && jacks.first == 1 = [ iconButton "object-group" "Join: draw this bank as one row of eight again" (Just (JoinFree wire)) false ]
    | otherwise = []

-- | Jacks something else holds: drawn so the bank reads true, and not a drop
-- | target (a module there would fight the voice for its jacks).
heldRow :: forall m. Held -> H.ComponentHTML Action Slots m
heldRow h =
  HH.div [ style "border-radius:9px;margin-bottom:10px;border:2px solid transparent" ]
    [ HH.div
        [ style $ "display:flex;align-items:center;gap:12px;padding:8px 12px;border-radius:8px;"
            <> "background:repeating-linear-gradient(135deg,#00000006 0 6px,#00000000 6px 12px);border:1px solid #00000018" ]
        [ HH.div [ style ("display:flex;flex-direction:column;gap:4px;flex:0 0 " <> headerWidth) ]
            [ HH.div [ style $ engrave <> ";font-size:10px;letter-spacing:0.1em;color:#6a6657" ] [ HH.text "HELD" ]
            , HH.span [ style "font:13px Georgia,serif;color:#1c1a12" ] [ HH.text (placeLabel { target: h.target, jacks: h.jacks }) ]
            ]
        , HH.span [ style "font:italic 12px Georgia,serif;color:#6a6657" ] [ HH.text h.by ]
        ]
    ]

-- | A bank and its run as the page names it: "FH-2 · jacks 5-8".
placeLabel :: Place -> String
placeLabel p = M.targetLabel p.target <> runLabel p.jacks

runLabel :: M.Jacks -> String
runLabel j
  | M.isWholeBank j = ""
  | j.count == 1 = " · jack " <> show j.first
  | otherwise = " · jacks " <> show j.first <> "-" <> show (j.first + j.count - 1)

-- | Whether a target has jacks to split (a MIDI channel or a virtual bus has none).
hasJacks :: M.Target -> Boolean
hasJacks = case _ of
  M.Midi _ -> false
  M.Virtual _ -> false
  _ -> true

-- | The STARTER WALL: every library shape drawn, shown only while an envelope
-- | slot is selected.
-- |
-- | Small multiples, which is the house idiom (Odonus's parameter-major grids)
-- | and the thing Zadar's two-encoders-and-an-OLED could never do: an envelope
-- | is SELF-DESCRIBING, so a wall of drawn curves needs no labels, no category
-- | names and no legend. You recognise the one you want the way you recognise a
-- | face. Names are kept underneath only as a handle for talking about them.
-- |
-- | Contextual rather than permanent: it is a lot of ink, and it is only ever
-- | relevant when there is a slot for a pick to land in.
envLibraryWall :: forall m. State -> Array (H.ComponentHTML Action Slots m)
envLibraryWall s =
  case s.selected of
    Just { dest } | isEnvSlot s dest ->
      [ HH.div
          [ style $ "margin-top:14px;padding:12px 12px 10px;border-radius:8px;"
              <> "background:#00000006;border:1px solid #00000012" ]
          [ HH.div [ style $ engrave <> ";font-size:9px;opacity:0.55;margin-bottom:9px" ]
              [ HH.text "STARTER SHAPES \x00b7 CLICK TO APPLY \x00b7 [ ] TO WALK \x00b7 EXTENT AND COLOUR ARE TIME; BAND IS VELOCITY" ]
          , HH.div [ style "display:flex;flex-wrap:wrap;gap:7px" ]
              (mapWithIndex (starterCell s) Lib.starters)
          ]
      ]
    _ -> []

starterCell :: forall m. State -> Int -> Lib.Starter -> H.ComponentHTML Action Slots m
starterCell s i st =
  HH.div
    [ HE.onClick \_ -> PickStarter i
    , HP.title (st.name <> " \x2014 " <> durLabel (Draw.durationMs st.slot)
        <> " on the " <> timeLabel st.slot.timeRange <> " scale"
        <> (if st.slot.velDepth == 64 then " \x00b7 no velocity response"
            else " \x00b7 vel " <> show st.slot.velDepth)
        <> (if st.slot.depth < 64 then " \x00b7 INVERTED" else ""))
    , style $ "width:104px;flex:0 0 auto;padding:4px 4px 2px;border-radius:6px;cursor:pointer;"
        <> ( if current then "background:#ffffffcc;border:2px solid #1a1a1a;"
             else "background:#ffffff55;border:1px solid #00000010;padding:5px 5px 3px;" ) ]
    [ envSvg { w: 96.0, h: 40.0 } st.slot
    , HH.div
        [ style $ engrave <> ";font-size:8px;opacity:0.6;text-align:center;letter-spacing:0.06em" ]
        [ HH.text st.name ]
    ]
  where
  -- Highlighted when the SELECTED slot currently holds exactly this shape, so
  -- the wall shows where you are, not just where you could go.
  current = case s.selected of
    Just { dest, slot } -> case map _.bank (s.sel.destinations !! dest) of
      Just (M.GEnv xs) -> (xs !! slot) == Just st.slot
      _ -> false
    Nothing -> false

-- | Duration in units a player reads, for the hover title.
durLabel :: Number -> String
durLabel ms =
  if ms < 1000.0 then show (round ms) <> "ms"
  else show (toNumber (round (ms / 100.0)) / 10.0) <> "s"

-- The rack library: named racks (each a saved eDSL doc), the active one brass +
-- editable. Persists to localStorage; a rack's `doc` is its transferable form.
rackBar :: forall m. State -> H.ComponentHTML Action Slots m
rackBar s =
  -- The live rack: its name, and its rebus large, the one spark of colour,
  -- which changes as the rack does. Dragged onto the drawer, it is kept as a
  -- new rack (as people will try). New, duplicate, publish and delete are
  -- the drawer's (2026-10-05); the last word from the rig is beside it.
  HH.div [ style "display:flex;align-items:center;gap:14px;margin-bottom:12px" ]
    [ HH.span
        [ HP.attr (HH.AttrName "draggable") "true"
        , HE.handler (EventType "dragstart") DragRackStart
        , HP.title "This rack. Drag it onto the drawer to keep it as a new rack."
        , style "display:inline-flex;gap:6px;font-size:26px;cursor:grab" ]
        (map (\g -> HH.i [ HP.attr (HH.AttrName "class") ("fa-solid fa-" <> g.icon), style ("font-size:26px;line-height:1;color:" <> g.color) ] []) (rackIcons (currentDoc s)))
    , HH.span [ style "font:15px Georgia,serif;color:#1c1a12" ] [ HH.text (fromMaybe "" (map _.name (s.library !! s.active))) ]
    -- live moved away from saved: the saved rebus, faded, for what it was
    , if diverged s then
        HH.span [ HP.title "The saved rack: the live one has moved away from it (save it from the drawer)", style "display:inline-flex;align-items:center;gap:5px;opacity:0.4;font:italic 12px Georgia,serif;color:#5a5648" ]
          ( [ HH.text "edited from" ]
              <> map (\g -> HH.i [ HP.attr (HH.AttrName "class") ("fa-solid fa-" <> g.icon), style ("font-size:15px;line-height:1;color:" <> g.color) ] []) (rackIcons (savedDoc s)) )
      else HH.text ""
    , publishStatus s
    ]

publishStatus :: forall m. State -> H.ComponentHTML Action Slots m
publishStatus s = case s.publishMsg of
  Nothing -> HH.text ""
  Just msg ->
    HH.span [ style $ engrave <> ";font-size:8px;color:#5a7458;margin-left:4px" ]
      [ HH.text msg ]

-- A compact horizontal transport: live clock + MIDI readouts, the Apply → rig
-- push button, and the per-bank status readout showing each daemon's OK / claim /
-- ERR reply (#142). The rack's CV/gate is generated by the daemons once applied.
transportStrip :: forall m. State -> H.ComponentHTML Action Slots m
transportStrip s =
  HH.div
    [ style $ "display:flex;align-items:center;gap:14px;margin-bottom:14px;padding:8px 10px;"
        <> "border-radius:7px;background:#00000008;border:1px solid #00000012;flex-wrap:wrap" ]
    -- ARM now lives on the tab dot in the top switcher; this strip keeps the readouts.
    -- The tempo is the dashboard's (Triggerfish.Tempo), shown there only.
    ( [ stat "BAR" (show s.clockBar)
      , stat "MIDI" s.midiName
      ]
        <> replyReadout s )
  where
  stat label val =
    HH.div [ style "display:flex;flex-direction:column;gap:1px" ]
      [ HH.span [ style $ engrave <> ";font-size:8px;opacity:0.55" ] [ HH.text label ]
      , HH.span [ style $ "font-family:'SF Mono',Menlo,monospace;font-size:10px;color:" <> ink ] [ HH.text val ]
      ]

-- One status pill per modular (es9/fh2) destination: its socket:bank + the latest
-- daemon reply. "—" before any push, "…" while in flight, then OK (green) / ERR
-- (amber) once the daemon answers. Non-modular destinations (Midi/Virtual) don't
-- appear — they aren't pushed.
replyReadout :: forall m. State -> Array (H.ComponentHTML Action Slots m)
replyReadout s = map pill (mapMaybe Wire.destinationEnvelope s.sel.destinations)
  where
  pill e =
    let
      key = replyKey e.socket e.bank
      status = fromMaybe "—" (Map.lookup key s.replies)
      col = if Str.contains (Str.Pattern "OK") status then "#4f7a3a"
            else if Str.contains (Str.Pattern "ERR") status then "#9a6a20"
            else ink
    in
      HH.div [ style "display:flex;flex-direction:column;gap:1px;max-width:220px" ]
        [ HH.span [ style $ engrave <> ";font-size:8px;opacity:0.55" ] [ HH.text key ]
        , HH.span
            [ style $ "font-family:'SF Mono',Menlo,monospace;font-size:9px;color:" <> col
                <> ";white-space:nowrap;overflow:hidden;text-overflow:ellipsis" ]
            [ HH.text status ]
        ]

footNote :: forall m. H.ComponentHTML Action Slots m
footNote =
  HH.div [ style $ engrave <> ";font-size:8px;opacity:0.45;margin-top:14px;line-height:1.6" ]
    [ HH.text "EACH DESTINATION = A BANK'S 8 JACKS, OR A RUN OF THEM (SPLIT: 1-4 AND 5-8). AS TEXT, IN LIMULUS: SELENE $ LFO ES9MAIN # RATE \"0.5 1 2 4\", OR SELENE $ NOTE FH2_0:5-8 # NOTE \"C3 E3 G3 B3\" — ONE LINE, ONE BANK, ALL AT ONCE." ]

-- | Whether a bank's jacks have a range to set: the ES-9's and the FH-2's do;
-- | an FHX-8GT (`FH2 n`, n > 0) is gates only, and MIDI or a virtual bank has
-- | no jacks.
hasRange :: M.Target -> Boolean
hasRange = case _ of
  M.FH2 n -> n == 0
  M.Midi _ -> false
  M.Virtual _ -> false
  _ -> true

-- One destination: a target/header strip on the left, eight visualised slots.
destinationRow :: forall m. MonadAff m => State -> EnvParam -> Maybe Sel -> Int -> M.Destination -> H.ComponentHTML Action Slots m
destinationRow s ep sel i d =
  HH.div
    [ style $ "display:flex;align-items:stretch;gap:12px;padding:11px 12px;border-radius:8px;"
        <> "background:#00000008;border:1px solid #00000012" ]
    [ destHeader s i d
    -- a muted bank is still drawn, and editable, but faded: its jacks are at 0 V
    , HH.div [ style (slotWrap d.bank <> if d.muted then ";opacity:0.35" else "") ] (slotViews ep i sel d.target d.mutedSlots d.bank)
    ]

-- | The slot layout: the CV/gate kinds flow eight-across. (POLYTRIG's 4×2
-- | mini-notation grid went with it to Balistes' TIDAL tab.)
slotWrap :: M.GenBank -> String
slotWrap _ = "display:flex;flex-wrap:wrap;gap:7px;align-items:center;flex:1 1 auto"

-- | Every bank's header is as wide as the widest (a title with its run of
-- | jacks, a range menu and four buttons on one line), so the outputs line
-- | up down the page.
headerWidth :: String
headerWidth = "290px"

destHeader :: forall m. MonadAff m => State -> Int -> M.Destination -> H.ComponentHTML Action Slots m
destHeader s i d =
  -- the title on the left, three lines; the controls a small panel beside it,
  -- no taller than the title: a row's height decides how many banks fit on a
  -- screen, and the panel gives back the width the buttons took
  HH.div [ style ("display:flex;align-items:center;gap:10px;flex:0 0 " <> headerWidth) ]
    -- the title as wide as the widest (a run's, with its rebus: measured 165px)
    -- and the panel straight after it, so the header is no wider than the two
    [ HH.div [ style "display:flex;flex-direction:column;gap:4px;justify-content:center;flex:0 0 172px;min-width:0;overflow:hidden" ]
        [ HH.div [ style $ engrave <> ";font-size:10px;letter-spacing:0.1em;color:" <> ink ]
            [ HH.text (M.kindLabel (M.bankKind d.bank)) ]
        -- where it plays (moving it is a drop on another row), then the
        -- setting's rebus, as the drawer shows it for a module
        , HH.span [ style "display:flex;align-items:center;gap:8px;font:13px Georgia,serif;color:#1c1a12;white-space:nowrap" ]
            ( [ HH.text (placeLabel { target: d.target, jacks: d.jacks }) ]
                <> map (\g -> HH.i [ HP.attr (HH.AttrName "class") ("fa-solid fa-" <> g.icon), style ("font-size:15px;line-height:1;color:" <> g.color) ] []) (settingIcons (Line.moduleOf d)) )
        , HH.span [ style $ engrave <> ";font-size:8px;opacity:0.45" ]
            [ HH.text ("→ " <> M.placeWire d.target d.jacks <> gatesWire) ]
        ]
    , HH.div [ style "display:flex;flex-direction:column;gap:4px;align-items:stretch;flex:0 0 auto" ]
        -- the jacks' voltage range: an LFO into a 0–5V input wants 0–5V, not
        -- the ±5V an LFO gets by default. An FHX-8GT has no range to set.
        ( ( if not (hasRange d.target) || isVoices then []
            else
              [ HH.select
                  [ HE.onValueChange (SetRange i)
                  , HP.title "Output range of these jacks; default is the rig's for this kind"
                  , style "font:10px Georgia,serif;border:1px solid #00000026;border-radius:4px;background:transparent;color:#5a5648;padding:1px 2px;height:20px" ]
                  ( [ HH.option [ HP.value "", HP.selected (d.range == Nothing) ] [ HH.text "default range" ] ]
                      <> map (\r -> HH.option [ HP.value (M.rangeToWire r), HP.selected (d.range == Just r) ] [ HH.text (M.rangeLabel r) ])
                           [ M.Bipolar5V, M.Unipolar5V, M.Unipolar8V, M.Unipolar10V, M.Unipolar1V ] )
              ] )
          <>
            [ HH.div [ style "display:flex;gap:3px" ]
                ( [ if isVoices
                      then iconButton "volume-high" "A voice is silenced where its notes come from: its playhead, or the routing" Nothing false
                      else iconButton (if d.muted then "volume-xmark" else "volume-high")
                        (if d.muted then "Unmute: play these jacks again, as the bank now is" else "Mute: the jacks go to 0 V; the bank is kept, and can still be edited")
                        (Just (ToggleMute i)) d.muted
                  ]
                  <> splitJoin
                  <> [ iconButton "bookmark" "Keep as a module, to drop on any bank (it goes in the drawer's Modules)" (Just (KeepModule i)) false
                     , iconButton "xmark" "Remove: take this off the rack and free its jacks (keep it as a module first to have it back)" (Just (RemoveDest i)) false
                     ] )
            ] )
    ]
  where
  isVoices = M.bankKind d.bank == M.KVoice
  -- a voice layout's gates, gathered on their own bank
  gatesWire = case d.bank of
    M.GVoice vb -> maybe "" (\g -> " · gates → " <> M.placeWire g.target g.jacks) vb.gates
    _ -> ""
  -- a whole bank splits into two rows of four (it keeps 1-4, 5-8 go free); a
  -- run joins back into the whole bank. Join is always shown on a run, and
  -- greyed with the reason when the rest of the bank is not free.
  splitJoin
    | not (hasJacks d.target) = []
    | M.isWholeBank d.jacks && M.bankKind d.bank == M.KEnv =
        [ iconButton "table-columns" "An envelope bank takes a whole bank: its envelopes answer MIDI channels 1-8, one per slot" Nothing false ]
    | M.isWholeBank d.jacks =
        [ iconButton "table-columns" "Split: keep jacks 1-4 here and free 5-8 for another module" (Just (SplitDest i)) false ]
    | Just o <- Array.find (\x -> x.target == d.target && not (M.samePlace x d)) s.sel.destinations =
        [ iconButton "object-group" ("Join needs the rest of the bank free: " <> M.placeWire o.target o.jacks <> " holds " <> M.kindLabel (M.bankKind o.bank) <> " (remove it first)") Nothing false ]
    | Just h <- Array.find (\x -> x.target == d.target) (voiceGatesOf s.sel) =
        [ iconButton "object-group" ("Join needs the rest of the bank free: " <> h.by <> " holds " <> runLabel h.jacks) Nothing false ]
    | otherwise =
        [ iconButton "object-group" "Join: grow back to the whole bank, the new jacks fresh" (Just (JoinDest i)) false ]

-- | A small square control: an icon, its title, what it does (`Nothing`:
-- | shown greyed, the title saying why not), and whether it is lit (a mute
-- | that is on).
iconButton :: forall m. String -> String -> Maybe Action -> Boolean -> H.ComponentHTML Action Slots m
iconButton icon title act lit =
  HH.button
    ( [ HP.title title
      , style $ "width:24px;height:22px;padding:0;border-radius:4px;display:flex;align-items:center;justify-content:center;"
          <> case act, lit of
               Nothing, _ -> "border:1px solid #00000014;background:transparent;color:#00000030;cursor:default"
               _, true -> "border:1px solid #b3261e;background:#b3261e;color:#fff;cursor:pointer"
               _, false -> "border:1px solid #00000026;background:transparent;color:#5a5648;cursor:pointer"
      ] <> maybe [] (\a -> [ HE.onClick \_ -> a ]) act )
    [ HH.i [ HP.attr (HH.AttrName "class") ("fa-solid fa-" <> icon), style "font-size:11px;line-height:1" ] [] ]

-- ---------------------------------------------------------------------------
-- Per-kind slot visualisations
-- ---------------------------------------------------------------------------

-- Each slot is drawn inside a focusable cell: clicking selects it (a black box);
-- arrow keys then nudge it. `slotViews` threads the destination index + current
-- selection so each cell knows whether it's the selected one and what to fire.
slotViews :: forall m. EnvParam -> Int -> Maybe Sel -> M.Target -> Array Int -> M.GenBank -> Array (H.ComponentHTML Action Slots m)
slotViews ep d sel target mutes = case _ of
  M.GLfo slots -> mapWithIndex (cellFor d sel mutes 90.0 (lfoInner target)) slots
  M.GEuclid slots -> mapWithIndex (cellFor d sel mutes 70.0 euclidInner) slots
  M.GClock slots -> mapWithIndex (cellFor d sel mutes 58.0 clockInner) slots
  M.GNote slots -> mapWithIndex (cellFor d sel mutes 58.0 noteInner) slots
  M.GEnv slots -> mapWithIndex (\j sl -> slotCell d j (sel == Just { dest: d, slot: j }) (Array.elem j mutes) 90.0 (envInner (sel == Just { dest: d, slot: j }) ep sl)) slots
  -- a voice is as wide as the jacks it takes, and has no mute of its own:
  -- it is silenced where its notes come from
  M.GVoice vb -> mapWithIndex (\j v -> voiceCell d j (sel == Just { dest: d, slot: j }) (if Array.elem M.VEnvelope vb.outputs then 104.0 + 40.0 * toNumber (M.voiceStride vb - 2) else 64.0) (voiceInner vb j v)) vb.voices

-- | Wrap one slot's inner drawing in the focusable, selectable cell.
cellFor :: forall m a. Int -> Maybe Sel -> Array Int -> Number -> (a -> Array (H.ComponentHTML Action Slots m)) -> Int -> a -> H.ComponentHTML Action Slots m
cellFor d sel mutes widthPx inner j sl =
  slotCell d j (sel == Just { dest: d, slot: j }) (Array.elem j mutes) widthPx (inner sl)

-- | One output's cell, with its mute in the corner: a muted output is drawn
-- | faded, keeps its settings (and can still be edited), and goes out silent.
slotCell :: forall m. Int -> Int -> Boolean -> Boolean -> Number -> Array (H.ComponentHTML Action Slots m) -> H.ComponentHTML Action Slots m
slotCell d j isSel muted widthPx body =
  HH.div
    [ HP.tabIndex 0
    , style $ "position:relative;width:" <> show (round widthPx) <> "px;flex:0 0 auto;padding:5px 4px;border-radius:6px;"
        <> "display:flex;flex-direction:column;gap:2px;align-items:center;cursor:pointer;outline:none;"
        <> ( if isSel then "background:#ffffffcc;border:2px solid #1a1a1a;"
             else "background:#ffffff55;border:1px solid #00000010;padding:6px 5px;" )
    , HE.onClick \_ -> SelectSlot d j
    , HE.onKeyDown SlotKeyDown ]
    ( [ HH.div [ style ("display:flex;flex-direction:column;gap:2px;align-items:center" <> if muted then ";opacity:0.3" else "") ] body
      , HH.button
          [ HE.onClick (ToggleSlotMute d j)
          , HP.title (if muted then "Unmute output " <> show (j + 1) <> ": it plays again, as it now is" else "Mute output " <> show (j + 1) <> ": it goes silent and keeps its settings")
          , style $ "position:absolute;top:2px;right:2px;width:15px;height:15px;padding:0;border-radius:3px;"
              <> "font:bold 9px/13px Georgia,serif;cursor:pointer;"
              <> if muted then "border:1px solid #b3261e;background:#b3261e;color:#fff" else "border:1px solid #00000022;background:#ffffffaa;color:#00000066" ]
          [ HH.text "M" ]
      ] )

-- --- POLYLFO: a scaled waveform, 0V baseline, log-frequency, rate label ------

lfoInner :: forall m. M.Target -> M.ModSlot -> Array (H.ComponentHTML Action Slots m)
lfoInner target sl =
  let
    w = 84.0
    h = 50.0
    mid = h / 2.0
    -- normalise the summed wave into the cell (90% of half-height)
    span = max 1.0 (abs sl.level + sl.sin + sl.sqr + sl.tri + abs sl.saw + sl.rnd + sl.nse)
    -- enough points that a random slot's steps draw as steps
    samples = 96
    pt k =
      let
        u = toNumber k / toNumber samples
        x = u * w
        y = mid - (lfoDrawn sl u / span) * (mid * 0.9)
      in
        show (round2 x) <> "," <> show (round2 y)
    poly = joinWith " " (map pt (range 0 samples))
    clamped = M.lfoRateOn target sl.rate
  in
    [ svgEl "svg"
        [ svgAttr "viewBox" ("0 0 " <> show w <> " " <> show h), svgAttr "width" "100%"
        , svgAttr "height" (show h), svgAttr "style" "display:block;overflow:visible" ]
        [ svgEl "line"
            [ svgAttr "x1" "0", svgAttr "y1" (show mid), svgAttr "x2" (show w), svgAttr "y2" (show mid)
            , svgAttr "stroke" "#00000022", svgAttr "stroke-width" "0.8", svgAttr "stroke-dasharray" "2 2" ] []
        , svgEl "polyline"
            [ svgAttr "points" poly, svgAttr "fill" "none", svgAttr "stroke" accent
            , svgAttr "stroke-width" "1.6", svgAttr "stroke-linejoin" "round" ] []
        ]
    -- the rate it really runs at, where the hardware clamps it (an FH-2 LFO),
    -- with a red ! so a clamped slot shows at a glance
    , HH.span [ style "display:flex;align-items:center;gap:3px" ]
        ( ( case clamped of
              Just c ->
                [ HH.span
                    [ HP.title ("The FH-2 can't run " <> fmt2 sl.rate <> " Hz: this LFO runs at " <> fmt2 c <> " Hz (its LFOs span 0.1 to 10.36 Hz)")
                    , style "font:bold 10px Georgia,serif;color:#fff;background:#b3261e;border-radius:6px;padding:0 4px;line-height:12px" ]
                    [ HH.text "!" ] ]
              Nothing -> [] )
          <> [ cellCaption (shapeName (activeShape sl) <> " · " <> fmt2 sl.rate <> " Hz" <> maybe "" (\c -> " → " <> fmt2 c) clamped) ] )
    ]

-- | The drawn wave: the model's deterministic shapes (M.lfoValue), plus random
-- | and noise as fixed icons, the same on every slot. The hardware's random
-- | is not this curve and cannot be drawn ahead, so the drawing says only that
-- | it is random, and how much: a sample-and-hold staircase that moves with
-- | the slot's rate and phase, and a fine jitter for noise.
lfoDrawn :: M.ModSlot -> Number -> Number
lfoDrawn sl u = M.lfoValue sl u + sl.rnd * held + sl.nse * jitter
  where
  t = u * M.lfoCyclesShown sl.rate + sl.phase
  held = stepTable (Int.floor (t * 4.0))
  jitter = stepTable (Int.floor (u * 96.0) * 5) * 0.8
  stepTable i = fromMaybe 0.0 (rndSteps !! (i `mod` 8))   -- Int mod is never negative

rndSteps :: Array Number
rndSteps = [ 0.55, -0.35, 0.9, -0.8, 0.15, -0.6, 0.7, -0.1 ]

-- --- POLYEUCLID: a ring of step-dots with k / n in the centre ----------------

euclidInner :: forall m. M.EuclidSlot -> Array (H.ComponentHTML Action Slots m)
euclidInner sl = [ Euclid.ring euclidStyle Nothing { beats: sl.beats, steps: sl.steps, rotation: 0 } ]

-- | Selene's dressing for the shared ring: steel-blue dots, no playhead (a
-- | polysignal Euclid is a shape handed to the daemon, not a thing this pane
-- | clocks), sized to sit in the 70px slot cell.
euclidStyle :: Euclid.Style
euclidStyle = Euclid.defaultStyle
  { size = 64.0, inset = 8.0, dotOn = 3.4, dotOff = 2.0
  , fill = accent, ink = ink, fontSize = 13.0
  }

-- --- POLYCLOCK: a list of division numbers -----------------------------------

clockInner :: forall m. M.ClockSlot -> Array (H.ComponentHTML Action Slots m)
clockInner sl =
  [ HH.div [ style $ "font-family:'SF Mono',Menlo,monospace;font-size:18px;color:" <> ink <> ";text-align:center" ]
      [ HH.text (clockRatioLabel sl) ]
  , cellCaption (M.clockBaseLabel sl.base <> " · " <> show sl.pulseWidth <> "%")
  ]

-- --- POLYNOTE: a list of notes -----------------------------------------------

noteInner :: forall m. M.PresetNoteSlot -> Array (H.ComponentHTML Action Slots m)
noteInner sl =
  [ HH.div [ style $ "font-family:'SF Mono',Menlo,monospace;font-size:16px;color:" <> ink <> ";text-align:center" ]
      [ HH.text (M.noteName sl.note) ]
  , cellCaption ("midi " <> show sl.note)
  ]

-- --- POLYVOICE: a MIDI voice, its channel and its outputs ---------------------

-- | A voice's cell: selectable as a slot is, with no mute badge.
voiceCell :: forall m. Int -> Int -> Boolean -> Number -> Array (H.ComponentHTML Action Slots m) -> H.ComponentHTML Action Slots m
voiceCell d j isSel widthPx body =
  HH.div
    [ HP.tabIndex 0
    , style $ "width:" <> show (round widthPx) <> "px;flex:0 0 auto;padding:5px 4px;border-radius:6px;"
        <> "display:flex;flex-direction:column;gap:2px;align-items:center;cursor:pointer;outline:none;"
        <> ( if isSel then "background:#ffffffcc;border:2px solid #1a1a1a;"
             else "background:#ffffff55;border:1px solid #00000010;padding:6px 5px;" )
    , HE.onClick \_ -> SelectSlot d j
    , HE.onKeyDown SlotKeyDown ]
    body

-- | The channel that plays the voice, its envelope when it has one, and its
-- | outputs in the order they come out of its jacks.
voiceInner :: forall m. M.VoiceBank -> Int -> M.VoiceSlot -> Array (H.ComponentHTML Action Slots m)
voiceInner vb _ v =
  [ HH.div [ style $ "font-family:'SF Mono',Menlo,monospace;font-size:15px;color:" <> ink <> ";text-align:center" ]
      [ HH.text ("ch " <> show v.channel) ] ]
    <> (if Array.elem M.VEnvelope vb.outputs then [ envSvgWith false { w: 92.0, h: 26.0 } v.env ] else [])
    <> [ cellCaption (Str.joinWith " · " ([ "pitch" ] <> map outName (Array.filter (\o -> Array.elem o vb.outputs) M.voiceOuts))) ]
  where
  outName = case _ of
    M.VVelocity -> "vel"
    M.VEnvelope -> "env"
    M.VPressure -> "pres"

-- --- POLYENV: the shape itself ----------------------------------------------

-- | Draw the envelope rather than name it. Eight of these side by side is the
-- | bank's whole story — you read plucked-to-swelling across the row without
-- | parsing a single number, which is what the other four visuals do for their
-- | kinds.
-- |
-- | A schematic, not a simulation: A/D/R are drawn as proportions of the cell
-- | width and S as a height, so the picture tracks the bytes without pretending
-- | to know the firmware's time buckets.
-- | The value of the currently-selected field, for the cell caption.
paramValue :: EnvParam -> M.EnvSlot -> Int
paramValue ep sl = case ep of
  EPAttack -> sl.attack
  EPDecay -> sl.decay
  EPSustain -> sl.sustain
  EPRelease -> sl.release
  EPTime -> sl.timeRange
  EPDepth -> sl.depth
  EPVel -> sl.velDepth

-- | The firmware's time bucket in the units a player thinks in. Shown always,
-- | because it is the single biggest determinant of whether a shape reads as
-- | snappy: the same a/d/s/r at bucket 0 and bucket 4 are a click and a swell,
-- | and nothing else on the cell would tell you which one you have.
timeLabel :: Int -> String
timeLabel = case _ of
  0 -> "200ms"
  1 -> "500ms"
  2 -> "1s"
  3 -> "2s"
  4 -> "5s"
  5 -> "10s"
  6 -> "20s"
  _ -> "50s"

envInner :: forall m. Boolean -> EnvParam -> M.EnvSlot -> Array (H.ComponentHTML Action Slots m)
envInner isSel ep sl =
  [ envSvgWith isSel { w: 100.0, h: 44.0 } sl
  -- The caption carries what the drawing cannot: the four ADSR numbers for
  -- precision, the LIVE parameter and its value (without which the key scheme is
  -- invisible and you would be adjusting a field you cannot see), and the
  -- library name — but only while the shape is still exactly a starter. Once
  -- tweaked it is an unnamed shape that began there, and saying otherwise would
  -- be a small lie that compounds. The time bucket is NOT repeated here: the
  -- drawing already carries it twice, as extent and as colour.
  , cellCaption ("a" <> show sl.attack <> " d" <> show sl.decay
                  <> " s" <> show sl.sustain <> " r" <> show sl.release
                  <> "  \x00b7  " <> envParamLabel ep <> " " <> show (paramValue ep sl)
                  <> (case Lib.nameOf sl of
                        Just n -> "  \x00b7  " <> n
                        Nothing -> ""))
  ]

-- | The one drawing of an envelope, shared by the in-rack slot and the library
-- | wall. Geometry comes from `EnvDraw`; this only turns points into SVG, so the
-- | two surfaces cannot drift into disagreeing about what a shape looks like.
envSvg :: forall m. { w :: Number, h :: Number } -> M.EnvSlot -> H.ComponentHTML Action Slots m
envSvg = envSvgWith false

-- | The drawing, with breakpoint handles when this is the cell being edited.
-- |
-- | Handles only on the selected cell: three per envelope across eight slots
-- | would be twenty-four dots competing with the shapes they are meant to let
-- | you read.
envSvgWith :: forall m. Boolean -> { w :: Number, h :: Number } -> M.EnvSlot -> H.ComponentHTML Action Slots m
envSvgWith isSel box sl =
  svgEl "svg"
    ( [ svgAttr "viewBox" ("0 0 " <> show box.w <> " " <> show box.h)
      , svgAttr "width" "100%", svgAttr "height" (show box.h) ]
        <> ( if isSel
               then [ svgOn "mousemove" \e -> EnvDragMove (ME.clientX e) (ME.clientY e) (ME.buttons e) (ME.shiftKey e) ]
               else [] ) )
    ( baselineRule
        <> [ line fig.curve 2.0 "1" ]
        <> velSpike
        <> (if isSel then handles else []) )
  where
  -- Grab points, at the three breakpoints of the drawn curve, which is the
  -- velocity-127 outline — the one the numbers actually describe.
  handles =
    [ grip HPeak (fig.curve !! 1)
    , grip HCorner (fig.curve !! 2)
    , grip HTail (fig.curve !! 4)
    ]
  grip h mp = case mp of
    Nothing -> svgEl "g" [] []
    Just pt ->
      svgEl "circle"
        [ svgAttr "cx" (show pt.x), svgAttr "cy" (show pt.y), svgAttr "r" "3.4"
        , svgAttr "fill" "#fffdf8", svgAttr "stroke" "#1a1a1a", svgAttr "stroke-width" "1.4"
        , svgAttr "cursor" (case h of
            HCorner -> "move"
            _ -> "ew-resize")
        , svgOn "mousedown" \e -> EnvGrab h (ME.clientX e) (ME.clientY e) ] []
  fig = Draw.figure box sl
  pts ps = joinWith " " (map (\p -> show p.x <> "," <> show p.y) ps)
  line ps wdt op =
    svgEl "polyline"
      [ svgAttr "points" (pts ps)
      , svgAttr "fill" "none"
      , svgAttr "stroke" fig.ink
      , svgAttr "stroke-width" (show wdt)
      , svgAttr "stroke-opacity" op
      , svgAttr "stroke-linejoin" "round"
      , svgAttr "stroke-linecap" "round" ] []
  -- The velocity spike: a bar hanging from the peak whose LENGTH is how much
  -- velocity moves it. A static envelope has no bar at all.
  --
  -- Green, and the only saturated colour on the cell. Everything else here is the
  -- pale Hainbach range, including the time-bucket ramp the curve is drawn in,
  -- so a mark in that family reads as more of the same and is lost the moment it
  -- gets short — which is exactly when it matters, since a small bar is a real
  -- reading and not an absence. The colour is doing no encoding work; it is
  -- there to separate one channel from the rest.
  --
  -- Red was the first try and was the wrong end of the wheel: the time ramp runs
  -- amber → deep brown, so a red bar sits close in hue to whatever it crosses and
  -- the contrast collapses on the darkest, slowest buckets. Green is opposite
  -- that ramp, so it separates equally well against all eight.
  --
  -- Drawn LAST, over the curve. A zero-attack shape puts the peak hard against
  -- the left edge, which makes the bar exactly collinear with the vertical
  -- attack stroke — so whichever is drawn second is the one you see, and it has
  -- to be this one. (`fall` is the case that proved it: drawn underneath, the
  -- bar vanished completely behind the attack.)
  velSpike = case fig.vel of
    Nothing -> []
    Just v ->
      [ svgEl "line"
          [ svgAttr "x1" (show v.x), svgAttr "x2" (show v.x)
          , svgAttr "y1" (show v.from), svgAttr "y2" (show v.to)
          , svgAttr "stroke" "mediumseagreen"
          , svgAttr "stroke-width" "3"
          , svgAttr "stroke-opacity" "0.9"
          , svgAttr "stroke-linecap" "round" ] [] ]
  -- Zero volts, drawn faintly, because an inverted envelope goes BELOW it and
  -- without the rule there is nothing to read "below" against.
  baselineRule =
    [ svgEl "line"
        [ svgAttr "x1" "0", svgAttr "x2" (show box.w)
        , svgAttr "y1" (show fig.baseline), svgAttr "y2" (show fig.baseline)
        , svgAttr "stroke" "#00000018", svgAttr "stroke-width" "1" ] [] ]

-- ---------------------------------------------------------------------------
-- Cell chrome
-- ---------------------------------------------------------------------------

cellCaption :: forall m. String -> H.ComponentHTML Action Slots m
cellCaption t =
  HH.span [ style $ engrave <> ";font-size:8px;opacity:0.55;text-align:center" ] [ HH.text t ]

-- ---------------------------------------------------------------------------
-- Source panel — the growing-spec eDSL, one block per destination (read-only)
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- helpers
-- ---------------------------------------------------------------------------

abs :: Number -> Number
abs x = if x < 0.0 then -x else x

round2 :: Number -> Number
round2 x = toNumber (round (x * 100.0)) / 100.0

fmt2 :: Number -> String
fmt2 x = show (toNumber (round (x * 100.0)) / 100.0)

-- | The banks of the rig a page shows (MIDI channels are not modular banks).
rigBanks :: Array M.Target
rigBanks = filter notMidi (availableTargets defaultRig)
  where
  notMidi = case _ of
    M.Midi _ -> false
    _ -> true

-- | What a drawer row of a module (or block) carries when dragged: the module,
-- | the bank a new line would put it on (the first free one that can take
-- | it, else the first that can), and what a drop adds after it (a module,
-- | not a block, replaces the bank's whole setting: `# fresh`), tab-separated
-- | (Triggerfish.Drag's `selene-module`; Limulus.Drop reads it).
moduleDrag :: State -> String -> String
moduleDrag s m = Drag.dragOf "selene-module" (m <> "\t" <> bank <> "\t" <> suffix)
  where
  takes t = maybe false (Line.accepts t) (Line.moduleKind m)
  used t = Array.any (\d -> d.target == t) s.sel.destinations
  bank = maybe "" M.targetWire (Array.find (\t -> takes t && not (used t)) rigBanks <|> Array.find takes rigBanks)
  isBlock = isJust (Array.head (Str.split (Str.Pattern " ") (Str.trim m)) >>= Block.blockNamed)
  suffix = if isBlock then "" else " # fresh"

-- | The module a drag carries (its first field), or the drag's text as it is.
moduleOfDrag :: String -> String
moduleOfDrag text = case Str.stripPrefix (Str.Pattern "selene-module ") text of
  Just payload -> Str.takeWhile (_ /= Str.codePointFromChar '\t') payload
  Nothing -> text

-- | What the drawer's undo gives back: the rack a recall replaced (as it was
-- | live), or a deleted rack or kept module, where it was.
data Undo
  = UndoReplace { rack :: Int, live :: String }
  | UndoRack Int Store.Rack
  | UndoModule Int Module

-- | The drawer has moved on, so the last undo is gone.
settleUndo :: forall slots o m. MonadAff m => H.HalogenM State Action slots o m Unit
settleUndo = H.modify_ _ { undoTo = Nothing, notice = "" }
