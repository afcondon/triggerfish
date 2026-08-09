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

import Data.Array (any, deleteAt, drop, filter, findIndex, length, mapMaybe, mapWithIndex, modifyAt, null, range, (!!))
import Data.Foldable (for_, foldl, foldr)
import Data.Tuple (Tuple(..), fst, snd)
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.Event.Event (preventDefault)
import Data.Int (round, toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.String as Str
import Data.String.Common (joinWith)
import Effect.Aff (attempt)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Timer (setInterval)
import Data.Either (Either(..))
import Data.String.CodeUnits (take)
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
import Triggerfish.Odonus.Grid.Widgets (engrave, style, svgAttr, svgEl, svgOn)
import Triggerfish.Ui.Euclid (Nudge(..))
import Triggerfish.Ui.Euclid as Euclid
import Triggerfish.Selene.EnvDraw as Draw
import Triggerfish.Selene.EnvLibrary as Lib
import Triggerfish.Selene.Model as M
import Triggerfish.Selene.Source as Source
import Triggerfish.Selene.Store as Store
import Triggerfish.Selene.Wire as Wire
import Triggerfish.Amphora as Amphora
import Triggerfish.Glyph as G
import Triggerfish.Preset (Preset, indexOfContent, presetAlias)
import Triggerfish.SourceQuery (Query(..))
import Triggerfish.Transport (Sounding(..))
import Triggerfish.Rig (defaultRig, targetGroups)
import Halogen.Widgets.Select as Select
import Type.Proxy (Proxy(..))
import Data.Maybe (Maybe(..), fromMaybe)

-- | The rack view hosts one cascade target-picker per destination, keyed by
-- | destination index — the same nested ES-9/FH-2/MIDI menu the routing modal
-- | uses (the machine's own copy of the shared control; see Triggerfish.Rig).
type Slots = ( selTarget :: Select.Slot Int )

_selTarget :: Proxy "selTarget"
_selTarget = Proxy

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
  | AddDest M.GenKind         -- append a template block (comment-safe)
  | SetDoc String             -- the whole editable document, verbatim
  | RetargetDest Int String   -- re-route destination i to a target wire (cascade menu)
  | SelectSlot Int Int        -- click a drawn slot (dest, slot): select it — or, if it's an
                              -- already-selected LFO, cycle its waveform
  | SlotKeyDown KeyboardEvent -- a keystroke on the focused slot cell (arrows nudge the field)
  | SelectRack Int            -- load a library rack into the editor
  | NewRack                   -- append a fresh empty rack + select it
  | SetRackName String        -- rename the active rack
  | ApplyToRig                -- push every modular destination to its daemon
  | SeleneReply String        -- a raw `selene-reply …` frame from the rig
  | PublishRack               -- publish the active rack to the Amphora store (selene-rack)
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
          , library: [ { name: "rack 1", doc } ], active: 0
          , sounding: Silent, playStep: 0
          , binnacle: Nothing, midiOut: Nothing, midiName: "…"
          , clockTempo: 120.0, clockLocked: false, clockBar: 0
          , replies: Map.empty
          , publishMsg: Nothing
          , presets: [], identity: Nothing, lastChip: Nothing
          , selected: Nothing
          , envParam: EPDecay
          , envPick: 0
          , envDrag: Nothing
          }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction, handleQuery = handleQuery, initialize = Just Initialize }
    }

handleQuery :: forall m a. MonadAff m => Query a -> H.HalogenM State Action Slots Output m (Maybe a)
handleQuery = case _ of
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
    H.modify_ \s -> s { library = setDocAt s.active doc s.library, sel = Source.parseRack doc }
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
  FeedChords _ next -> pure (Just next)
  FeedVoiceChords _ next -> pure (Just next)   -- no chord quantiser
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
      in s { active = i, sel = Source.parseRack doc }
    persist
    pure (Just next)
  -- Import always succeeds (parseRack is total) — the manager routes here by an
  -- explicit target, so the user already chose Selene.
  ImportText txt reply -> do
    H.modify_ \s ->
      let n = length s.library
      in s { library = s.library <> [ { name: "imported " <> show (n + 1), doc: txt } ], active = n
           , sel = Source.parseRack txt }
    persist
    pure (Just (reply true))
  -- No quantiser — the rig's harmonic context doesn't apply to Selene.
  SetContextPitchSet _ _ next -> pure (Just next)
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
        { presets = st.presets <> [ { content: text, name: Nothing, starred: false } ]
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
        { library = setDocAt s.active p.content s.library
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
      Nothing -> HS.notify midiL (MidiReady Nothing "unavailable")
    -- restore the saved rack library + preset bank (falls back to the default rack).
    msaved <- liftEffect Store.loadLibrary
    for_ msaved \sv -> do
      H.modify_ _ { presets = sv.presets }
      when (not (null sv.library)) do
        let a = if sv.active >= 0 && sv.active < length sv.library then sv.active else 0
            doc = fromMaybe "" (map _.doc (sv.library !! a))
        H.modify_ _ { library = sv.library, active = a, sel = Source.parseRack doc }
    H.modify_ _ { binnacle = Just bin }
    -- Merge the shared Amphora rack library over the local one (by name), in the
    -- BACKGROUND: awaiting it blocked Initialize (hence all queries to Selene) until
    -- the ~30s offline timeout. The store being offline is not fatal — keep local.
    void $ H.fork do
      dbRes <- liftAff (attempt (Amphora.fetchCollection "selene-rack"))
      case dbRes of
        Right items | not (null items) ->
          H.modify_ \s -> s { library = mergeRacksByName s.library (map amphoraRack items) }
        _ -> pure unit

  Step tick -> do
    st <- H.get
    -- No audible output yet (CV/gate awaits the es9 path, #142) — just advance
    -- the playhead so the rack's visuals sweep under the master transport.
    when (st.sounding == Local) $
      H.modify_ _ { playStep = tick.index `mod` cycleSteps }

  Frame -> do
    st <- H.get
    for_ st.binnacle \bin -> do
      r <- liftEffect $ Clock.read (Binnacle.clock bin)
      H.modify_ _ { clockTempo = r.tempo, clockLocked = r.locked, clockBar = r.bar }
    -- Report the identity chip up to the shell's status board, but only when it
    -- actually changed (this fires ~30×/s) — capture/recall/divergence all land here.
    s2 <- H.get
    let cv = chipViewOf s2
    when (cv /= s2.lastChip) do
      H.modify_ _ { lastChip = cv }
      H.raise (IdentityChanged cv)

  MidiReady mout nm -> H.modify_ _ { midiOut = mout, midiName = nm }

  -- append a fresh block to the active rack's doc (so existing comments
  -- survive), then re-derive the rack from the new text.
  AddDest k -> do
    H.modify_ \s ->
      let block = Source.printDest { target: M.defaultTargetFor k, range: Nothing, bank: M.freshBank k }
          doc = currentDoc s <> "\n\n" <> block
      in s { library = setDocAt s.active doc s.library, sel = Source.parseRack doc }
    persist
  SetDoc doc -> do
    H.modify_ \s -> s { library = setDocAt s.active doc s.library, sel = Source.parseRack doc }
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
      in s { library = setDocAt s.active doc s.library, sel = Source.parseRack doc }
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
      in s { active = i, sel = Source.parseRack doc }
    persist
  NewRack -> do
    H.modify_ \s ->
      let doc = Source.printRack M.defaultSelene
          n = length s.library
      in s { library = s.library <> [ { name: "rack " <> show (n + 1), doc } ], active = n, sel = Source.parseRack doc }
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
    let envs = mapMaybe Wire.destinationEnvelope st.sel.destinations
    for_ st.binnacle \bin ->
      for_ envs \e ->
        liftEffect $ Transport.send (Binnacle.socket bin)
          ("selene-apply " <> e.socket <> " " <> e.bank <> " " <> e.json)
    -- mark every pushed bank pending; the daemon's OK/ERR replaces it.
    H.modify_ \s -> s { replies = foldr (\e m -> Map.insert (replyKey e.socket e.bank) "…" m) s.replies envs }

  -- A `selene-reply <socket> <bank> <status…>` frame — record the status against
  -- its bank. Non-matching frames (other verbs) are ignored.
  SeleneReply raw -> case Str.stripPrefix (Str.Pattern "selene-reply ") raw of
    Nothing -> pure unit
    Just rest ->
      let toks = Str.split (Str.Pattern " ") rest
      in case toks !! 0, toks !! 1 of
           Just socket, Just bank ->
             H.modify_ \s -> s { replies = Map.insert (replyKey socket bank) (joinWith " " (drop 2 toks)) s.replies }
           _, _ -> pure unit

  -- Publish the active rack to the shared Amphora store (selene-rack collection).
  -- The rack's doc is already its canonical eDSL form; the name rides the label.
  -- Store offline → a transient failure message, never fatal.
  PublishRack -> do
    s <- H.get
    case s.library !! s.active of
      Nothing -> pure unit
      Just r -> do
        H.modify_ _ { publishMsg = Just "publishing…" }
        res <- liftAff (attempt (Amphora.publish
          { kind: "selene-rack", collection: "selene-rack"
          , name: r.name, source: "user", payload: r.doc, tags: [] }))
        H.modify_ _ { publishMsg = Just case res of
          Right hash -> "✓ " <> r.name <> " · " <> take 8 hash
          Left _ -> "✗ publish failed (store offline?)" }

-- | The active rack's eDSL doc — the editable text + the AskSource answer.
currentDoc :: State -> String
currentDoc s = fromMaybe "" (map _.doc (s.library !! s.active))

-- | Replace one rack's doc in the library.
setDocAt :: Int -> String -> Array Store.Rack -> Array Store.Rack
setDocAt i doc lib = fromMaybe lib (modifyAt i (_ { doc = doc }) lib)

-- | An Amphora library item as a local rack (payload = the rack's eDSL doc).
amphoraRack :: Amphora.LibItem -> Store.Rack
amphoraRack it = { name: it.name, doc: it.payload }

-- | Merge incoming (Amphora) racks over the current local ones by name: keep
-- | every local rack, then append any incoming rack whose name isn't present.
mergeRacksByName :: Array Store.Rack -> Array Store.Rack -> Array Store.Rack
mergeRacksByName current incoming =
  current <> filter (\p -> not (any (\q -> q.name == p.name) current)) incoming

-- | Persist the rack library + preset bank (after any library/active/preset change).
persist :: forall o m. MonadAff m => H.HalogenM State Action Slots o m Unit
persist = do
  s <- H.get
  liftEffect (Store.saveLibrary { active: s.active, library: s.library, presets: s.presets })

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
editDest :: forall m. MonadAff m => Int -> (M.Destination -> M.Destination) -> H.HalogenM State Action Slots Output m Unit
editDest d f = do
  H.modify_ \s ->
    let dests' = mapWithIndex (\i dd -> if i == d then f dd else dd) s.sel.destinations
        doc = Source.printRack (s.sel { destinations = dests' })
    in s { library = setDocAt s.active doc s.library, sel = Source.parseRack doc }
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
  let e = Euclid.nudge euclidBounds dir shift { beats: sl.beats, steps: sl.steps }
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

activeShape :: M.ModSlot -> Int
activeShape sl = fst (foldl (\best (Tuple k v) -> if v > snd best then Tuple k v else best) (Tuple 0 (-1.0)) (mapWithIndex Tuple (lfoAmps sl)))

activeAmp :: M.ModSlot -> Number
activeAmp sl = fromMaybe 0.0 (lfoAmps sl !! activeShape sl)

setShape :: Int -> Number -> M.ModSlot -> M.ModSlot
setShape ix amp sl = sl { sin = a 0, sqr = a 1, tri = a 2, saw = a 3, rnd = a 4, nse = a 5 }
  where
  a k = if k == ix then max 0.0 amp else 0.0

setActiveAmp :: Number -> M.ModSlot -> M.ModSlot
setActiveAmp amp sl = setShape (activeShape sl) (clamp 0.0 2.0 amp) sl

cycleWave :: M.ModSlot -> M.ModSlot
cycleWave sl = setShape ((activeShape sl + 1) `mod` 6) (max 0.1 (activeAmp sl)) sl

cycleWaveBank :: Int -> M.GenBank -> M.GenBank
cycleWaveBank j = case _ of
  M.GLfo xs -> M.GLfo (overAt j cycleWave xs)
  b -> b

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render s =
  HH.div
    [ style $ "position:fixed;top:var(--tf-bar);left:0;right:0;bottom:0;display:flex;align-items:stretch;overflow:hidden;"
        <> "user-select:none;-webkit-user-select:none;background:#b7b1a0;font-family:Georgia,serif" ]
    [ rackPanel s
    , sourcePanel s
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
    ( [ rackBar s, transportStrip s ]
        <> mapWithIndex (destinationRow s.envParam s.selected) s.sel.destinations
        <> envLibraryWall s
        <> [ addBar, footNote ]
    )

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
  HH.div [ style "display:flex;align-items:center;gap:6px;flex-wrap:wrap;margin-bottom:12px" ]
    ( [ HH.span [ style $ engrave <> ";font-size:9px;opacity:0.6;margin-right:2px" ] [ HH.text "RACKS" ] ]
        <> mapWithIndex (\i r -> rackChip r.name (i == s.active) (SelectRack i)) s.library
        <> [ newRackChip
           , HH.input
               [ HP.value (fromMaybe "" (map _.name (s.library !! s.active)))
               , HE.onValueInput SetRackName
               , style $ "margin-left:6px;padding:5px 9px;border:1px solid #a8a392;border-radius:5px;background:#f4f1e8;"
                   <> "font-family:Georgia,serif;font-size:12px;color:#1c1a12;width:130px" ]
           , publishRackChip
           , publishStatus s
           ]
    )

-- Publish the active rack to the Amphora store (⚱); a sibling of + NEW.
publishRackChip :: forall m. H.ComponentHTML Action Slots m
publishRackChip =
  HH.button
    [ HE.onClick \_ -> PublishRack
    , HP.title "publish the active rack to the Amphora store"
    , style $ "padding:5px 11px;border:1px solid #8aa08a;border-radius:6px;cursor:pointer;"
        <> "font-family:Georgia,serif;font-size:11px;color:#3d5c3b;background:#00000006" ]
    [ HH.text "⚱ PUBLISH" ]

publishStatus :: forall m. State -> H.ComponentHTML Action Slots m
publishStatus s = case s.publishMsg of
  Nothing -> HH.text ""
  Just msg ->
    HH.span [ style $ engrave <> ";font-size:8px;color:#5a7458;margin-left:4px" ]
      [ HH.text msg ]

rackChip :: forall m. String -> Boolean -> Action -> H.ComponentHTML Action Slots m
rackChip label active act =
  HH.button
    [ HE.onClick \_ -> act
    , style $ "padding:5px 11px;border:1px solid #a8a392;border-radius:6px;cursor:pointer;"
        <> "font-family:Georgia,serif;font-size:11px;letter-spacing:0.04em;"
        <> (if active then "color:#1c1a12;background:linear-gradient(#8fb0c0,#7a9eb0)"
            else "color:#3f3c33;background:linear-gradient(#efece1,#ddd9cb)") ]
    [ HH.text label ]

newRackChip :: forall m. H.ComponentHTML Action Slots m
newRackChip =
  HH.button
    [ HE.onClick \_ -> NewRack
    , style $ "padding:5px 11px;border:1px dashed #a8a392;border-radius:6px;cursor:pointer;"
        <> "font-family:Georgia,serif;font-size:11px;color:#6a6657;background:#00000006" ]
    [ HH.text "+ NEW" ]

-- A compact horizontal transport: live clock + MIDI readouts, the Apply → rig
-- push button, and the per-bank status readout showing each daemon's OK / claim /
-- ERR reply (#142). The rack's CV/gate is generated by the daemons once applied.
transportStrip :: forall m. State -> H.ComponentHTML Action Slots m
transportStrip s =
  HH.div
    [ style $ "display:flex;align-items:center;gap:14px;margin-bottom:14px;padding:8px 10px;"
        <> "border-radius:7px;background:#00000008;border:1px solid #00000012;flex-wrap:wrap" ]
    -- ARM now lives on the tab dot in the top switcher; this strip keeps the readouts.
    ( [ stat "TEMPO" (show (round s.clockTempo) <> " bpm" <> (if s.clockLocked then " ⛓" else " ·"))
      , stat "BAR" (show s.clockBar <> " · step " <> show (s.playStep + 1) <> "/" <> show cycleSteps)
      , stat "MIDI" s.midiName
      , applyButton
      ]
        <> replyReadout s )
  where
  stat label val =
    HH.div [ style "display:flex;flex-direction:column;gap:1px" ]
      [ HH.span [ style $ engrave <> ";font-size:8px;opacity:0.55" ] [ HH.text label ]
      , HH.span [ style $ "font-family:'SF Mono',Menlo,monospace;font-size:10px;color:" <> ink ] [ HH.text val ]
      ]

applyButton :: forall m. H.ComponentHTML Action Slots m
applyButton =
  HH.button
    [ HE.onClick \_ -> ApplyToRig
    , style $ "padding:7px 14px;border:1px solid #6f8fa0;border-radius:6px;cursor:pointer;"
        <> "font-family:Georgia,serif;font-size:11px;letter-spacing:0.06em;color:#1c2a30;"
        <> "background:linear-gradient(#9fc0d0,#7a9eb0)" ]
    [ HH.text "APPLY → RIG" ]

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

addBar :: forall m. H.ComponentHTML Action Slots m
addBar =
  HH.div [ style "display:flex;align-items:center;gap:8px;margin-top:14px" ]
    ( [ HH.span [ style $ engrave <> ";font-size:9px;opacity:0.6;margin-right:2px" ] [ HH.text "ADD 8 →" ] ]
        <> map addButton M.allKinds
    )

addButton :: forall m. M.GenKind -> H.ComponentHTML Action Slots m
addButton k =
  HH.button
    [ HE.onClick \_ -> AddDest k
    , style $ "padding:6px 11px;border:1px solid #a8a392;border-radius:6px;cursor:pointer;"
        <> "font-family:Georgia,serif;font-size:11px;letter-spacing:0.08em;color:#3f3c33;"
        <> "background:linear-gradient(#efece1,#ddd9cb)" ]
    [ HH.text (M.kindLabel k) ]

footNote :: forall m. H.ComponentHTML Action Slots m
footNote =
  HH.div [ style $ engrave <> ";font-size:8px;opacity:0.45;margin-top:14px;line-height:1.6" ]
    [ HH.text "EACH DESTINATION = 8 SIGNALS → 8 JACKS. EDIT THE NUMBERS — AND RE-PATCH / REMOVE BLOCKS — IN THE SOURCE PANE. -- MUTES A SLOT." ]

-- One destination: a target/header strip on the left, eight visualised slots.
destinationRow :: forall m. MonadAff m => EnvParam -> Maybe Sel -> Int -> M.Destination -> H.ComponentHTML Action Slots m
destinationRow ep sel i d =
  HH.div
    [ style $ "display:flex;align-items:stretch;gap:12px;padding:11px 12px;margin-bottom:10px;border-radius:8px;"
        <> "background:#00000008;border:1px solid #00000012" ]
    [ destHeader i d
    , HH.div [ style (slotWrap d.bank) ] (slotViews ep i sel d.bank)
    ]

-- | The slot layout: the CV/gate kinds flow eight-across. (POLYTRIG's 4×2
-- | mini-notation grid went with it to Balistes' TIDAL tab.)
slotWrap :: M.GenBank -> String
slotWrap _ = "display:flex;flex-wrap:wrap;gap:7px;align-items:center;flex:1 1 auto"

destHeader :: forall m. MonadAff m => Int -> M.Destination -> H.ComponentHTML Action Slots m
destHeader i d =
  HH.div [ style "display:flex;flex-direction:column;gap:5px;flex:0 0 190px;justify-content:center" ]
    [ HH.div [ style $ engrave <> ";font-size:10px;letter-spacing:0.1em;color:" <> ink ]
        [ HH.text (M.kindLabel (M.bankKind d.bank)) ]
    -- The routing atom, now direct-manipulation: the same rig-bounded cascade
    -- menu the routing modal uses, editing this destination's target in place.
    , HH.slot _selTarget i Select.component
        ((Select.cascadingInput (targetGroups defaultRig))
           { selected = Just (M.targetWire d.target), placeholder = "route" })
        (\(Select.Selected wire) -> RetargetDest i wire)
    , HH.span [ style $ engrave <> ";font-size:8px;opacity:0.45" ]
        [ HH.text ("→ " <> M.targetWire d.target) ]
    ]

-- ---------------------------------------------------------------------------
-- Per-kind slot visualisations
-- ---------------------------------------------------------------------------

-- Each slot is drawn inside a focusable cell: clicking selects it (a black box);
-- arrow keys then nudge it. `slotViews` threads the destination index + current
-- selection so each cell knows whether it's the selected one and what to fire.
slotViews :: forall m. EnvParam -> Int -> Maybe Sel -> M.GenBank -> Array (H.ComponentHTML Action Slots m)
slotViews ep d sel = case _ of
  M.GLfo slots -> mapWithIndex (cellFor d sel 90.0 lfoInner) slots
  M.GEuclid slots -> mapWithIndex (cellFor d sel 70.0 euclidInner) slots
  M.GClock slots -> mapWithIndex (cellFor d sel 58.0 clockInner) slots
  M.GNote slots -> mapWithIndex (cellFor d sel 58.0 noteInner) slots
  M.GEnv slots -> mapWithIndex (\j sl -> slotCell d j (sel == Just { dest: d, slot: j }) 90.0 (envInner (sel == Just { dest: d, slot: j }) ep sl)) slots

-- | Wrap one slot's inner drawing in the focusable, selectable cell.
cellFor :: forall m a. Int -> Maybe Sel -> Number -> (a -> Array (H.ComponentHTML Action Slots m)) -> Int -> a -> H.ComponentHTML Action Slots m
cellFor d sel widthPx inner j sl =
  slotCell d j (sel == Just { dest: d, slot: j }) widthPx (inner sl)

slotCell :: forall m. Int -> Int -> Boolean -> Number -> Array (H.ComponentHTML Action Slots m) -> H.ComponentHTML Action Slots m
slotCell d j isSel widthPx body =
  HH.div
    [ HP.tabIndex 0
    , style $ "width:" <> show (round widthPx) <> "px;flex:0 0 auto;padding:5px 4px;border-radius:6px;"
        <> "display:flex;flex-direction:column;gap:2px;align-items:center;cursor:pointer;outline:none;"
        <> ( if isSel then "background:#ffffffcc;border:2px solid #1a1a1a;"
             else "background:#ffffff55;border:1px solid #00000010;padding:6px 5px;" )
    , HE.onClick \_ -> SelectSlot d j
    , HE.onKeyDown SlotKeyDown ]
    body

-- --- POLYLFO: a scaled waveform, 0V baseline, log-frequency, rate label ------

lfoInner :: forall m. M.ModSlot -> Array (H.ComponentHTML Action Slots m)
lfoInner sl =
  let
    w = 84.0
    h = 50.0
    mid = h / 2.0
    -- normalise the summed wave into the cell (90% of half-height)
    span = max 1.0 (abs sl.level + sl.sin + sl.sqr + sl.tri + abs sl.saw)
    samples = 48
    pt k =
      let
        u = toNumber k / toNumber samples
        x = u * w
        y = mid - (M.lfoValue sl u / span) * (mid * 0.9)
      in
        show (round2 x) <> "," <> show (round2 y)
    poly = joinWith " " (map pt (range 0 samples))
    hint = lfoShapeHint sl
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
    , cellCaption (shapeName (activeShape sl) <> " · " <> fmt2 sl.rate <> " Hz" <> hint)
    ]

-- which non-drawn shapes are present, as a tiny tag
lfoShapeHint :: M.ModSlot -> String
lfoShapeHint sl =
  let tags = filter (_ /= "") [ if sl.rnd > 0.0 then "+rnd" else "", if sl.nse > 0.0 then "+nse" else "" ]
  in if length tags == 0 then "" else " " <> joinWith "" tags

-- --- POLYEUCLID: a ring of step-dots with k / n in the centre ----------------

euclidInner :: forall m. M.EuclidSlot -> Array (H.ComponentHTML Action Slots m)
euclidInner sl = [ Euclid.ring euclidStyle Nothing { beats: sl.beats, steps: sl.steps } ]

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
        <> band
        <> [ line fig.hi 2.0 "1" ]
        <> (if fig.hasBand then [ line fig.lo 1.0 "0.5" ] else [])
        <> (if isSel then handles else []) )
  where
  -- Grab points, at the three breakpoints of the drawn curve. `hi` is the
  -- velocity-127 outline, which is the one the numbers actually describe.
  handles =
    [ grip HPeak (fig.hi !! 1)
    , grip HCorner (fig.hi !! 2)
    , grip HTail (fig.hi !! 4)
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
  -- The velocity band: the same shape at velocity 127 and at 1, filled between.
  -- Its WIDTH is how much velocity does — a static envelope has none at all.
  band =
    if fig.hasBand
      then [ svgEl "polygon"
               [ svgAttr "points" (pts fig.band)
               , svgAttr "fill" fig.ink
               , svgAttr "fill-opacity" "0.18"
               , svgAttr "stroke" "none" ] [] ]
      else []
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

sourcePanel :: forall m. State -> H.ComponentHTML Action Slots m
sourcePanel s =
  panel "SOURCE" "flex:0 0 340px"
    [ HH.div [ style $ engrave <> ";font-size:8px;opacity:0.6;margin-bottom:6px" ]
        [ HH.text "THE RACK · EDIT THE NUMBERS · -- MUTES A SLOT" ]
    , HH.textarea
        [ HP.value (currentDoc s)
        , HE.onValueInput SetDoc
        , HP.spellcheck false
        , style $ "flex:1 1 auto;min-height:420px;resize:none;box-sizing:border-box;"
            <> "padding:9px 10px;border:1px solid #a8a392;border-radius:6px;background:#f4f1e8;"
            <> "font-family:'SF Mono',Menlo,monospace;font-size:11px;line-height:1.5;color:#2b2922;"
            <> "white-space:pre;overflow:auto;outline:none" ]
    ]

-- ---------------------------------------------------------------------------
-- helpers
-- ---------------------------------------------------------------------------

abs :: Number -> Number
abs x = if x < 0.0 then -x else x

round2 :: Number -> Number
round2 x = toNumber (round (x * 100.0)) / 100.0

fmt2 :: Number -> String
fmt2 x = show (toNumber (round (x * 100.0)) / 100.0)
