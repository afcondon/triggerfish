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

import Data.Array (any, deleteAt, drop, filter, length, mapMaybe, mapWithIndex, modifyAt, null, range, (!!))
import Data.Foldable (for_, foldr)
import Data.Int (round, toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Number (cos, pi, sin) as Num
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
import Triggerfish.Odonus.Grid.Widgets (engrave, style, svgAttr, svgEl)
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
  }

data Action
  = Initialize
  | Step Scheduler.Tick
  | Frame
  | MidiReady (Maybe Midi.MidiOut) String
  | AddDest M.GenKind         -- append a template block (comment-safe)
  | SetDoc String             -- the whole editable document, verbatim
  | RetargetDest Int String   -- re-route destination i to a target wire (cascade menu)
  | SelectRack Int            -- load a library rack into the editor
  | NewRack                   -- append a fresh empty rack + select it
  | SetRackName String        -- rename the active rack
  | ApplyToRig                -- push every modular destination to its daemon
  | SeleneReply String        -- a raw `selene-reply …` frame from the rig
  | PublishRack               -- publish the active rack to the Amphora store (selene-rack)

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
          }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction, handleQuery = handleQuery, initialize = Just Initialize }
    }

handleQuery :: forall m a. MonadAff m => Query a -> H.HalogenM State Action Slots Output m (Maybe a)
handleQuery = case _ of
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
      let block = Source.printDest { target: M.defaultTargetFor k, range: M.Bipolar5V, bank: M.freshBank k }
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
        <> mapWithIndex destinationRow s.sel.destinations
        <> [ addBar, footNote ]
    )

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
destinationRow :: forall m. MonadAff m => Int -> M.Destination -> H.ComponentHTML Action Slots m
destinationRow i d =
  HH.div
    [ style $ "display:flex;align-items:stretch;gap:12px;padding:11px 12px;margin-bottom:10px;border-radius:8px;"
        <> "background:#00000008;border:1px solid #00000012" ]
    [ destHeader i d
    , HH.div [ style (slotWrap d.bank) ] (slotViews d.bank)
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

slotViews :: forall m. M.GenBank -> Array (H.ComponentHTML Action Slots m)
slotViews = case _ of
  M.GLfo slots -> map lfoCell slots
  M.GEuclid slots -> map euclidRing slots
  M.GClock slots -> mapWithIndex clockNumber slots
  M.GNote slots -> map noteCell slots

-- --- POLYLFO: a scaled waveform, 0V baseline, log-frequency, rate label ------

lfoCell :: forall m. M.ModSlot -> H.ComponentHTML Action Slots m
lfoCell sl =
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
    cellBox 90.0
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
      , cellCaption (fmt2 sl.rate <> " Hz" <> hint)
      ]

-- which non-drawn shapes are present, as a tiny tag
lfoShapeHint :: M.ModSlot -> String
lfoShapeHint sl =
  let tags = filter (_ /= "") [ if sl.rnd > 0.0 then "+rnd" else "", if sl.nse > 0.0 then "+nse" else "" ]
  in if length tags == 0 then "" else " " <> joinWith "" tags

-- --- POLYEUCLID: a ring of step-dots with k / n in the centre ----------------

euclidRing :: forall m. M.EuclidSlot -> H.ComponentHTML Action Slots m
euclidRing sl = cellBox 70.0 [ ringFigure 64.0 sl.beats sl.steps ]

-- | The Euclidean ring — a dot per step, filled on a pulse, k/n in the centre.
-- | Drawn for every POLYEUCLID slot (structure-driven viz).
ringFigure :: forall m. Number -> Int -> Int -> H.ComponentHTML Action Slots m
ringFigure sz k n =
  let
    c = sz / 2.0
    r = c - 8.0
    bits = M.euclidBits { beats: k, steps: n, rate: 0, accentRate: 0 }
    nb = length bits
    dotFor i on =
      let
        ang = (toNumber i / toNumber (max 1 nb)) * 2.0 * pi - pi / 2.0
        dx = c + r * cos ang
        dy = c + r * sin ang
        rad = if on then 3.4 else 2.0
      in
        svgEl "circle"
          [ svgAttr "cx" (show (round2 dx)), svgAttr "cy" (show (round2 dy)), svgAttr "r" (show rad)
          , svgAttr "fill" (if on then accent else "none")
          , svgAttr "stroke" accent, svgAttr "stroke-width" (if on then "0" else "1") ] []
  in
    svgEl "svg"
      [ svgAttr "viewBox" ("0 0 " <> show sz <> " " <> show sz), svgAttr "width" "100%"
      , svgAttr "height" (show sz), svgAttr "style" "display:block" ]
      ( mapWithIndex dotFor bits
          <> [ svgEl "text"
                 [ svgAttr "x" (show c), svgAttr "y" (show (c + 4.0)), svgAttr "text-anchor" "middle"
                 , svgAttr "fill" ink, svgAttr "font-family" "'SF Mono',Menlo,monospace"
                 , svgAttr "font-size" "13" ]
                 [ HH.text (show k <> "/" <> show n) ]
             ]
      )

-- --- POLYCLOCK: a list of division numbers -----------------------------------

clockNumber :: forall m. Int -> M.ClockSlot -> H.ComponentHTML Action Slots m
clockNumber _ sl =
  cellBox 58.0
    [ HH.div [ style $ "font-family:'SF Mono',Menlo,monospace;font-size:18px;color:" <> ink <> ";text-align:center" ]
        [ HH.text ("×" <> show sl.multiplier) ]
    , cellCaption (M.clockBaseLabel sl.base <> " · " <> show sl.pulseWidth <> "%")
    ]

-- --- POLYNOTE: a list of notes -----------------------------------------------

noteCell :: forall m. M.PresetNoteSlot -> H.ComponentHTML Action Slots m
noteCell sl =
  cellBox 58.0
    [ HH.div [ style $ "font-family:'SF Mono',Menlo,monospace;font-size:16px;color:" <> ink <> ";text-align:center" ]
        [ HH.text (M.noteName sl.note) ]
    , cellCaption ("midi " <> show sl.note)
    ]

-- ---------------------------------------------------------------------------
-- Cell chrome
-- ---------------------------------------------------------------------------

cellBox :: forall m. Number -> Array (H.ComponentHTML Action Slots m) -> H.ComponentHTML Action Slots m
cellBox widthPx body =
  HH.div
    [ style $ "width:" <> show (round widthPx) <> "px;flex:0 0 auto;padding:5px 4px;border-radius:6px;"
        <> "background:#ffffff55;border:1px solid #00000010;display:flex;flex-direction:column;gap:2px;align-items:center" ]
    body

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

pi :: Number
pi = Num.pi

cos :: Number -> Number
cos = Num.cos

sin :: Number -> Number
sin = Num.sin

abs :: Number -> Number
abs x = if x < 0.0 then -x else x

round2 :: Number -> Number
round2 x = toNumber (round (x * 100.0)) / 100.0

fmt2 :: Number -> String
fmt2 x = show (toNumber (round (x * 100.0)) / 100.0)
