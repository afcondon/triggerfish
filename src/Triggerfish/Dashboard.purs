-- | **The Atlantis dashboard**: one page for the rig as a whole.
-- | See `docs/kb/plans/dashboard.md`.
-- |
-- | First build, the three parts AC marked core:
-- |
-- |   * **transport and mode** (1a–1d): which machines are open, each one's play
-- |     and stop, the tempo and Link, Solo/Atlantis for every page, Panic;
-- |   * **routing** (2a): the whole table, every source, in one place;
-- |   * **status and stage** (3a, 4a): the rig link, and each machine's preset.
-- |
-- | It reaches the machines' pages over the tab bus (`Triggerfish.TabBus`), which
-- | needs no rig, and the mode through the shared store. It has its own rig socket
-- | for the clock, Panic and auditioning a leg. It plays nothing itself.
-- |
-- | Each machine keeps its nameplate from the identity study: its own colour,
-- | face and fish. Everything else is standard, and styled by `dashboard.html`.
-- |
-- | Two views so far, each at its own address: the machines (`#`, the landing)
-- | and routing (`#routing`), so the table is not the first thing seen. More are
-- | planned (process management, documentation); see the plan.
module Triggerfish.Dashboard
  ( component
  ) where

import Prelude

import Binnacle as Binnacle
import Binnacle.Clock as Clock
import Binnacle.Midi as Midi
import Binnacle.Time (dateNow)
import Binnacle.Transport as Transport
import Data.Array (elem, filter, find, length, nubEq, null, (..))
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Traversable (traverse)
import Data.Number.Format (fixed, toStringWith)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Timer (setInterval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Triggerfish.Glyph as G
import Triggerfish.Fish as Fish
import Triggerfish.Flow as Flow
import Triggerfish.Flow.View as FlowView
import Triggerfish.GlyphView (chipIcons)
import Triggerfish.Route as Route
import Triggerfish.Routing.Edit as RE
import Triggerfish.Routing.Model as RM
import Triggerfish.Routing.Out as RO
import Triggerfish.Routing.Store as RStore
import Triggerfish.Routing.View as RV
import Triggerfish.SampleSets (SampleSet)
import Triggerfish.SampleSets as SampleSets
import Triggerfish.TabBus as Bus
import Triggerfish.Transport (Mode(..))
import Triggerfish.Transport.Store as TransportStore

-- | A machine as the dashboard knows it: its slot, its nameplate, and where its
-- | page is. `onBus` is false for the pages not yet on the tab bus, which the
-- | dashboard can open but not see or drive.
type Machine =
  { slot :: String
  , name :: String
  , href :: String
  , target :: String
  , onBus :: Boolean
  }

machines :: Array Machine
machines =
  [ { slot: "odonus", name: "Odonus", href: "/#odonus", target: "atlantis-triggerfish", onBus: true }
  , { slot: "vetula", name: "Vetula", href: "/#vetula", target: "atlantis-triggerfish", onBus: true }
  , { slot: "balistes", name: "Balistes", href: "/balistes.html", target: "atlantis-balistes", onBus: true }
  , { slot: "selene", name: "Selene", href: "/selene.html", target: "atlantis-selene", onBus: true }
  , { slot: "conspicillum", name: "Conspicillum", href: "/conspicillum/", target: "atlantis-conspicillum", onBus: false }
  , { slot: "quadrat", name: "quadrat", href: "/quadrat.html", target: "atlantis-quadrat", onBus: false }
  ]

-- | A tab that has not been heard from for this long is taken to be closed. The
-- | shells announce every 1.5 s.
openWindowMs :: Number
openWindowMs = 4500.0

type Heard = { state :: Bus.MachineState, at :: Number }

-- | The page's views. Each is a real link (`#routing`), so the back button and
-- | bookmarks work.
data View = MachinesView | RoutingView

derive instance Eq View

viewOf :: String -> View
viewOf = case _ of
  "routing" -> RoutingView
  _ -> MachinesView

type State =
  { mode :: Mode
  , view :: View
  , now :: Number
  , heard :: Map String Heard
  , rig :: Maybe Binnacle.Binnacle
  , rigUp :: Boolean
  , tempo :: Number
  , locked :: Boolean
  , bus :: Maybe Bus.Bus
  , table :: RM.Table
  , ports :: Array String
  , sampleSets :: Array SampleSet
  , hot :: Maybe String      -- the machine hovered on the chart
  , voices :: Array String   -- machines the chart draws as their voices
  }

data Action
  = Init
  | Tick
  | FromBus Bus.Msg
  | ModeStored
  | SetMode Mode
  | Command String Boolean
  | StopAll
  | Panic
  | RoutingStored
  | SetPorts (Array String)
  | Edit RE.Edit
  | Audition RM.Destination
  | ShowView View
  | Hover (Maybe String)
  | ToggleVoices String

component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ ->
      { mode: Solo, view: MachinesView, now: 0.0, heard: Map.empty, rig: Nothing, rigUp: false
      , tempo: 120.0, locked: false, bus: Nothing
      , table: RM.defaultTable, ports: [], sampleSets: [], hot: Nothing, voices: [] }
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Init }
  }

type M o = H.HalogenM State Action () o Aff

handleAction :: forall o. Action -> M o Unit
handleAction = case _ of
  Init -> do
    liftEffect Fish.install
    { emitter, listener } <- liftEffect HS.create
    _ <- H.subscribe emitter
    hash <- liftEffect Route.readHash
    H.modify_ _ { view = viewOf hash }
    liftEffect $ Route.onHashChange (HS.notify listener <<< ShowView <<< viewOf)
    mmode <- liftEffect TransportStore.load
    for_ mmode \m -> H.modify_ _ { mode = m }
    liftEffect $ TransportStore.onChange (HS.notify listener ModeStored)
    handleAction RoutingStored
    liftEffect $ RStore.onChange (HS.notify listener RoutingStored)
    rig <- liftEffect $ Binnacle.connect { url: rigUrl, tempo: 120.0 }
    H.modify_ _ { rig = Just rig }
    bus <- liftEffect Bus.open
    H.modify_ _ { bus = Just bus }
    liftEffect $ Bus.onMessage bus (HS.notify listener <<< FromBus)
    -- Ask every open page to say what it is now, rather than on its next tick.
    liftEffect $ Bus.post bus Bus.Hello
    liftEffect $ Midi.requestAccess case _ of
      Just access -> Midi.outputNames access >>= HS.notify listener <<< SetPorts
      Nothing -> HS.notify listener (SetPorts [])
    void $ H.fork do
      sets <- liftAff SampleSets.load
      H.modify_ _ { sampleSets = sets }
    _ <- liftEffect $ setInterval 1000 (HS.notify listener Tick)
    handleAction Tick

  -- Once a second: the clock, the rig link, and which tabs have gone quiet.
  -- State is written only when something shown changes.
  Tick -> do
    st <- H.get
    now <- liftEffect dateNow
    up <- case st.rig of
      Nothing -> pure false
      Just bin -> liftEffect $ Transport.isConnected (Binnacle.socket bin)
    reading <- traverse (\bin -> liftEffect (Clock.read (Binnacle.clock bin))) st.rig
    let
      tempo = maybe st.tempo _.tempo reading
      locked = maybe false _.locked reading
      openBefore = map _.slot (filter (isOpen st) machines)
      openAfter = map _.slot (filter (isOpen st { now = now }) machines)
    when (up /= st.rigUp || tempo /= st.tempo || locked /= st.locked || openBefore /= openAfter || st.now == 0.0)
      (H.modify_ _ { now = now, rigUp = up, tempo = tempo, locked = locked })

  FromBus msg -> case msg of
    Bus.State s -> do
      now <- liftEffect dateNow
      st <- H.get
      let changed = map _.state (Map.lookup s.machine st.heard) /= Just s || wasClosed st s.machine
      -- Keep the time heard without re-rendering on every heartbeat.
      if changed
        then H.modify_ \x -> x { heard = Map.insert s.machine { state: s, at: now } x.heard, now = now }
        else H.modify_ \x -> x { heard = Map.insert s.machine { state: s, at: now } x.heard }
    _ -> pure unit

  ModeStored -> do
    mmode <- liftEffect TransportStore.load
    for_ mmode \m -> H.modify_ _ { mode = m }

  -- The dashboard's switch is the mode for every page: saved, and every page
  -- follows through its storage listener.
  SetMode m -> do
    H.modify_ _ { mode = m }
    liftEffect $ TransportStore.save m

  Command slot play -> post (if play then Bus.Play slot else Bus.Stop slot)

  StopAll -> do
    st <- H.get
    for_ machines \m -> when (playing st m) (post (Bus.Stop m.slot))

  -- Kill every voice on the rig, orphans included, and stop every page's
  -- transport. The same PANIC as each page's, for all of them at once.
  Panic -> do
    st <- H.get
    for_ st.rig \bin -> liftEffect $ Transport.send (Binnacle.socket bin) "hush"
    post Bus.Panic

  RoutingStored -> do
    mtbl <- liftEffect RStore.load
    for_ mtbl \t -> H.modify_ _ { table = t }

  -- At first run, with nothing stored, the default table is made for the ports
  -- this machine has and saved, as every other page with a router does.
  SetPorts ns -> do
    H.modify_ _ { ports = ns }
    stored <- liftEffect RStore.load
    when (isNothing stored) do
      let t = RM.defaultTableFor ns
      liftEffect $ RStore.save t
      H.modify_ _ { table = t }

  -- An edit is saved, and every page with a machine that reads the table picks
  -- it up through its storage listener.
  Edit e -> do
    st <- H.get
    for_ (RE.apply { ports: st.ports, sampleSets: st.sampleSets } e st.table) \t -> do
      liftEffect $ RStore.save t
      H.modify_ _ { table = t }

  ShowView v -> H.modify_ _ { view = v }

  Hover m -> H.modify_ _ { hot = m }

  ToggleVoices m -> H.modify_ \x -> x { voices = if m `elem` x.voices then filter (_ /= m) x.voices else x.voices <> [ m ] }

  Audition dest -> do
    st <- H.get
    for_ (RO.auditionLine dest) \line ->
      for_ st.rig \bin -> liftEffect $ Transport.send (Binnacle.socket bin) line
  where
  post msg = do
    mbus <- H.gets _.bus
    for_ mbus \bus -> liftEffect $ Bus.post bus msg

-- | Whether a message is from a machine the dashboard lists and has not heard
-- | from lately.
wasClosed :: State -> String -> Boolean
wasClosed st slot = maybe true (not <<< isOpen st) (find (\m -> m.slot == slot) machines)

isOpen :: State -> Machine -> Boolean
isOpen st m = case Map.lookup m.slot st.heard of
  Just h -> st.now - h.at < openWindowMs
  Nothing -> false

playing :: State -> Machine -> Boolean
playing st m = isOpen st m && maybe false _.state.playing (Map.lookup m.slot st.heard)

-- ---------------------------------------------------------------------------
-- View
-- ---------------------------------------------------------------------------

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  HH.div [ cls "dash" ]
    [ topBar st
    , HH.main [ cls "body" ]
        [ case st.view of
            MachinesView ->
              HH.div_
                [ flowChart st
                , HH.section [ cls "machines", HP.attr (AttrName "aria-label") "Triggerfish machines" ]
                    (map (card st) machines)
                ]
            RoutingView -> routing st
        ]
    ]

-- | The signal-flow chart: what the open pages drive, by the path the mode
-- | gives them. Conspicillum and Quadrat route themselves rather than through
-- | the table, so their routes are stated here; they appear once their pages
-- | are on the tab bus.
flowChart :: forall m. State -> H.ComponentHTML Action () m
flowChart st =
  HH.section [ cls "flow", HP.attr (AttrName "aria-label") "Where it all goes" ]
    [ HH.div [ cls "flow-chart" ]
        [ FlowView.chart { hover: Hover, pick: ToggleVoices } st.hot
            ( Flow.flow
                { mode: st.mode
                , table: st.table
                , ports: { found: st.ports, rigUp: st.rigUp }
                , machines: map _.slot (filter (isOpen st) machines)
                , open: st.voices
                , extras
                }
            )
        ]
    , FlowView.key
    ]
  where
  extras =
    [ { machine: "conspicillum", dest: RM.DSample { set: "", n: 0, begin: 0, end: 100, reverse: false, gain: 100, chop: 1 }, via: Nothing }
    , { machine: "quadrat", dest: RM.DEs9Cv { bus: 1 }, via: Just "foi" }
    ]

topBar :: forall m. State -> H.ComponentHTML Action () m
topBar st =
  HH.header [ cls "top" ]
    [ HH.div [ cls "row" ]
        -- The brand is the way home (the landing); the tabs are the other views.
        ( [ HH.a
              ( [ cls "brand", HP.href "#", HP.title "Triggerfish: home" ]
                  <> (if st.view == MachinesView then [ HP.attr (AttrName "aria-current") "page" ] else [])
              )
              [ HH.text "Triggerfish" ]
        , HH.nav [ cls "tabs", HP.attr (AttrName "aria-label") "Views" ]
            [ tab RoutingView "#routing" "Routing" ]
        , HH.div [ cls "seg", HP.attr (AttrName "role") "group", HP.attr (AttrName "aria-label") "Mode" ]
            [ seg "Solo" Solo, seg "Atlantis" Atlantis ]
        , HH.button [ cls "btn", HE.onClick \_ -> StopAll, HP.disabled (not anyPlaying) ] [ HH.text "■ Stop all" ]
        ]
          -- The rig's tempo and Link only mean something in Atlantis. In Solo each
          -- page keeps its own tempo, so the dashboard has none to show.
          <> atlantisOnly
            [ HH.span [ cls "tempo" ]
                [ HH.span [ cls "num" ] [ HH.text (toStringWith (fixed 1) st.tempo) ]
                , HH.text " bpm"
                ]
            , lamp st.locked (if st.locked then "Link" else "free-running")
            ]
          <> [ HH.span [ cls "spacer" ] []
             , HH.button [ cls "btn panic", HE.onClick \_ -> Panic ] [ HH.text "Panic" ]
             ]
        )
    , HH.div [ cls "row status" ]
        ( atlantisOnly [ lamp st.rigUp (if st.rigUp then "rig connected" else "no rig") ]
        <> [ lamp (not (null st.ports)) (portsNote st.ports)
        , HH.span [ cls "note" ]
            [ HH.text case st.mode of
                Solo -> "Solo: each page plays through MIDI on this computer."
                Atlantis -> "Atlantis: the rig plays; pages send it what to play."
            ]
        ])
    ]
  where
  -- Progressive disclosure: the rig's instruments appear only when the rig is in
  -- use. Solo shows what a newcomer with a browser and a synth needs, no more.
  atlantisOnly xs = if st.mode == Atlantis then xs else []
  anyPlaying = not (null (filter (playing st) machines))
  -- A plain link: the browser moves the hash and keeps history; the page follows
  -- through its hashchange listener.
  tab v href label =
    HH.a
      ( [ cls ("tab" <> if st.view == v then " on" else "")
        , HP.href (if href == "" then "#" else href)
        ]
          <> (if st.view == v then [ HP.attr (AttrName "aria-current") "page" ] else [])
      )
      [ HH.text label ]
  seg label m =
    HH.button
      [ cls (if st.mode == m then "on" else "")
      , HP.attr (AttrName "aria-pressed") (if st.mode == m then "true" else "false")
      , HE.onClick \_ -> SetMode m
      ]
      [ HH.text label ]
  portsNote ns = case length ns of
    0 -> "no MIDI outputs"
    1 -> "1 MIDI output"
    n -> show n <> " MIDI outputs"

lamp :: forall w i. Boolean -> String -> HH.HTML w i
lamp on label =
  HH.span [ cls ("lamp" <> if on then " live" else "") ] [ HH.i_ [], HH.text label ]

card :: forall m. State -> Machine -> H.ComponentHTML Action () m
card st m =
  HH.article [ cls ("card m-" <> m.slot <> if open then "" else " closed") ]
    [ HH.a [ cls "nameplate", HP.href m.href, HP.target m.target, HP.title ("Open " <> m.name) ]
        [ HH.span [ cls "roundel" ] [ Fish.icon "ico" m.slot ]
        , HH.span [ cls ("wordmark w-" <> m.slot) ] [ HH.text m.name ]
        ]
    , HH.div [ cls "controls" ]
        ( if not m.onBus then
            [ HH.span [ cls "note" ] [ HH.text "not on the dashboard yet" ] ]
          else if not open then
            [ HH.a [ cls "btn", HP.href m.href, HP.target m.target ] [ HH.text "Open ↗" ] ]
          else
            [ HH.button
                [ cls ("btn" <> if isPlaying then " on" else "")
                , HE.onClick \_ -> Command m.slot (not isPlaying)
                ]
                [ HH.text (if isPlaying then "■ Stop" else "▶ Play") ]
            , chip
            ]
        )
    , HH.div [ cls "meta" ] [ lamp open (if open then "tab open" else "closed") ]
    ]
  where
  open = isOpen st m
  isPlaying = playing st m
  heard = Map.lookup m.slot st.heard
  chip = case heard >>= _.state.alias of
    Just alias ->
      HH.span [ cls ("chip" <> if edited then " edited" else "") ]
        [ chipIcons (Just { glyph: G.glyphFromAlias alias, diverged: edited })
        , HH.span [ cls "alias" ] [ HH.text alias ]
        ]
    Nothing -> HH.span [ cls "chip empty" ] [ HH.text "no preset" ]
  edited = fromMaybe false (map _.state.edited heard)

-- | The whole table, one ledger, each source drawn by the same rows as every
-- | other router.
routing :: forall m. State -> H.ComponentHTML Action () m
routing st =
  HH.section [ cls "routing", HP.attr (AttrName "aria-label") "Routing" ]
    [ HH.div [ cls "sectionhead" ]
        [ HH.h2_ [ HH.text "Routing" ]
        , HH.span [ cls "note" ] [ HH.text "Every source and where it goes. Changes save at once and reach every open page." ]
        ]
    , RV.key env allSources
    , RV.sourceRows env allSources
    ]
  where
  -- Each row names its machine with its fish, so there are no group headings;
  -- the order is the machines' order.
  allSources = odonusHeads <> vetulaVoices <> drumLanes <> seleneBanks
  odonusHeads = map RM.SOdonusHead (0 .. 3)
  drumLanes = map RM.SDrumLane (0 .. 15)
  env =
    { table: st.table, ports: st.ports, rigUp: st.rigUp, sampleSets: st.sampleSets
    , onEdit: Edit, onAudition: Audition }
  sources = map _.source st.table
  vetulaVoices = nubEq ([ RM.SVetulaVoice "" ] <> filter isVetula sources)
  seleneBanks = filter isSelene sources
  isVetula = case _ of
    RM.SVetulaVoice _ -> true
    _ -> false
  isSelene = case _ of
    RM.SSeleneBank _ -> true
    _ -> false

cls :: forall r i. String -> HP.IProp (class :: String | r) i
cls = HP.class_ <<< H.ClassName

rigUrl :: String
rigUrl = "ws://127.0.0.1:3012/ws"
