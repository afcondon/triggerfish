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
-- | It reaches the machines' pages over the tab bus (`Binnacle.TabBus`), which
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
import Data.Array (elem, filter, find, foldl, nubEq, null)
import Data.Array as Array
import Data.Int as Int
import Data.String (Pattern(..), stripPrefix)
import Data.String as String
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Traversable (traverse)
import Data.Number.Format (fixed, toStringWith)
import Effect (Effect)
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
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME
import Triggerfish.Flow as Flow
import Triggerfish.Flow.View as FlowView
import Triggerfish.GlyphView (chipIcons)
import Triggerfish.Route as Route
import Triggerfish.Router as Router
import Triggerfish.Routing.Matrix as Matrix
import Reef.Route as HarmonyRoute
import Triggerfish.Routing.Edit as RE
import Triggerfish.Routing.Model as RM
import Triggerfish.Routing.Out as RO
import Triggerfish.Routing.Store as RStore
import Triggerfish.SampleSets (SampleSet)
import Triggerfish.SampleSets as SampleSets
import Binnacle.TabBus as Bus
import Triggerfish.Transport (Mode(..))
import Triggerfish.Transport.Store as TransportStore

-- | A machine as the dashboard knows it: its slot, its nameplate, and where its
-- | page is. `playable` is false for Quadrat, whose "playing" is a capture
-- | being armed: the dashboard shows it but must not start a take or cut one.
type Machine =
  { slot :: String
  , name :: String
  , href :: String
  , target :: String
  , playable :: Boolean
  }

machines :: Array Machine
machines =
  [ { slot: "odonus", name: "Odonus", href: "/odonus.html", target: "atlantis-odonus", playable: true }
  , { slot: "vetula", name: "Vetula", href: "/vetula.html", target: "atlantis-vetula", playable: true }
  , { slot: "balistes", name: "Balistes", href: "/balistes.html", target: "atlantis-balistes", playable: true }
  , { slot: "selene", name: "Selene", href: "/selene.html", target: "atlantis-selene", playable: true }
  , { slot: "conspicillum", name: "Conspicillum", href: "/conspicillum/", target: "atlantis-conspicillum", playable: true }
  , { slot: "quadrat", name: "Quadrat", href: "/quadrat.html", target: "atlantis-quadrat", playable: false }
  -- Not a machine but the editor that plays them; it hushes rather than stops.
  , { slot: "limulus", name: "Limulus", href: "/limulus/", target: "atlantis-limulus", playable: false }
  ]

-- | A tab that has not been heard from for this long is taken to be closed.
-- | The shells announce every 1.5 s, but a browser slows a background tab's
-- | timers to once a minute after a few minutes, so silence is a poor sign of
-- | a closed tab. A closing page says `Bye`; this is only the fallback for one
-- | that could not (a crash, a killed browser).
openWindowMs :: Number
openWindowMs = 90000.0

type Heard = { state :: Bus.MachineState, at :: Number }

foreign import openInBackground :: String -> Effect Unit

-- | The page's views. Each is a real link (`#routing`), so the back button and
-- | bookmarks work.
-- | The hash names an open matrix: `#notes`, `#drums` (and the old `#routing`,
-- | which was the routing page, opens the notes).
matrixOfHash :: String -> Maybe Matrix.Grid
matrixOfHash = case _ of
  "notes" -> Just Matrix.Notes
  "routing" -> Just Matrix.Notes
  "drums" -> Just Matrix.Drums
  _ -> Nothing

type State =
  { mode :: Mode
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
  , router :: Router.Router  -- the harmony routes, as the rig's stage holds them
  -- the notes or drums matrix, when open, its picked cell and lit column
  , matrix :: Maybe Matrix.Grid
  , pick :: Maybe Matrix.Pick
  , focus :: Maybe String
  , sheet :: Maybe String
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
  | FromHash String
  | Hover (Maybe String)
  | OpenMachine Machine MouseEvent
  | ToggleVoices String
  | RigOpen
  | RigFrame String
  | RouterToggle Router.Line HarmonyRoute.Input
  | RouterEdit (Router.Router -> Router.Router)
  | RouterCommit Router.Line
  | NoOp
  | OpenMatrix Matrix.Grid (Maybe String)
  | CloseMatrix
  | PickCell (Maybe Matrix.Pick)
  | OpenSheet (Maybe String)
  | Audition RM.Destination
  | Edits (Array RE.Edit)
  | ChartLink String String

component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ ->
      { mode: Solo, now: 0.0, heard: Map.empty, rig: Nothing, rigUp: false
      , tempo: 120.0, locked: false, bus: Nothing
      , table: RM.defaultTable, ports: [], sampleSets: [], hot: Nothing, voices: [], router: Router.initial
      , matrix: Nothing, pick: Nothing, focus: Nothing, sheet: Nothing }
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
    handleAction (FromHash hash)
    liftEffect $ Route.onHashChange (HS.notify listener <<< FromHash)
    mmode <- liftEffect TransportStore.load
    for_ mmode \m -> H.modify_ _ { mode = m }
    liftEffect $ TransportStore.onChange (HS.notify listener ModeStored)
    handleAction RoutingStored
    liftEffect $ RStore.onChange (HS.notify listener RoutingStored)
    rig <- liftEffect $ Binnacle.connect { url: rigUrl, tempo: 120.0 }
    H.modify_ _ { rig = Just rig }
    -- the stage's text objects: the harmony routes, and Vetula's cards for
    -- its voices; asked for again whenever the socket (re)opens
    liftEffect $ Binnacle.onAppMessage rig (HS.notify listener <<< RigFrame)
    liftEffect $ Binnacle.onOpen rig (HS.notify listener RigOpen)
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
    Bus.Bye slot -> H.modify_ \x -> x { heard = Map.delete slot x.heard }
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

  FromHash h -> case matrixOfHash h of
    Just g -> handleAction (OpenMatrix g Nothing)
    Nothing -> H.modify_ _ { matrix = Nothing, focus = Nothing, pick = Nothing }

  Hover m -> H.modify_ _ { hot = m }

  -- A machine opens behind the dashboard: the dashboard is where you are.
  OpenMachine m ev -> do
    liftEffect $ preventDefault (ME.toEvent ev)
    liftEffect $ openInBackground m.href

  ToggleVoices m -> H.modify_ \x -> x { voices = if m `elem` x.voices then filter (_ /= m) x.voices else x.voices <> [ m ] }

  RigOpen -> sendRig Router.subscribeLine

  RigFrame msg -> do
    st <- H.get
    for_ (Router.readFrame msg st.router) \r -> H.modify_ _ { router = r }

  RouterToggle line input -> do
    st <- H.get
    for_ (Router.toggle line input st.router) sendRig

  RouterEdit f -> H.modify_ \x -> x { router = f x.router }

  RouterCommit line -> do
    st <- H.get
    for_ (Router.commit line st.router) sendRig

  NoOp -> pure unit

  OpenMatrix g focus -> H.modify_ _ { matrix = Just g, focus = focus, pick = Nothing, sheet = Nothing }

  CloseMatrix -> do
    H.modify_ _ { matrix = Nothing, focus = Nothing, pick = Nothing, sheet = Nothing }
    liftEffect (Route.writeHash "")

  PickCell p -> H.modify_ _ { pick = p, sheet = Nothing }

  OpenSheet k -> H.modify_ _ { sheet = k, pick = Nothing }

  -- A sample voice's ▶: played once, now, through the rig.
  Audition dest -> for_ (RO.auditionLine dest) sendRig

  -- Several edits as one change: an added leg, then its port and value. Saved,
  -- and every page with a machine that reads the table picks it up through its
  -- storage listener.
  Edits es -> do
    st <- H.get
    let
      ctx = { ports: st.ports, sampleSets: st.sampleSets }
      step t e = fromMaybe t (RE.apply ctx e t)
      t' = foldl step st.table es
    liftEffect $ RStore.save t'
    H.modify_ _ { table = t' }

  -- A link in the chart opens the matrix that edits it, its column lit.
  ChartLink m to -> when (m `elem` [ "odonus", "balistes", "selene", "limulus" ]) do
    let
      grid = if m == "limulus" then Matrix.Drums else Matrix.gridOf m
      focus
        | Just p <- stripPrefix (Pattern "port:") to = Just ("midi:" <> p)
        | to == "fh2" = Just (if grid == Matrix.Drums then "fh2gate" else "fh2env")
        | to == "continuo" = Just "continuo"
        | to == "d-dirt" = Just "sample"
        | otherwise = Nothing
    handleAction (OpenMatrix grid focus)

  where
  sendRig line = do
    mrig <- H.gets _.rig
    for_ mrig \bin -> liftEffect $ Transport.send (Binnacle.socket bin) line
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
        [ flowChart st, harmonyPanel st ]
    , case st.matrix of
        Nothing -> HH.text ""
        Just g -> Matrix.view g
          { table: st.table, ports: st.ports, cards: cardChannels st.router, sampleSets: st.sampleSets
          , pick: st.pick, focus: st.focus, sheet: st.sheet
          , onEdits: Edits, onPick: PickCell, onSheet: OpenSheet, onAudition: Audition
          , onGrid: \g' -> OpenMatrix g' Nothing, onClose: CloseMatrix }
    ]

-- | The signal-flow chart: what the open pages drive, by the path the mode
-- | gives them. Conspicillum and Quadrat route themselves rather than through
-- | the table, so their routes are stated here.
flowChart :: forall m. State -> H.ComponentHTML Action () m
flowChart st =
  HH.section [ cls "flow", HP.attr (AttrName "aria-label") "Where it all goes" ]
    [ HH.div [ cls "flow-chart" ]
        [ FlowView.chart { hover: Hover, pick: ToggleVoices, link: ChartLink } st.hot { playing: map _.slot (filter (playing st) machines), rigUp: st.rigUp, tempo: st.tempo }
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
    -- Limulus: its Tidal streams to SuperDirt, and `drums $` down the drum
    -- lanes' own routing.
    , { machine: "limulus", dest: RM.DSample { set: "d1–d16", n: 0, begin: 0, end: 100, reverse: false, gain: 100, chop: 1 }, via: Nothing }
    ] <> map (\dest -> { machine: "limulus", dest, via: Nothing }) drumDests
  drumDests = nubEq do
    r <- st.table
    case r.source of
      RM.SDrumLane _ -> map _.dest (filter _.on r.legs)
      _ -> []

-- | The harmony matrix, under the chart: it is played, not set and forgotten,
-- | so it stays in view where the music is watched (docs/kb/plans/matrix-router.md).
harmonyPanel :: forall m. State -> H.ComponentHTML Action () m
harmonyPanel st =
  HH.div [ cls "harmony-panel" ]
    [ Router.view
        { toggle: RouterToggle
        , scalePattern: RouterEdit <<< Router.setScalePattern
        , scaleRoot: RouterEdit <<< Router.setScaleRoot
        , harmony: RouterEdit <<< Router.setHarmony
        , commit: RouterCommit
        , none: NoOp
        }
        st.rigUp st.router
    ]

topBar :: forall m. State -> H.ComponentHTML Action () m
topBar st =
  HH.header [ cls "top" ]
    [ HH.div [ cls "row" ]
        -- The brand is the way home (the landing); the tabs are the other views.
        ( [ HH.a
              [ cls "brand", HP.href "#", HP.title "Triggerfish: home", HP.attr (AttrName "aria-current") "page" ]
              [ HH.text "Triggerfish" ]
        , HH.nav [ cls "tabs", HP.attr (AttrName "aria-label") "Views" ]
            [ tab Matrix.Notes "#notes" "Routing: Notes"
            , tab Matrix.Drums "#drums" "Routing: Drums"
            , HH.a [ cls "tab", HP.href "/about.html", HP.target "atlantis-about" ] [ HH.text "About" ]
            ]
        , HH.span [ cls "spacer" ] []
        , HH.button [ cls "btn panic", HE.onClick \_ -> Panic ] [ HH.text "Panic" ]
        ]
        )
    , machineBar st
    ]
  where
  -- A plain link: the browser moves the hash and keeps history; the page follows
  -- through its hashchange listener.
  tab g href label =
    HH.a
      ( [ cls ("tab" <> if st.matrix == Just g then " on" else ""), HP.href href ]
          <> (if st.matrix == Just g then [ HP.attr (AttrName "aria-current") "page" ] else [])
      )
      [ HH.text label ]

lamp :: forall w i. Boolean -> String -> HH.HTML w i
lamp on label =
  HH.span [ cls ("lamp" <> if on then " live" else "") ] [ HH.i_ [], HH.text label ]

-- | The machines, as the app's navigation: each one's nameplate, with its fish
-- | as its play button (turned to face right, the way a play arrow points).
-- | A closed machine's name opens its page, behind the dashboard; an open
-- | one's name is only a name, since following a link into a tab that is
-- | already open would reload it.
machineBar :: forall m. State -> H.ComponentHTML Action () m
machineBar st =
  HH.div [ cls "mbar" ]
    ( [ HH.div [ cls "seg", HP.attr (AttrName "role") "group", HP.attr (AttrName "aria-label") "Mode" ]
          [ seg "Solo" Solo, seg "Atlantis" Atlantis ]
      , HH.button [ cls "btn", HE.onClick \_ -> StopAll, HP.disabled (not anyPlaying) ] [ HH.text "■ Stop all" ]
      , HH.nav [ cls "mitems", HP.attr (AttrName "aria-label") "Machines" ] (map item machines)
      , HH.span [ cls "spacer" ] []
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
    )
  where
  atlantisOnly xs = if st.mode == Atlantis then xs else []
  anyPlaying = not (null (filter (playing st) machines))
  seg label m =
    HH.button
      [ cls (if st.mode == m then "on" else "")
      , HP.attr (AttrName "aria-pressed") (if st.mode == m then "true" else "false")
      , HP.title case m of
          Solo -> "Solo: each page plays through MIDI on this computer."
          Atlantis -> "Atlantis: the rig plays; pages send it what to play."
      , HE.onClick \_ -> SetMode m
      ]
      [ HH.text label ]
  item m =
    HH.div [ cls ("mitem m-" <> m.slot <> state) ]
      ( [ fish, name ] <> chip )
    where
    open = isOpen st m
    isPlaying = playing st m
    state
      | isPlaying = " playing"
      | open = " open"
      | otherwise = " closed"
    fish
      | open && m.playable =
          HH.button
            [ cls "fishplay"
            , HP.title ((if isPlaying then "Stop " else "Play ") <> m.name)
            , HP.attr (AttrName "aria-label") ((if isPlaying then "Stop " else "Play ") <> m.name)
            , HP.attr (AttrName "aria-pressed") (if isPlaying then "true" else "false")
            , HE.onClick \_ -> Command m.slot (not isPlaying)
            ]
            [ Fish.icon "ico" m.slot ]
      | otherwise =
          HH.span
            [ cls "fishplay off"
            , HP.title (if isPlaying then m.name <> ": recording" else m.name)
            , HP.attr (AttrName "aria-hidden") "true"
            ]
            [ Fish.icon "ico" m.slot ]
    name
      | open = HH.span [ cls ("wordmark w-" <> m.slot) ] [ HH.text m.name ]
      | otherwise =
          HH.a
            [ cls ("wordmark w-" <> m.slot), HP.href m.href, HP.title ("Open " <> m.name <> " in a new tab")
            , HE.onClick (OpenMachine m)
            ]
            [ HH.text m.name ]
    heard = Map.lookup m.slot st.heard
    chip = case heard >>= _.state.alias of
      Just alias | open ->
        [ HH.span [ cls ("alias" <> if edited then " edited" else ""), HP.title "the loaded preset" ]
            [ chipIcons (Just { glyph: G.glyphFromAlias alias, diverged: edited }), HH.text alias ]
        ]
      _ -> []
    edited = fromMaybe false (map _.state.edited heard)

-- | The whole table, one ledger, each source drawn by the same rows as every
-- | other router.
cls :: forall r i. String -> HP.IProp (class :: String | r) i
cls = HP.class_ <<< H.ClassName

rigUrl :: String
rigUrl = "ws://127.0.0.1:3012/ws"

-- | Vetula's cards' channels, from the stage's cards the router reads.
cardChannels :: Router.Router -> Array Int
cardChannels r = Array.sort (Array.nub (Array.mapMaybe channel (Array.fromFoldable (Map.values r.cards))))
  where
  channel line = Array.head (String.split (Pattern " ") (String.trim line)) >>= stripPrefix (Pattern "ch") >>= Int.fromString
