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
import Data.Number as Number
import Data.Number.Format (fixed, toStringWith)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Timer (setInterval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Triggerfish.Fish as Fish
import Web.Event.Event (preventDefault)
import Halogen.Query.Event (eventListener)
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Triggerfish.Flow as Flow
import Triggerfish.Bosun as Bosun
import Triggerfish.BackgroundOpen as BackgroundOpen
import Triggerfish.Dashboard.Atlantis as Atlantis
import Triggerfish.Capture.RigLoops as RigLoops
import Data.Tuple (Tuple(..))
import Triggerfish.Flow.View as FlowView
import Triggerfish.Route as Route
import Triggerfish.Router as Router
import Triggerfish.Routing.Matrix as Matrix
import Reef.Route as HarmonyRoute
import Triggerfish.Routing.Edit as RE
import Triggerfish.Routing.Model as RM
import Triggerfish.Routing.Out as RO
import Triggerfish.Routing.VetulaSync as VetulaSync
import Reef.Routing as RR
import Triggerfish.Routing.Store as RStore
import Triggerfish.SampleSets (SampleSet)
import Triggerfish.SampleSets as SampleSets
import Binnacle.TabBus as Bus
import Triggerfish.Transport (Mode(..))
import Triggerfish.Transport.Store as TransportStore
import Triggerfish.Tempo as Tempo

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
  -- the free-run tempo (Triggerfish.Tempo), what the pages keep off the rig
  , freeTempo :: Number
  , bus :: Maybe Bus.Bus
  , table :: RM.Table
  , ports :: Array String
  , sampleSets :: Array SampleSet
  , hot :: Maybe String      -- the machine hovered on the chart
  , router :: Router.Router  -- the harmony routes, as the rig's stage holds them
  -- the notes or drums matrix, when open, its picked cell and lit column
  , matrix :: Maybe Matrix.Grid
  , pick :: Maybe Matrix.Pick
  , focus :: Maybe String
  , sheet :: Maybe String
  , foldDrums :: Boolean
  -- the Vetula routing last written to the stage (`vetula/routing`)
  , vetulaSent :: Maybe String
  -- the chart's switches: Diaphus drawn on the rig's MIDI path, and every
  -- machine opened into its voices
  , relays :: Boolean
  , allVoices :: Boolean
  -- the marks the rig keeps, per machine (`loops` frames), for the bubbles
  , rigLoops :: Map String (Array RigLoops.RigMark)
  -- the Atlantis group as Bosun last said, Nothing while out of reach
  , bosun :: Maybe Bosun.Health
  -- the Atlantis page (`#atlantis`): open, its restarts asked, and whether
  -- lowering the rig is waiting on a second press
  , atlantis :: Boolean
  , asked :: Array Atlantis.Asked
  , confirmingDown :: Boolean
  -- the key as a filter: kinds of line hidden, and the kind hovered
  , hidden :: Array String
  , keyHot :: Maybe String
  -- the harmony port clicked first, waiting for its other end
  , armed :: Maybe String
  -- the closed machine whose 'open ↗' a plain click has shown
  , peeked :: Maybe String
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
  | RigOpen
  | RigFrame String
  | NoOp
  | OpenMatrix Matrix.Grid (Maybe String)
  | CloseMatrix
  | PickCell (Maybe Matrix.Pick)
  | OpenSheet (Maybe String)
  | FoldDrums Boolean
  | SetTempo String
  | BumpTempo Tempo.Bump
  | TempoStored
  | KeyDown KeyboardEvent
  | Audition RM.Destination
  | Edits (Array RE.Edit)
  | ChartLink String String
  | ShowRelays Boolean
  | BosunPoll
  | RigRestart String
  | RigGroup String
  | ConfirmDown Boolean
  | KeyToggle String
  | KeyHover (Maybe String)
  | KeyAll
  | PortClick String
  | Peek String
  | CableClick String String
  | ShowAllVoices Boolean

component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ ->
      { mode: Solo, now: 0.0, heard: Map.empty, rig: Nothing, rigUp: false
      , tempo: 120.0, freeTempo: 120.0, locked: false, bus: Nothing
      , table: RM.defaultTable, ports: [], sampleSets: [], hot: Nothing, router: Router.initial
      , matrix: Nothing, pick: Nothing, focus: Nothing, sheet: Nothing, foldDrums: true, vetulaSent: Nothing
      , relays: false, allVoices: false, rigLoops: Map.empty, bosun: Nothing
      , atlantis: false, asked: [], confirmingDown: false, hidden: [], keyHot: Nothing, armed: Nothing, peeked: Nothing }
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
    free <- liftEffect $ fromMaybe 120.0 <$> Tempo.load
    rig <- liftEffect $ Binnacle.connect { url: rigUrl, tempo: free }
    H.modify_ _ { rig = Just rig, freeTempo = free, tempo = free }
    liftEffect $ Tempo.onChange (HS.notify listener TempoStored)
    win <- liftEffect window
    void $ H.subscribe $ eventListener KET.keydown (Window.toEventTarget win) (map KeyDown <<< KE.fromEvent)
    -- the stage's text objects: the harmony routes, and Vetula's cards for
    -- its voices; asked for again whenever the socket (re)opens
    -- a plain click on a ghost fish stays here; cmd-click opens it behind
    liftEffect BackgroundOpen.install
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
    _ <- liftEffect $ setInterval 3000 (HS.notify listener BosunPoll)
    handleAction BosunPoll

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
      -- Link's tempo once anchored; until then the free run the pages keep
      tempo = maybe st.freeTempo (\r -> if r.locked then r.tempo else st.freeTempo) reading
      locked = maybe false _.locked reading
      openBefore = map _.slot (filter (isOpen st) machines)
      openAfter = map _.slot (filter (isOpen st { now = now }) machines)
    when (up /= st.rigUp || tempo /= st.tempo || locked /= st.locked || openBefore /= openAfter || st.now == 0.0)
      (H.modify_ _ { now = now, rigUp = up, tempo = tempo, locked = locked })
    -- the rig's marks are the rig's: gone with it, sent again when it returns
    when (st.rigUp && not up) (H.modify_ _ { rigLoops = Map.empty })

  -- The tempo, typed here: for every page and, with the rig up, for Link.
  SetTempo v -> for_ (Number.fromString v) setTempo
  BumpTempo d -> do
    st <- H.get
    setTempo (st.tempo + d)
  TempoStored -> do
    mfree <- liftEffect Tempo.load
    for_ mfree \free -> H.modify_ \s -> s { freeTempo = free, tempo = if s.locked then s.tempo else free }
  KeyDown ke | KE.key ke == "Escape" -> H.modify_ _ { armed = Nothing, peeked = Nothing }
  KeyDown ke -> for_ (Tempo.hotkey ke) \d -> do
    liftEffect $ preventDefault (KE.toEvent ke)
    handleAction (BumpTempo d)

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
  -- Atlantis asks for the rig: a held group is raised. Solo leaves the rig
  -- as it is; lowering it is the Atlantis page's, and deliberate.
  SetMode m -> do
    H.modify_ _ { mode = m }
    liftEffect $ TransportStore.save m
    st <- H.get
    when (m == Atlantis && maybe false (\h -> h.desired /= "up") st.bosun)
      (handleAction (RigGroup "up"))

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
    syncCards

  -- At first run, with nothing stored, the default table is made for the ports
  -- this machine has and saved, as every other page with a router does.
  SetPorts ns -> do
    H.modify_ _ { ports = ns }
    stored <- liftEffect RStore.load
    when (isNothing stored) do
      let t = RM.defaultTableFor ns
      liftEffect $ RStore.save t
      H.modify_ _ { table = t }
    syncCards

  FromHash h -> do
    H.modify_ _ { atlantis = h == "atlantis", confirmingDown = false }
    case matrixOfHash h of
      Just g -> handleAction (OpenMatrix g Nothing)
      Nothing -> H.modify_ _ { matrix = Nothing, focus = Nothing, pick = Nothing }

  Hover m -> H.modify_ _ { hot = m }

  ShowRelays b -> H.modify_ _ { relays = b }

  -- Every three seconds: which of the group's daemons are up.
  BosunPoll -> void $ H.fork do
    h <- liftAff Bosun.state
    now <- liftEffect dateNow
    st <- H.get
    when (h /= st.bosun) (H.modify_ _ { bosun = h })
    -- a restart's outcome is judged against the clock
    unless (null st.asked) (H.modify_ _ { now = now })

  -- Restart one daemon. What happened is read from /state, not the reply.
  RigRestart id -> do
    now <- liftEffect dateNow
    st <- H.get
    let before = find (\sv -> sv.id == id) (maybe [] _.services st.bosun)
    H.modify_ _ { now = now, asked = [ { service: id, at: now, before } ] <> filter (\a -> a.service /= id) st.asked }
    void $ H.fork do
      _ <- liftAff (Bosun.control "restart" id)
      handleAction BosunPoll

  RigGroup verb -> do
    H.modify_ _ { confirmingDown = false }
    void $ H.fork do
      _ <- liftAff (Bosun.control verb "")
      handleAction BosunPoll

  ConfirmDown b -> H.modify_ _ { confirmingDown = b }

  KeyToggle k -> H.modify_ \x -> x { hidden = if k `elem` x.hidden then filter (_ /= k) x.hidden else x.hidden <> [ k ] }
  KeyHover k -> H.modify_ _ { keyHot = k }
  KeyAll -> H.modify_ _ { hidden = [] }

  -- A port, then its other end: a source and an input make a cable (or
  -- unplug the one they already make). The rig owns the table: this sends
  -- the change, as the matrix does, and draws what the stage says back.
  PortClick id -> do
    st <- H.get
    case st.armed of
      Nothing -> H.modify_ _ { armed = Just id }
      Just a
        | a == id -> H.modify_ _ { armed = Nothing }
        | Just pair <- patchPair a id -> do
            H.modify_ _ { armed = Nothing }
            for_ (Router.toggle pair.line pair.input st.router) sendRig
        | otherwise -> H.modify_ _ { armed = Just id }

  Peek slot -> H.modify_ \x -> x { peeked = if x.peeked == Just slot then Nothing else Just slot }

  CableClick source input -> do
    st <- H.get
    H.modify_ _ { armed = Nothing }
    for_ (patchPair source ("in:" <> input)) \pair ->
      for_ (Router.toggle pair.line pair.input st.router) sendRig

  ShowAllVoices b -> H.modify_ _ { allVoices = b }


  RigOpen -> do
    H.modify_ _ { vetulaSent = Nothing }
    sendRig Router.subscribeLine
    sendRig RigLoops.syncLine

  RigFrame msg -> do
    for_ loopMachines \m -> do
      for_ (RigLoops.readLoops m msg) \marks -> H.modify_ \x -> x { rigLoops = Map.insert m marks x.rigLoops }
      when (RigLoops.readClear m msg) (H.modify_ \x -> x { rigLoops = Map.delete m x.rigLoops })
    st <- H.get
    for_ (Router.readFrame msg st.router) \r -> do
      H.modify_ _ { router = r }
      syncCards

  NoOp -> pure unit

  OpenMatrix g focus -> H.modify_ _ { matrix = Just g, focus = focus, pick = Nothing, sheet = Nothing }

  CloseMatrix -> do
    H.modify_ _ { matrix = Nothing, focus = Nothing, pick = Nothing, sheet = Nothing }
    liftEffect (Route.writeHash "")

  PickCell p -> H.modify_ _ { pick = p, sheet = Nothing }

  OpenSheet k -> H.modify_ _ { sheet = k, pick = Nothing }
  FoldDrums b -> H.modify_ _ { foldDrums = b, pick = Nothing }

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
    syncCards

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
        ( if st.atlantis then
            [ Atlantis.view { restart: RigRestart, group: RigGroup, confirmDown: ConfirmDown }
                { now: st.now, health: st.bosun, asked: st.asked, confirmingDown: st.confirmingDown }
            ]
          else [ flowChart st ]
        )
    , case st.matrix of
        Nothing -> HH.text ""
        Just g -> Matrix.view g
          { table: st.table, ports: st.ports, cards: VetulaSync.cardChannels st.router, sampleSets: st.sampleSets
          , pick: st.pick, focus: st.focus, sheet: st.sheet, fold: st.foldDrums
          , onEdits: Edits, onPick: PickCell, onSheet: OpenSheet, onAudition: Audition
          , onGrid: \g' -> OpenMatrix g' Nothing, onFold: FoldDrums, onClose: CloseMatrix }
    ]

-- | The signal-flow chart: what the open pages drive, by the path the mode
-- | gives them. Conspicillum and Quadrat route themselves rather than through
-- | the table, so their routes are stated here.
-- | An input's word on the chart.
inputWord :: HarmonyRoute.Input -> String
inputWord = case _ of
  HarmonyRoute.OdonusGrid -> "grid"
  HarmonyRoute.OdonusOut -> "out"

-- | Two ports clicked, as a matrix cell: one a source (`src:…`), the other
-- | an input (`in:…`), in either order.
patchPair :: String -> String -> Maybe { line :: Router.Line, input :: HarmonyRoute.Input }
patchPair a b = case lineOf a, inputOf b, lineOf b, inputOf a of
  Just line, Just input, _, _ -> Just { line, input }
  _, _, Just line, Just input -> Just { line, input }
  _, _, _, _ -> Nothing
  where
  lineOf id = case String.stripPrefix (Pattern "src:") id of
    Just "key" -> Just Router.RKey
    Just "scale" -> Just Router.RScale
    Just "harmony" -> Just Router.RHarmony
    Just v -> String.stripPrefix (Pattern "v") v >>= Int.fromString <#> Router.RVoice
    Nothing -> Nothing
  inputOf = case _ of
    "in:grid" -> Just HarmonyRoute.OdonusGrid
    "in:out" -> Just HarmonyRoute.OdonusOut
    _ -> Nothing

-- | The machines whose marks the rig keeps.
loopMachines :: Array String
loopMachines = [ "odonus", "vetula" ]

flowChart :: forall m. State -> H.ComponentHTML Action () m
flowChart st =
  HH.section [ cls "flow", HP.attr (AttrName "aria-label") "Where it all goes" ]
    [ HH.div [ cls ("flow-chart" <> if st.allVoices then " all-voices" else "") ]
        [ FlowView.chart { hover: Hover, link: ChartLink, port: PortClick, cable: CableClick, play: Command, peek: Peek } st.hot { playing: map _.slot (filter (playing st) machines), rigUp: st.rigUp, tempo: st.tempo, lamps, hidden: st.hidden, keyHot: st.keyHot, patch, dock, locked: st.locked, peeked: st.peeked }
            ( Flow.flow
                { mode: st.mode
                , table: st.table
                , ports: { found: st.ports, rigUp: st.rigUp }
                , machines: map _.slot (filter (isOpen st) machines)
                , open: if st.allVoices then map _.slot machines else []
                , extras
                , relays: st.relays
                , loops: rigLoops
                , rigUp: st.rigUp
                , down: downNodes
                , quantise
                }
            )
        ]
    , HH.div [ cls "flow-foot" ]
        [ FlowView.key { toggle: KeyToggle, hover: KeyHover, all: KeyAll } { hidden: st.hidden, keyHot: st.keyHot }
        , HH.div [ cls "flow-switches" ]
            [ switch "Relays" "Draw Diaphus, which delivers every MIDI note the rig sends." st.relays ShowRelays
            , switch "Every voice" "Open every machine into its voices: one line per channel, head or lane." st.allVoices ShowAllVoices
            ]
        ]
    ]
  where
  -- Every machine, for the chart's dock and its fish.
  dock = machines <#> \m ->
    let heard = Map.lookup m.slot st.heard
    in
      { slot: m.slot, name: m.name, open: isOpen st m, playing: playing st m, playable: m.playable
      , alias: if isOpen st m then heard >>= _.state.alias else Nothing
      , href: m.href, target: m.target
      }
  -- The patch bay: a port for every row of the harmony matrix, the cables
  -- its routes make, and the port clicked first.
  patch =
    { sources: Router.rows st.router <#> \row ->
        let
          allowed = map inputWord (filter (Router.allowed row) [ HarmonyRoute.OdonusGrid, HarmonyRoute.OdonusOut ])
        in case row of
          Router.RKey -> { id: "src:key", short: "K", label: "key" <> maybe "" (\k -> " " <> k) st.router.vetulaKey, machine: Just "vetula", allowed }
          Router.RVoice v -> { id: "src:v" <> show v, short: show v, label: "voice " <> show v, machine: Just "vetula", allowed }
          Router.RScale -> { id: "src:scale", short: "S", label: "scale " <> st.router.scalePattern, machine: Nothing, allowed }
          Router.RHarmony -> { id: "src:harmony", short: "H", label: "harmony " <> st.router.harmony, machine: Nothing, allowed }
    , routes: st.router.routes <#> \rt ->
        { input: inputWord rt.input
        , source: case rt.source of
            HarmonyRoute.VetulaKey -> "src:key"
            HarmonyRoute.VetulaVoice v -> "src:v" <> show v
            HarmonyRoute.Scale _ -> "src:scale"
            HarmonyRoute.Harmony _ -> "src:harmony"
        }
    , armed: st.armed
    }
  -- The harmony routes as quantisation: what feeds Odonus's grid and out.
  quantise = st.router.routes <#> \rt ->
    { target: "odonus"
    , input: case rt.input of
        HarmonyRoute.OdonusGrid -> "grid"
        HarmonyRoute.OdonusOut -> "out"
    , machine: case rt.source of
        HarmonyRoute.VetulaKey -> Just "vetula"
        HarmonyRoute.VetulaVoice _ -> Just "vetula"
        _ -> Nothing
    , label: case rt.source of
        HarmonyRoute.VetulaKey -> "key" <> maybe "" (\k -> " " <> k) st.router.vetulaKey
        HarmonyRoute.VetulaVoice v -> "voice " <> show v
        HarmonyRoute.Scale sc -> "scale " <> sc.pattern
        HarmonyRoute.Harmony h -> "harmony " <> h
    }
  services = maybe [] _.services st.bosun
  lamps = services # Array.mapMaybe \sv -> Bosun.nodeOf sv.id <#> \node ->
    { node, lamp: Bosun.lampOf sv
    , title: sv.id <> ": " <> sv.state <> (if sv.gaveUp then ", gave up" else "")
        <> (if sv.restarts > 0 then " · " <> show sv.restarts <> " restarts" else "") }
  downNodes = services # Array.mapMaybe \sv ->
    if Bosun.lampOf sv == Bosun.Down && Bosun.breaksStreams sv.id then Bosun.nodeOf sv.id else Nothing
  switch label tip on act =
    HH.label [ cls "flow-switch", HP.title tip ]
      [ HH.input [ HP.type_ HP.InputCheckbox, HP.checked on, HE.onChecked act ], HH.text label ]
  rigLoops = do
    Tuple m marks <- Map.toUnfoldable st.rigLoops
    mk <- Array.reverse marks
    pure { machine: m, n: mk.n, playing: mk.playing }
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

topBar :: forall m. State -> H.ComponentHTML Action () m
topBar st =
  HH.header [ cls "top" ]
    [ HH.div [ cls "row" ]
        -- The brand is the way home (the landing); the tabs are the other views.
        ( [ HH.a
              [ cls "brand", HP.href "#", HP.title "Triggerfish: home", HP.attr (AttrName "aria-current") "page" ]
              [ HH.text "Triggerfish" ]
        , HH.nav [ cls "tabs", HP.attr (AttrName "aria-label") "Views" ]
            ( [ tab Matrix.Notes "#notes" "Routing: Notes"
              , tab Matrix.Drums "#drums" "Routing: Drums"
              ]
                -- the rig's daemons: a tab in Atlantis, where they are needed
                <> (if st.mode == Atlantis || st.atlantis then [ atlantisTab ] else [])
                <> [ HH.a [ cls "tab", HP.href "/about.html", HP.target "atlantis-about" ] [ HH.text "About" ] ]
            )
        , HH.span [ cls "spacer" ] []
        , tempoControl st
        , rigLamp st
        , lamp st.locked (if st.locked then "Link" else "free-running")
        , HH.button [ cls "btn panic", HE.onClick \_ -> Panic ] [ HH.text "Panic" ]
        ]
        )
    , machineBar st
    ]
  where
  -- A plain link: the browser moves the hash and keeps history; the page follows
  -- through its hashchange listener.
  atlantisTab =
    HH.a
      ( [ cls ("tab" <> if st.atlantis then " on" else ""), HP.href "#atlantis" ]
          <> (if st.atlantis then [ HP.attr (AttrName "aria-current") "page" ] else [])
      )
      [ HH.text "Atlantis" ]
  tab g href label =
    HH.a
      ( [ cls ("tab" <> if st.matrix == Just g then " on" else ""), HP.href href ]
          <> (if st.matrix == Just g then [ HP.attr (AttrName "aria-current") "page" ] else [])
      )
      [ HH.text label ]

-- | The one tempo control (Triggerfish.Tempo): Link's tempo once the rig is
-- | up, else the free run every page keeps. The hotkeys work on every page,
-- | this one included.
tempoControl :: forall m. State -> H.ComponentHTML Action () m
tempoControl st =
  HH.span [ cls "tempo", HP.title Tempo.hotkeyHelp ]
    [ HH.button [ cls "step", HE.onClick \_ -> BumpTempo (-1.0), HP.title "tempo −1 (⌥−; with ⇧, −5)" ] [ HH.text "−" ]
    , HH.input
        [ cls "num", HP.type_ HP.InputNumber, HP.attr (AttrName "step") "1"
        , HP.attr (AttrName "min") "20", HP.attr (AttrName "max") "300"
        , HP.value (Tempo.showTempo st.tempo)
        , HE.onValueChange SetTempo
        , HP.attr (AttrName "aria-label") "Tempo, beats a minute"
        ]
    , HH.button [ cls "step", HE.onClick \_ -> BumpTempo 1.0, HP.title "tempo +1 (⌥=; with ⇧, +5)" ] [ HH.text "+" ]
    , HH.text " bpm"
    ]

-- | The rig's trunk, Architeuthis and Diaphus: every rig path needs both. Lit
-- | when this page reaches the rig and Bosun has both running; the title says
-- | which is missing.
rigLamp :: forall w i. State -> HH.HTML w i
rigLamp st =
  HH.span [ cls ("lamp" <> if up then " live" else ""), HP.title tip ] [ HH.i_ [], HH.text (if up then "rig" else "no rig") ]
  where
  trunk = [ "architeuthis", "diaphus" ]
  services = maybe [] _.services st.bosun
  missing = filter (\id -> not (Array.any (\sv -> sv.id == id && Bosun.lampOf sv == Bosun.Up) services)) trunk
  up = st.rigUp && (isNothing st.bosun || null missing)
  tip
    | not st.rigUp = "This page does not reach Architeuthis (:3012)."
    | isNothing st.bosun = "Architeuthis answers. Bosun (:3994) is out of reach, so its daemons are not shown."
    | null missing = "Architeuthis and Diaphus are running."
    | otherwise = "Not running under Bosun: " <> String.joinWith ", " missing

lamp :: forall w i. Boolean -> String -> HH.HTML w i
lamp on label =
  HH.span [ cls ("lamp" <> if on then " live" else "") ] [ HH.i_ [], HH.text label ]

-- | Set the tempo for every page, and for Link if the rig is up; shown at
-- | once rather than on the next tick.
setTempo :: forall o m. MonadAff m => Number -> H.HalogenM State Action () o m Unit
setTempo n = do
  st <- H.get
  let bpm = Tempo.clampTempo n
  for_ st.rig \bin -> liftEffect (Tempo.set bin bpm)
  H.modify_ _ { tempo = bpm, freeTempo = bpm }

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
      -- The machines themselves are on the chart: flowing, or in its dock.
      ]
    )
  where
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

-- | The whole table, one ledger, each source drawn by the same rows as every
-- | other router.
cls :: forall r i. String -> HP.IProp (class :: String | r) i
cls = HP.class_ <<< H.ClassName

rigUrl :: String
rigUrl = "ws://127.0.0.1:3012/ws"

-- | Vetula's cards' channels, from the stage's cards the router reads.
-- | Vetula's cards in the routing table: a row for each card channel that has
-- | none (its default, saved), and the rig told where each channel plays, as
-- | `vetula/routing` on the stage, which its card player reads. Only once the
-- | ports are known (a leg resolves to a port by name), and only on a change.
syncCards :: forall o. M o Unit
syncCards = do
  st <- H.get
  unless (Array.null st.ports) do
    let s = VetulaSync.sync st.ports st.router st.table
    when (s.table /= st.table) do
      liftEffect $ RStore.save s.table
      H.modify_ _ { table = s.table }
    when (st.vetulaSent /= Just s.json) $ for_ st.rig \bin -> do
      liftEffect $ Transport.send (Binnacle.socket bin) (VetulaSync.stageLine s.json)
      H.modify_ _ { vetulaSent = Just s.json }
