-- | One Triggerfish machine on a page of its own: the smallest shell that can
-- | carry it. Balistes (`balistes.html`) and the Selene rack (`selene.html`) run
-- | in it; see `docs/kb/plans/the-offering.md`.
-- |
-- | The machine is the same component the Triggerfish page used to mount, and
-- | this shell does for it what Triggerfish's did:
-- |
-- |   * the transport: Solo or Atlantis, and play/stop, pushed down as the one
-- |     derived `Sounding` (`Triggerfish.Transport.soundingOf`, so a machine with
-- |     no rig voice, like Selene, stays Local in both modes);
-- |   * the routing table, for a machine that reads it: loaded from the store,
-- |     edited in this page's own router (its own sources only; ⌥1, as in
-- |     Triggerfish), saved, and pushed down. The pages share one origin, so one
-- |     store, and each follows the others' edits live (`Routing.Store.onChange`);
-- |   * the free-run clock baseline and tempo (Solo; Link overrides it on the rig);
-- |   * the preset chip, and the CAPTURE key (`c`);
-- |   * the machine's stage slot: its chip and whether it sounds, recorded on
-- |     the rig whenever either changes (`Triggerfish.Stage`), for the dashboard;
-- |   * the tab bus (`Triggerfish.TabBus`): the same state announced to the
-- |     dashboard tab to tab, and its play, stop and Panic obeyed. The mode
-- |     follows the dashboard's switch through the store.
-- |
-- | What spans machines (scenes, macro lanes, the library manager) belongs to the
-- | dashboard to come, not here.
-- |
-- | The machine's panel is `position:fixed; top:var(--tf-bar)`, so the page must
-- | set `--tf-bar` (the HTML does, 44px, as index.html does) and the bar is pinned
-- | inside it. A bar in the flow sits UNDER the panel: drawn, but unclickable.
module Triggerfish.Standalone
  ( Config
  , Router
  , run
  ) where

import Prelude

import Binnacle as Binnacle
import Binnacle.Audio (armAudioKeepAlive)
import Binnacle.Midi as Midi
import Binnacle.Time (dateNow)
import Binnacle.Transport as Transport
import Data.Foldable (for_)
import Data.Int as Int
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.Set as Set
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Timer (setInterval)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Triggerfish.Glyph (ChipView)
import Triggerfish.GlyphView (chipIcons)
import Triggerfish.Routing.Edit as RE
import Triggerfish.Routing.Model as RM
import Triggerfish.Routing.Out as RO
import Triggerfish.Routing.Store as RStore
import Triggerfish.Routing.View as RV
import Triggerfish.SampleSets (SampleSet)
import Triggerfish.SampleSets as SampleSets
import Triggerfish.SourceQuery as SQ
import Triggerfish.Stage as Stage
import Triggerfish.TabBus as Bus
import Triggerfish.Transport (Mode(..), Sounding(..), Which, soundingOf)
import Triggerfish.Transport.Store as TransportStore
import Triggerfish.Ui.Style (engrave, style)
import Type.Proxy (Proxy(..))
import Web.Event.Event as E
import Web.HTML (window)
import Web.HTML.HTMLInputElement as HInput
import Web.HTML.HTMLTextAreaElement as HTextArea
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

-- | One machine's page.
-- |
-- | - `which`: the machine, for the transport's rules;
-- | - `nameplate`: the engraved name at the left of the bar;
-- | - `chipOf`: the preset chip, when an output carries one;
-- | - `router`: the sources this page routes, or `Nothing` for a machine that
-- |   does not read the routing table.
type Config o =
  { which :: Which
  , nameplate :: String
  , component :: H.Component SQ.Query Unit o Aff
  , chipOf :: o -> Maybe (Maybe ChipView)
  , router :: Maybe Router
  }

-- | A page's router: its title, the sources it shows, and what they are called
-- | in the restore link.
type Router =
  { title :: String
  , note :: String
  , sources :: Array RM.Source
  , restoreLabel :: String
  }

run :: forall o. Config o -> Effect Unit
run cfg = HA.runHalogenAff do
  liftEffect armAudioKeepAlive
  body <- HA.awaitBody
  void $ runUI (root cfg) unit body

type State =
  { mode :: Mode
  , playing :: Boolean
  , bpm :: Int
  , freeT0 :: Number
  , chip :: Maybe ChipView
  , rig :: Maybe Binnacle.Binnacle
  , rigUp :: Boolean
  , table :: RM.Table
  , ports :: Array String
  , sampleSets :: Array SampleSet
  , routerOpen :: Boolean
  -- The last stage-put sent, so an unchanged slot sends nothing; cleared when
  -- the rig goes away, so a reconnect records it again.
  , staged :: Maybe String
  , bus :: Maybe Bus.Bus
  }

data Action o
  = Init
  | SetMode Mode
  | TogglePlay
  | SetBpm String
  | Capture
  | Panic
  | RoutingChanged
  | FromMachine o
  | Key E.Event
  | ToggleRouter
  | Edit RE.Edit
  | Audition RM.Destination
  | ResetRouting
  | SetPorts (Array String)
  | Tick
  | FromBus Bus.Msg
  | ModeStored

type Slots o = (machine :: H.Slot SQ.Query o Unit)

_machine :: Proxy "machine"
_machine = Proxy

root :: forall q i o' o. Config o -> H.Component q i o' Aff
root cfg = H.mkComponent
  { initialState: \_ ->
      { mode: Solo, playing: false, bpm: 120, freeT0: 0.0, chip: Nothing, rig: Nothing
      , rigUp: false, table: RM.defaultTable, ports: [], sampleSets: [], routerOpen: false
      , staged: Nothing, bus: Nothing }
  , render: render cfg
  , eval: H.mkEval H.defaultEval { handleAction = handleAction cfg, initialize = Just Init }
  }

type M o o' = H.HalogenM State (Action o) (Slots o) o' Aff

handleAction :: forall o o'. Config o -> Action o -> M o o' Unit
handleAction cfg = case _ of
  Init -> do
    now <- liftEffect dateNow
    H.modify_ _ { freeT0 = now * 1000.0 }
    -- The page's own rig socket, for PANIC, as in the Triggerfish shell.
    rig <- liftEffect $ Binnacle.connect { url: rigUrl, tempo: 120.0 }
    H.modify_ _ { rig = Just rig }
    mmode <- liftEffect TransportStore.load
    for_ mmode \m -> H.modify_ _ { mode = m }
    { emitter, listener } <- liftEffect HS.create
    _ <- H.subscribe emitter
    liftEffect $ RStore.onChange (HS.notify listener RoutingChanged)
    liftEffect $ TransportStore.onChange (HS.notify listener ModeStored)
    bus <- liftEffect Bus.open
    H.modify_ _ { bus = Just bus }
    liftEffect $ Bus.onMessage bus (HS.notify listener <<< FromBus)
    -- The port names, for the router's reach column and its port menus. The
    -- machine asks for MIDI itself to play; this is only to know what exists.
    for_ cfg.router \_ -> do
      liftEffect $ Midi.requestAccess case _ of
        Just access -> Midi.outputNames access >>= HS.notify listener <<< SetPorts
        Nothing -> HS.notify listener (SetPorts [])
      void $ H.fork do
        sets <- liftAff SampleSets.load
        H.modify_ _ { sampleSets = sets }
    _ <- liftEffect $ setInterval 1500 (HS.notify listener Tick)
    target <- liftEffect $ Window.toEventTarget <$> window
    _ <- H.subscribe $ eventListener KET.keydown target (Just <<< Key)
    handleAction cfg RoutingChanged
    pushFree
    pushSounding cfg
  SetMode m -> do
    H.modify_ _ { mode = m }
    liftEffect $ TransportStore.save m
    pushSounding cfg
  TogglePlay -> do
    H.modify_ \s -> s { playing = not s.playing }
    pushSounding cfg
  SetBpm v -> for_ (Int.fromString v) \n -> do
    H.modify_ _ { bpm = clamp 20 999 n }
    pushFree
  Capture -> void $ H.query _machine unit (SQ.Capture unit)
  Panic -> do
    st <- H.get
    for_ st.rig \bin -> liftEffect $ Transport.send (Binnacle.socket bin) "hush"
    H.modify_ _ { playing = false }
    pushSounding cfg
  RoutingChanged -> do
    mtbl <- liftEffect RStore.load
    for_ mtbl \t -> do
      H.modify_ _ { table = t }
      void $ H.query _machine unit (SQ.SetRouting t unit)
  -- An edit here is saved first, so the store, the machine and every other open
  -- page (through its storage event) agree on the next note.
  Edit e -> do
    st <- H.get
    for_ (RE.apply { ports: st.ports, sampleSets: st.sampleSets } e st.table) keepTable
  ResetRouting -> for_ cfg.router \r -> do
    st <- H.get
    keepTable (RE.resetSources st.ports r.sources st.table)
  Audition dest -> do
    st <- H.get
    for_ (RO.auditionLine dest) \line ->
      for_ st.rig \bin -> liftEffect $ Transport.send (Binnacle.socket bin) line
  ToggleRouter -> for_ cfg.router \_ -> H.modify_ \s -> s { routerOpen = not s.routerOpen }
  SetPorts ns -> do
    H.modify_ _ { ports = ns }
    firstRun ns
  -- The rig link, polled as the Triggerfish shell polls it: what a rig-only leg
  -- can reach depends on it.
  Tick -> do
    st <- H.get
    ok <- case st.rig of
      Nothing -> pure false
      Just bin -> liftEffect $ Transport.isConnected (Binnacle.socket bin)
    when (ok /= st.rigUp) (H.modify_ _ { rigUp = ok, staged = Nothing })
    publishStage cfg
    announce cfg
  -- The dashboard's commands, for this machine or for all.
  FromBus msg -> do
    st <- H.get
    let mine m = Stage.slotOf cfg.which == Just m
    case msg of
      Bus.Play m | mine m && not st.playing -> handleAction cfg TogglePlay
      Bus.Stop m | mine m && st.playing -> handleAction cfg TogglePlay
      -- The dashboard hushes the rig itself; here only the local transport stops.
      Bus.Panic -> do
        H.modify_ _ { playing = false }
        pushSounding cfg
      Bus.Hello -> announce cfg
      _ -> pure unit
  -- Another tab (the dashboard) changed the mode.
  ModeStored -> do
    mmode <- liftEffect TransportStore.load
    st <- H.get
    for_ mmode \m -> when (m /= st.mode) do
      H.modify_ _ { mode = m }
      pushSounding cfg
  -- Only the chip is this shell's business; a machine's other outputs (Balistes'
  -- macro-lane edits) belong to the dashboard.
  FromMachine out -> for_ (cfg.chipOf out) \cv -> do
    H.modify_ _ { chip = cv }
    publishStage cfg
    announce cfg
  Key e -> for_ (KE.fromEvent e) \ke -> unless (targetIsField e || KE.metaKey ke || KE.ctrlKey ke) do
    -- ⌥1 by the key's position, as in Triggerfish: on a Mac, Option+1 types "¡".
    if KE.altKey ke then
      when (KE.code ke == "Digit1") do
        liftEffect $ E.preventDefault e
        handleAction cfg ToggleRouter
    else case KE.key ke of
      "c" -> handleAction cfg Capture
      " " -> do
        liftEffect $ E.preventDefault e
        handleAction cfg TogglePlay
      _ -> pure unit

-- | At first run, with nothing stored, the default table is made for the ports
-- | this machine has and saved, so the choice is made once and shown in the
-- | router rather than left to whichever port happens to come first later.
firstRun :: forall o o'. Array String -> M o o' Unit
firstRun ports = do
  stored <- liftEffect RStore.load
  when (isNothing stored) (keepTable (RM.defaultTableFor ports))

keepTable :: forall o o'. RM.Table -> M o o' Unit
keepTable t = do
  liftEffect $ RStore.save t
  H.modify_ _ { table = t }
  void $ H.query _machine unit (SQ.SetRouting t unit)

-- | The machine's one `Sounding`, derived exactly as the Triggerfish shell derives
-- | it: playing is being armed, and the mode says who makes the sound.
pushSounding :: forall o o'. Config o -> M o o' Unit
pushSounding cfg = do
  st <- H.get
  void $ H.query _machine unit (SQ.SetSounding (sounding cfg st) unit)
  publishStage cfg
  announce cfg

sounding :: forall o. Config o -> State -> Sounding
sounding cfg st =
  soundingOf st.mode (if st.playing then Set.singleton cfg.which else Set.empty) Set.empty cfg.which

-- | Record the machine's chip and whether it sounds on the rig's stage, if the
-- | rig is there and something changed.
publishStage :: forall o o'. Config o -> M o o' Unit
publishStage cfg = do
  st <- H.get
  for_ (Stage.slotOf cfg.which) \slot -> for_ st.rig \bin -> when st.rigUp do
    let line = Stage.putLine slot st.chip (sounding cfg st /= Silent)
    when (st.staged /= Just line) do
      liftEffect $ Transport.send (Binnacle.socket bin) line
      H.modify_ _ { staged = Just line }

-- | Tell the dashboard, tab to tab, what this machine has loaded and whether it
-- | sounds. Sent on every change and on every tick, so silence means the tab
-- | has gone.
announce :: forall o o'. Config o -> M o o' Unit
announce cfg = do
  st <- H.get
  for_ (Stage.slotOf cfg.which) \slot -> for_ st.bus \bus ->
    liftEffect $ Bus.post bus $ Bus.State
      { machine: slot
      , alias: map _.glyph.alias st.chip
      , edited: maybe false _.diverged st.chip
      , playing: sounding cfg st /= Silent
      }

pushFree :: forall o o'. M o o' Unit
pushFree = do
  st <- H.get
  void $ H.query _machine unit (SQ.SyncFree st.freeT0 (Int.toNumber st.bpm) unit)

targetIsField :: E.Event -> Boolean
targetIsField e = case E.target e of
  Just t -> isJust (HInput.fromEventTarget t) || isJust (HTextArea.fromEventTarget t)
  Nothing -> false

render :: forall o. Config o -> State -> H.ComponentHTML (Action o) (Slots o) Aff
render cfg st =
  HH.div [ style "min-height:100vh;background:#fafafa" ]
    [ bar cfg st
    , case cfg.router of
        Just r | st.routerOpen -> router r st
        _ -> HH.text ""
    , HH.slot _machine unit cfg.component unit FromMachine
    ]

bar :: forall o. Config o -> State -> H.ComponentHTML (Action o) (Slots o) Aff
bar cfg st =
  HH.div
    [ style $ "position:fixed;top:0;left:0;right:0;height:var(--tf-bar);z-index:50;box-sizing:border-box;"
        <> "display:flex;align-items:center;gap:14px;padding:0 16px;overflow:hidden;"
        <> "border-bottom:1px solid #00000026;background:linear-gradient(#f1eee5,#e6e2d6)" ]
    ( [ HH.span [ style (engrave <> ";font-size:11px") ] [ HH.text cfg.nameplate ]
      , HH.div
          [ style "display:flex;border:1px solid #00000033;border-radius:5px;overflow:hidden" ]
          [ modeSeg "Solo" (st.mode == Solo) "#1c1a12" "linear-gradient(#c8a86a,#b8975a)" Solo
          , modeSeg "Atlantis" (st.mode == Atlantis) "#eaf3fa" "linear-gradient(#3a6b8a,#2d5670)" Atlantis
          ]
      , button (if st.playing then "■ Stop" else "▶ Play") TogglePlay
      , HH.label [ style (engrave <> ";font-size:10px;display:flex;align-items:center;gap:6px") ]
          [ HH.text "BPM"
          , HH.input
              [ HP.type_ HP.InputNumber
              , HP.value (show st.bpm)
              , HE.onValueChange SetBpm
              , style "width:52px;font-size:12px;padding:2px 4px"
              ]
          ]
      , button "Capture (c)" Capture
      , HH.span [ style "display:flex;align-items:center;min-width:40px" ] [ chipIcons st.chip ]
      , HH.span [ style "flex:1" ] []
      ]
      <> (case cfg.router of
            Just _ -> [ button (if st.routerOpen then "Close routing" else "Routing (⌥1)") ToggleRouter ]
            Nothing -> [])
      <> [ button "Panic" Panic ]
    )
  where
  modeSeg label active onColor onBg m =
    HH.button
      [ HE.onClick \_ -> SetMode m
      , style $ "padding:5px 13px;border:0;cursor:pointer;font-size:10px;letter-spacing:0.16em;"
          <> "text-transform:uppercase;color:" <> (if active then onColor else "#5a564b")
          <> ";background:" <> (if active then onBg else "linear-gradient(#e9e5d9,#dcd8c9)")
      ]
      [ HH.text label ]
  button label act =
    HH.button
      [ HE.onClick \_ -> act
      , style $ "padding:5px 12px;border:1px solid #00000033;border-radius:5px;cursor:pointer;"
          <> "font-size:10px;letter-spacing:0.14em;text-transform:uppercase;color:#1c1a12;"
          <> "background:linear-gradient(#f4f1e8,#e2ddcf)"
      ]
      [ HH.text label ]

-- | This page's sources, drawn by the same rows as Triggerfish's ⌥1 router, as a
-- | sheet dropped over the machine.
router :: forall o. Router -> State -> H.ComponentHTML (Action o) (Slots o) Aff
router r st =
  HH.div
    [ style $ "position:fixed;top:var(--tf-bar);left:0;right:0;z-index:45;max-height:75vh;overflow-y:auto;"
        <> "box-sizing:border-box;padding:14px 16px 10px;background:#f3f0e7;"
        <> "border-bottom:1px solid #00000026;box-shadow:0 6px 18px #00000022" ]
    [ HH.div [ style "display:flex;align-items:baseline;gap:14px;margin-bottom:10px" ]
        [ HH.span [ style (engrave <> ";font-size:11px") ] [ HH.text r.title ]
        , HH.span [ style "font-size:10px;color:#8a8474" ] [ HH.text r.note ]
        , HH.span
            [ HE.onClick \_ -> ResetRouting
            , HP.title "return these sources to the shipped defaults; other machines' routes are left alone"
            , style "cursor:pointer;font-size:10px;color:#a08676;text-decoration:underline" ]
            [ HH.text r.restoreLabel ]
        ]
    , RV.key env r.sources
    , RV.sourceRows env r.sources
    ]
  where
  env =
    { table: st.table, ports: st.ports, rigUp: st.rigUp, sampleSets: st.sampleSets
    , onEdit: Edit, onAudition: Audition }

rigUrl :: String
rigUrl = "ws://127.0.0.1:3012/ws"
