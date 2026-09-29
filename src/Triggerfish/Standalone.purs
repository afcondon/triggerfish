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
-- |   * the preset chip, and the CAPTURE key (`c`).
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
import Data.Maybe (Maybe(..), isJust)
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
import Triggerfish.Transport (Mode(..), Which, soundingOf)
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

type Slots o = (machine :: H.Slot SQ.Query o Unit)

_machine :: Proxy "machine"
_machine = Proxy

root :: forall q i o' o. Config o -> H.Component q i o' Aff
root cfg = H.mkComponent
  { initialState: \_ ->
      { mode: Solo, playing: false, bpm: 120, freeT0: 0.0, chip: Nothing, rig: Nothing
      , rigUp: false, table: RM.defaultTable, ports: [], sampleSets: [], routerOpen: false }
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
    keepTable (RE.resetSources r.sources st.table)
  Audition dest -> do
    st <- H.get
    for_ (RO.auditionLine dest) \line ->
      for_ st.rig \bin -> liftEffect $ Transport.send (Binnacle.socket bin) line
  ToggleRouter -> for_ cfg.router \_ -> H.modify_ \s -> s { routerOpen = not s.routerOpen }
  SetPorts ns -> H.modify_ _ { ports = ns }
  -- The rig link, polled as the Triggerfish shell polls it: what a rig-only leg
  -- can reach depends on it.
  Tick -> do
    st <- H.get
    ok <- case st.rig of
      Nothing -> pure false
      Just bin -> liftEffect $ Transport.isConnected (Binnacle.socket bin)
    when (ok /= st.rigUp) (H.modify_ _ { rigUp = ok })
  -- Only the chip is this shell's business; a machine's other outputs (Balistes'
  -- macro-lane edits) belong to the dashboard.
  FromMachine out -> for_ (cfg.chipOf out) \cv -> H.modify_ _ { chip = cv }
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
  let armed = if st.playing then Set.singleton cfg.which else Set.empty
  void $ H.query _machine unit (SQ.SetSounding (soundingOf st.mode armed Set.empty cfg.which) unit)

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
    , HH.div
        [ style "display:grid;grid-template-columns:repeat(auto-fill,minmax(420px,1fr));gap:4px 28px" ]
        (map (RV.sourceRows env) r.sources)
    ]
  where
  env =
    { table: st.table, ports: st.ports, rigUp: st.rigUp, sampleSets: st.sampleSets
    , onEdit: Edit, onAudition: Audition }

rigUrl :: String
rigUrl = "ws://127.0.0.1:3012/ws"
