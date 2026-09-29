-- | Balistes on its own page (`balistes.html`): the first machine lifted out
-- | of the Triggerfish shell, unchanged. See `docs/kb/plans/the-offering.md`.
-- |
-- | The component is the same one Triggerfish mounts. This module is the
-- | smallest shell that can carry it, doing the four things Triggerfish's shell
-- | did for Balistes:
-- |
-- |   * the transport: Solo or Atlantis, and play/stop, pushed down as the one
-- |     derived `Sounding`;
-- |   * the routing table: loaded from the store, edited in this page's own
-- |     router (the drum lanes only; ⌥1, as in Triggerfish), saved, and pushed
-- |     down. The page is served from the same origin as Triggerfish, so the two
-- |     share one store, and a `storage` event carries an edit made in either
-- |     router to the other live;
-- |   * the free-run clock baseline and tempo (Solo; Link overrides it on the rig);
-- |   * the preset chip, and the CAPTURE key (`c`).
-- |
-- | Everything else the Triggerfish shell offers Balistes (scenes, macro lanes,
-- | the library manager) spans machines, and belongs to the dashboard to come.
-- |
-- | Bundle: `spago bundle --module Triggerfish.Balistes.Main --outfile public/balistes.js`.
module Triggerfish.Balistes.Main (main) where

import Prelude

import Binnacle as Binnacle
import Binnacle.Audio (armAudioKeepAlive)
import Binnacle.Midi as Midi
import Binnacle.Time (dateNow)
import Binnacle.Transport as Transport
import Data.Array ((..))
import Data.Foldable (for_)
import Data.Int as Int
import Data.Maybe (Maybe(..), isJust)
import Data.Set as Set
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Triggerfish.Balistes.Component as Balistes
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
import Triggerfish.Transport (Mode(..), Which(..), soundingOf)
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
import Effect.Timer (setInterval)

main :: Effect Unit
main = HA.runHalogenAff do
  liftEffect armAudioKeepAlive
  body <- HA.awaitBody
  void $ runUI root unit body

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

data Action
  = Init
  | SetMode Mode
  | TogglePlay
  | SetBpm String
  | Capture
  | Panic
  | RoutingChanged
  | ChipChanged (Maybe ChipView)
  | Key E.Event
  -- A macro-lane edit. The lanes are the dashboard's; this page has none.
  | LaneEdited
  | ToggleRouter
  | Edit RE.Edit
  | Audition RM.Destination
  | ResetRouting
  | SetPorts (Array String)
  | Tick

type Slots = (bal :: H.Slot SQ.Query Balistes.Output Unit)

_bal :: Proxy "bal"
_bal = Proxy

root :: forall q i o m. MonadAff m => H.Component q i o m
root = H.mkComponent
  { initialState: \_ ->
      { mode: Solo, playing: false, bpm: 120, freeT0: 0.0, chip: Nothing, rig: Nothing
      , rigUp: false, table: RM.defaultTable, ports: [], sampleSets: [], routerOpen: false }
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Init }
  }

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
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
    -- The port names, for the router's reach column and its port menus. Balistes
    -- asks for MIDI itself to play; this is only to know what exists.
    liftEffect $ Midi.requestAccess case _ of
      Just access -> Midi.outputNames access >>= HS.notify listener <<< SetPorts
      Nothing -> HS.notify listener (SetPorts [])
    _ <- liftEffect $ setInterval 1500 (HS.notify listener Tick)
    void $ H.fork do
      sets <- liftAff SampleSets.load
      H.modify_ _ { sampleSets = sets }
    target <- liftEffect $ Window.toEventTarget <$> window
    _ <- H.subscribe $ eventListener KET.keydown target (Just <<< Key)
    handleAction RoutingChanged
    pushFree
    pushSounding
  SetMode m -> do
    H.modify_ _ { mode = m }
    liftEffect $ TransportStore.save m
    pushSounding
  TogglePlay -> do
    H.modify_ \s -> s { playing = not s.playing }
    pushSounding
  SetBpm v -> for_ (Int.fromString v) \n -> do
    H.modify_ _ { bpm = clamp 20 999 n }
    pushFree
  Capture -> void $ H.query _bal unit (SQ.Capture unit)
  Panic -> do
    st <- H.get
    for_ st.rig \bin -> liftEffect $ Transport.send (Binnacle.socket bin) "hush"
    H.modify_ _ { playing = false }
    pushSounding
  RoutingChanged -> do
    mtbl <- liftEffect RStore.load
    for_ mtbl \t -> do
      H.modify_ _ { table = t }
      void $ H.query _bal unit (SQ.SetRouting t unit)
  -- An edit here is saved first, so the store, Balistes and an open Triggerfish
  -- (through its storage event) all agree on the next note.
  Edit e -> do
    st <- H.get
    for_ (RE.apply { ports: st.ports, sampleSets: st.sampleSets } e st.table) keepTable
  ResetRouting -> do
    st <- H.get
    keepTable (RE.resetSources drumLanes st.table)
  Audition dest -> do
    st <- H.get
    for_ (RO.auditionLine dest) \line ->
      for_ st.rig \bin -> liftEffect $ Transport.send (Binnacle.socket bin) line
  ToggleRouter -> H.modify_ \s -> s { routerOpen = not s.routerOpen }
  SetPorts ns -> H.modify_ _ { ports = ns }
  -- The rig link, polled as the Triggerfish shell polls it: what a rig-only leg
  -- can reach depends on it.
  Tick -> do
    st <- H.get
    ok <- case st.rig of
      Nothing -> pure false
      Just bin -> liftEffect $ Transport.isConnected (Binnacle.socket bin)
    when (ok /= st.rigUp) (H.modify_ _ { rigUp = ok })
  ChipChanged cv -> H.modify_ _ { chip = cv }
  LaneEdited -> pure unit
  Key e -> for_ (KE.fromEvent e) \ke -> unless (targetIsField e || KE.metaKey ke || KE.ctrlKey ke) do
    -- ⌥1 by the key's position, as in Triggerfish: on a Mac, Option+1 types "¡".
    if KE.altKey ke then
      when (KE.code ke == "Digit1") do
        liftEffect $ E.preventDefault e
        handleAction ToggleRouter
    else case KE.key ke of
      "c" -> handleAction Capture
      " " -> do
        liftEffect $ E.preventDefault e
        handleAction TogglePlay
      _ -> pure unit

keepTable :: forall o m. MonadAff m => RM.Table -> H.HalogenM State Action Slots o m Unit
keepTable t = do
  liftEffect $ RStore.save t
  H.modify_ _ { table = t }
  void $ H.query _bal unit (SQ.SetRouting t unit)

-- | The sources this page routes: Balistes' sixteen kit lanes.
drumLanes :: Array RM.Source
drumLanes = map RM.SDrumLane (0 .. 15)

-- | Balistes' one `Sounding`, derived exactly as the Triggerfish shell derives it:
-- | playing is being armed, and the mode says who makes the sound.
pushSounding :: forall o m. MonadAff m => H.HalogenM State Action Slots o m Unit
pushSounding = do
  st <- H.get
  let armed = if st.playing then Set.singleton Bal else Set.empty
  void $ H.query _bal unit (SQ.SetSounding (soundingOf st.mode armed Set.empty Bal) unit)

pushFree :: forall o m. MonadAff m => H.HalogenM State Action Slots o m Unit
pushFree = do
  st <- H.get
  void $ H.query _bal unit (SQ.SyncFree st.freeT0 (Int.toNumber st.bpm) unit)

targetIsField :: E.Event -> Boolean
targetIsField e = case E.target e of
  Just t -> isJust (HInput.fromEventTarget t) || isJust (HTextArea.fromEventTarget t)
  Nothing -> false

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render st =
  -- Balistes' panel is `position:fixed; top:var(--tf-bar)`: it expects a shell
  -- bar pinned in the `--tf-bar` it reserves (balistes.html sets it, as
  -- index.html does). A bar in the flow instead sits UNDER the panel, drawn but
  -- unclickable.
  HH.div [ style "min-height:100vh;background:#fafafa" ]
    [ bar st
    , if st.routerOpen then router st else HH.text ""
    , HH.slot _bal unit Balistes.component unit case _ of
        Balistes.IdentityChanged cv -> ChipChanged cv
        Balistes.LaneEdited _ -> LaneEdited
    ]

bar :: forall m. State -> H.ComponentHTML Action Slots m
bar st =
  HH.div
    [ style $ "position:fixed;top:0;left:0;right:0;height:var(--tf-bar);z-index:50;box-sizing:border-box;"
        <> "display:flex;align-items:center;gap:14px;padding:0 16px;overflow:hidden;"
        <> "border-bottom:1px solid #00000026;background:linear-gradient(#f1eee5,#e6e2d6)" ]
    [ HH.span [ style (engrave <> ";font-size:11px") ] [ HH.text "Triggerfish · Model Balistes" ]
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
    , button (if st.routerOpen then "Close routing" else "Routing (⌥1)") ToggleRouter
    , button "Panic" Panic
    ]
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

-- | The drum lanes' routes, drawn by the same rows as Triggerfish's ⌥1 router.
router :: forall m. State -> H.ComponentHTML Action Slots m
router st =
  HH.div
    [ style $ "position:fixed;top:var(--tf-bar);left:0;right:0;z-index:45;max-height:75vh;overflow-y:auto;"
        <> "box-sizing:border-box;padding:14px 16px 10px;background:#f3f0e7;"
        <> "border-bottom:1px solid #00000026;box-shadow:0 6px 18px #00000022" ]
    [ HH.div [ style "display:flex;align-items:baseline;gap:14px;margin-bottom:10px" ]
        [ HH.span [ style (engrave <> ";font-size:11px") ] [ HH.text "Routing · Balistes kit" ]
        , HH.span [ style "font-size:10px;color:#8a8474" ]
            [ HH.text "shared with Triggerfish's router; sample legs sound in Atlantis only" ]
        , HH.span
            [ HE.onClick \_ -> ResetRouting
            , HP.title "return the sixteen kit lanes to the shipped defaults; other machines' routes are left alone"
            , style "cursor:pointer;font-size:10px;color:#a08676;text-decoration:underline" ]
            [ HH.text "restore default kit routing" ]
        ]
    , HH.div
        [ style "display:grid;grid-template-columns:repeat(auto-fill,minmax(420px,1fr));gap:4px 28px" ]
        (map (RV.sourceRows env) drumLanes)
    ]
  where
  env =
    { table: st.table, ports: st.ports, rigUp: st.rigUp, sampleSets: st.sampleSets
    , onEdit: Edit, onAudition: Audition }

rigUrl :: String
rigUrl = "ws://127.0.0.1:3012/ws"
