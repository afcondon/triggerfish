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
-- |   * the routing table: loaded from the store and pushed down, and again
-- |     whenever it changes. It is edited only on the dashboard (since
-- |     2026-10-03: one place to route, as for the tempo); the pages share one
-- |     origin, so one store, and follow its edits live (`Routing.Store.onChange`);
-- |   * the free-run clock baseline and tempo (Solo; Link overrides it on the rig);
-- |   * the preset chip, and the CAPTURE key (`c`);
-- |   * the machine's stage slot: its chip and whether it sounds, recorded on
-- |     the rig whenever either changes (`Triggerfish.Stage`), for the dashboard;
-- |   * the tab bus (`Binnacle.TabBus`): the same state announced to the
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
  , run
  ) where

import Prelude

import Binnacle as Binnacle
import Binnacle.Audio (armAudioKeepAlive)
import Binnacle.Time (dateNow)
import Binnacle.Transport as Transport
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Set as Set
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
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
import Triggerfish.Fish as Fish
import Triggerfish.Glyph (ChipView)
import Triggerfish.GlyphView (chipIcons, faIcon)
import Triggerfish.Bar (Bar)
import Triggerfish.Browser as Browser
import Halogen.Widgets.Drawer as Drawer
import Data.Array as Array
import Data.String as String
import Triggerfish.Routing.Model as RM
import Triggerfish.Routing.Store as RStore
import Triggerfish.SourceQuery as SQ
import Triggerfish.Stage as Stage
import Binnacle.TabBus as Bus
import Triggerfish.Tempo as Tempo
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
import Web.HTML.HTMLElement (HTMLElement)
import Web.Event.Event (EventType(..))

-- | One machine's page.
-- |
-- | - `which`: the machine, for the transport's rules;
-- | - `nameplate`: the engraved name at the left of the bar;
-- | - `chipOf`: the preset chip, when an output carries one;
-- | - `armOf`: the machine armed or disarmed itself (Vetula's own play, stop
-- |   and unload), which the shell's transport follows;
-- | - `markOf`: the machine made a mark (named by its time), which the shell
-- |   makes rig-wide: every other open machine's text at that moment comes
-- |   back over the tab bus and is kept with it (docs/kb/plans/the-deck.md).
type Config o =
  { which :: Which
  , nameplate :: String
  , component :: H.Component SQ.Query Unit o Aff
  , chipOf :: o -> Maybe (Maybe ChipView)
  , armOf :: o -> Maybe Boolean
  , markOf :: o -> Maybe Number
  -- | whether the page has a Play: Selene does not, since what it applies
  -- | runs on the modular from the moment it is applied, and the Dashboard
  -- | stops and resumes it there
  , playable :: Boolean
  -- | whether the bar offers Capture (c): not Selene, whose drawer keeps racks
  , capturable :: Boolean
  }

run :: forall o. Config o -> Effect Unit
run cfg = HA.runHalogenAff do
  liftEffect armAudioKeepAlive
  body <- HA.awaitBody
  liftEffect Fish.install
  void $ runUI (root cfg) unit body

type State =
  { mode :: Mode
  , playing :: Boolean
  -- The free-run tempo (Triggerfish.Tempo): set on the dashboard or by the
  -- tempo hotkeys, never on a page; the clock follows Link instead once the
  -- rig's anchor arrives.
  , bpm :: Number
  -- The tempo a hotkey just set, shown for a moment, and which press it was.
  , tempoFlash :: Maybe { bpm :: Number, n :: Int }
  , freeT0 :: Number
  , chip :: Maybe ChipView
  -- the machine's own controls for the bar (Triggerfish.Bar); Nothing: none
  , machineBar :: Maybe Bar
  , rig :: Maybe Binnacle.Binnacle
  , rigUp :: Boolean
  , table :: RM.Table
  -- The last stage-put sent, so an unchanged slot sends nothing; cleared when
  -- the rig goes away, so a reconnect records it again.
  , staged :: Maybe String
  , bus :: Maybe Bus.Bus
  -- Limulus as a panel on this page (Atlantis only; docs/kb/plans/the-deck.md,
  -- revision 2026-10-04): open, and whether its frame exists yet. The frame is
  -- kept once made, hidden when closed, so its log and undo survive.
  , limulus :: Boolean
  , limulusMade :: Boolean
  -- its width, as a drawer from the right (a view preference, per page)
  , limWidth :: Number
  -- the page gives Limulus a region that keeps it open (Vetula's Perform)
  , limulusAlways :: Boolean
  -- The browser drawer on the left (Triggerfish.Browser): the machine's rows
  -- (Nothing: it keeps nothing to browse, so there is no drawer), the
  -- drawer's place, the way a row was last recalled, and a name being edited.
  , browser :: Maybe Browser.Browser
  , drawer :: { open :: Boolean, width :: Number }
  , lastRecall :: Browser.Recall
  , renaming :: Maybe { slot :: Int, text :: String }
  -- a delete clicked once, waiting for the second click that confirms it
  , confirming :: Maybe { slot :: Int, act :: String }
  }

-- | The panel asks to close (Escape inside it).
foreign import limulusAskedClose :: E.Event -> Boolean
foreign import focusFrame :: HTMLElement -> Effect Unit
foreign import focusSelf :: Effect Unit
foreign import watchDocks :: (Boolean -> Effect Unit) -> Effect Unit
foreign import selectAll :: HTMLElement -> Effect Unit
foreign import setDragText :: E.Event -> String -> Effect Unit
foreign import currentDrag :: Effect String
foreign import allowDrop :: E.Event -> Effect Unit
foreign import dropText :: E.Event -> Effect String
foreign import loadDrawer :: String -> Effect { open :: Boolean, width :: Number }
foreign import loadWidth :: String -> Number -> Effect Number
foreign import saveDrawer :: String -> { open :: Boolean, width :: Number } -> Effect Unit

data Action o
  = Init
  | TogglePlay
  | TempoStored
  | BumpTempo Tempo.Bump
  | EndFlash Int
  | Capture
  | Panic
  | RoutingChanged
  | FromMachine o
  | Key E.Event
  | Tick
  | FromBus Bus.Msg
  | ModeStored
  | ToggleLimulus
  | FromLimDrawer Drawer.Output
  | FromFrame E.Event
  | DockAlways Boolean
  | AskBrowser
  | AskBar
  | PressBar String
  | FromDrawer Drawer.Output
  | RecallRow Int Browser.Recall
  | StartRename Browser.Item
  | RenameInput String
  | RenameKey KE.KeyboardEvent
  | CommitRename
  | KeepRow
  | UndoRow
  | RowAction Int String
  | DrawerDragOver E.Event
  | DrawerDrop E.Event
  | DragRow String E.Event

type Slots o = (machine :: H.Slot SQ.Query o Unit, drawer :: Drawer.Slot Unit, limdrawer :: Drawer.Slot Unit)

_limdrawer :: Proxy "limdrawer"
_limdrawer = Proxy

_drawer :: Proxy "drawer"
_drawer = Proxy

_machine :: Proxy "machine"
_machine = Proxy

root :: forall q i o' o. Config o -> H.Component q i o' Aff
root cfg = H.mkComponent
  { initialState: \_ ->
      { mode: Solo, playing: false, bpm: 120.0, tempoFlash: Nothing, freeT0: 0.0, chip: Nothing, machineBar: Nothing, rig: Nothing
      , rigUp: false, table: RM.defaultTable, staged: Nothing, bus: Nothing, limulus: false, limulusMade: false, limulusAlways: false, limWidth: 560.0
      , browser: Nothing, drawer: { open: false, width: 280.0 }, lastRecall: { frozen: false, inKey: false }, renaming: Nothing, confirming: Nothing }
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
    bpm <- liftEffect $ fromMaybe 120.0 <$> Tempo.load
    rig <- liftEffect $ Binnacle.connect { url: rigUrl, tempo: bpm }
    H.modify_ _ { rig = Just rig, bpm = bpm }
    mmode <- liftEffect TransportStore.load
    for_ mmode \m -> H.modify_ _ { mode = m }
    { emitter, listener } <- liftEffect HS.create
    _ <- H.subscribe emitter
    liftEffect $ RStore.onChange (HS.notify listener RoutingChanged)
    liftEffect $ TransportStore.onChange (HS.notify listener ModeStored)
    liftEffect $ Tempo.onChange (HS.notify listener TempoStored)
    bus <- liftEffect Bus.open
    H.modify_ _ { bus = Just bus }
    liftEffect $ Bus.onMessage bus (HS.notify listener <<< FromBus)
    liftEffect $ Bus.sayGoodbye bus (maybe [] pure (Stage.slotOf cfg.which))
    -- Ask the other pages to say where they are (the dashboard's chips).
    liftEffect $ Bus.post bus Bus.Hello
    _ <- liftEffect $ setInterval 1500 (HS.notify listener Tick)
    target <- liftEffect $ Window.toEventTarget <$> window
    _ <- H.subscribe $ eventListener KET.keydown target (Just <<< Key)
    _ <- H.subscribe $ eventListener (EventType "message") target (Just <<< FromFrame)
    liftEffect $ watchDocks (HS.notify listener <<< DockAlways)
    drawer <- liftEffect (loadDrawer (drawerKey cfg))
    limWidth <- liftEffect (loadWidth (limKey cfg) 560.0)
    H.modify_ _ { drawer = drawer, limWidth = limWidth }
    handleAction cfg AskBrowser
    handleAction cfg AskBar
    handleAction cfg RoutingChanged
    pushFree
    pushSounding cfg
  TogglePlay -> do
    H.modify_ \s -> s { playing = not s.playing }
    pushSounding cfg
  -- The tempo, set in another tab (the dashboard, or a hotkey there).
  TempoStored -> do
    mbpm <- liftEffect Tempo.load
    for_ mbpm \bpm -> do
      H.modify_ _ { bpm = bpm }
      pushFree
  -- A tempo hotkey: from the tempo the clock is keeping (Link's, once
  -- anchored), set for every page and the rig; shown for a moment here.
  BumpTempo d -> do
    st <- H.get
    for_ st.rig \bin -> do
      now <- liftEffect $ Tempo.current bin st.bpm
      let bpm = Tempo.clampTempo (now + d)
          n = maybe 1 (\f -> f.n + 1) st.tempoFlash
      liftEffect $ Tempo.set bin bpm
      H.modify_ _ { bpm = bpm, tempoFlash = Just { bpm, n } }
      pushFree
      void $ H.fork do
        liftAff $ delay (Milliseconds 1200.0)
        handleAction cfg (EndFlash n)
  EndFlash n -> H.modify_ \s -> s { tempoFlash = if map _.n s.tempoFlash == Just n then Nothing else s.tempoFlash }
  Capture -> do
    void $ H.query _machine unit (SQ.Capture unit)
    handleAction cfg AskBrowser
  AskBrowser -> do
    b <- H.query _machine unit (SQ.AskBrowser identity)
    H.modify_ _ { browser = b }
  AskBar -> do
    b <- H.query _machine unit (SQ.AskBar identity)
    st <- H.get
    when (b /= st.machineBar) (H.modify_ _ { machineBar = b })
  PressBar act -> do
    void $ H.query _machine unit (SQ.BarAction act unit)
    handleAction cfg AskBar
  FromDrawer out -> do
    st <- H.get
    let d = case out of
          Drawer.Toggled open -> st.drawer { open = open }
          Drawer.Resizing width -> st.drawer { width = width }
          Drawer.Resized width -> st.drawer { width = width }
    H.modify_ _ { drawer = d }
    case out of
      Drawer.Resizing _ -> pure unit
      _ -> liftEffect (saveDrawer (drawerKey cfg) d)
  RecallRow slot r -> do
    H.modify_ _ { lastRecall = r }
    void $ H.query _machine unit (SQ.BrowserRecall slot r unit)
    handleAction cfg AskBrowser
  -- An action on a row; a delete asks first (a second click confirms).
  RowAction slot act -> do
    st <- H.get
    if act == "delete" && st.confirming /= Just { slot, act } then H.modify_ _ { confirming = Just { slot, act } }
    else do
      H.modify_ _ { confirming = Nothing }
      void $ H.query _machine unit (SQ.BrowserAction slot act unit)
      handleAction cfg AskBrowser
  -- Something from the page dropped on the drawer ("keep:…", e.g. Selene's
  -- rack rebus): the machine keeps it.
  DrawerDragOver e -> do
    t <- liftEffect currentDrag
    when (String.take 5 t == "keep:") (liftEffect (allowDrop e))
  DrawerDrop e -> do
    t <- liftEffect (dropText e)
    when (String.take 5 t == "keep:") do
      void $ H.query _machine unit (SQ.BrowserDrop t unit)
      handleAction cfg AskBrowser
  UndoRow -> do
    void $ H.query _machine unit (SQ.BrowserUndo unit)
    handleAction cfg AskBrowser
  DragRow text e -> liftEffect (setDragText e text)
  KeepRow -> do
    void $ H.query _machine unit (SQ.BrowserKeep unit)
    handleAction cfg AskBrowser
  StartRename row -> do
    H.modify_ _ { renaming = Just { slot: row.slot, text: row.name } }
    H.getHTMLElementRef renameRef >>= traverse_ (liftEffect <<< selectAll)
  RenameInput t -> H.modify_ \st -> st { renaming = map (_ { text = t }) st.renaming }
  RenameKey ke -> case KE.key ke of
    "Enter" -> handleAction cfg CommitRename
    "Escape" -> H.modify_ _ { renaming = Nothing }
    _ -> pure unit
  CommitRename -> do
    st <- H.get
    for_ st.renaming \r -> when (r.text /= "") do
      void $ H.query _machine unit (SQ.BrowserRename r.slot r.text unit)
    H.modify_ _ { renaming = Nothing }
    handleAction cfg AskBrowser
  ToggleLimulus -> do
    st <- H.get
    let open = not st.limulus && st.mode == Atlantis
    -- made shut the first time, then opened a frame later, so it eases open
    -- like every later time rather than appearing at full width
    when (open && not st.limulusMade) do
      H.modify_ _ { limulusMade = true }
      liftAff (delay (Milliseconds 30.0))
    H.modify_ _ { limulus = open }
    if open then H.getHTMLElementRef limulusRef >>= traverse_ (liftEffect <<< focusFrame)
    else liftEffect focusSelf
  FromLimDrawer out -> case out of
    Drawer.Toggled open -> do
      st <- H.get
      when (open /= st.limulus) (handleAction cfg ToggleLimulus)
    Drawer.Resizing w -> H.modify_ _ { limWidth = w }
    Drawer.Resized w -> do
      H.modify_ _ { limWidth = w }
      liftEffect (saveDrawer (limKey cfg) { open: false, width: w })
  DockAlways on -> H.modify_ _ { limulusAlways = on }
  FromFrame e -> when (limulusAskedClose e) do
    H.modify_ _ { limulus = false }
    liftEffect focusSelf
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
  -- The rig link, polled: the stage slot is recorded only while it is up.
  Tick -> do
    st <- H.get
    ok <- case st.rig of
      Nothing -> pure false
      Just bin -> liftEffect $ Transport.isConnected (Binnacle.socket bin)
    when (ok /= st.rigUp) (H.modify_ _ { rigUp = ok, staged = Nothing })
    publishStage cfg
    announce cfg
    handleAction cfg AskBrowser
  -- The dashboard's commands, for this machine or for all.
  FromBus msg -> do
    st <- H.get
    let mine m = Stage.slotOf cfg.which == Just m
    case msg of
      Bus.Play m | cfg.playable && mine m && not st.playing -> handleAction cfg TogglePlay
      Bus.Stop m | cfg.playable && mine m && st.playing -> handleAction cfg TogglePlay
      -- The dashboard hushes the rig itself; here only the local transport stops.
      Bus.Panic -> do
        H.modify_ _ { playing = false }
        pushSounding cfg
      Bus.Hello -> announce cfg
      -- Another page marked: answer with this machine as text at this moment.
      Bus.Marked m | not (mine m.by) -> for_ (Stage.slotOf cfg.which) \machine -> do
        mtext <- H.query _machine unit (SQ.AskMarkText identity)
        text <- case mtext of
          Just t -> pure (Just t)
          Nothing -> H.query _machine unit (SQ.AskSource identity)
        for_ text \t -> for_ st.bus \bus ->
          liftEffect $ Bus.post bus (Bus.Snapshot { at: m.at, by: m.by, machine, text: t })
      -- An answer to a mark this machine made: keep it with the mark.
      Bus.Snapshot n | mine n.by ->
        void $ H.query _machine unit (SQ.AddMarkSnapshot n.at n.machine n.text unit)
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
  FromMachine out -> do
    -- anything a machine says may change its drawer (Vetula's voices arrive
    -- from the rig) or its controls in the bar, so both are asked for again
    handleAction cfg AskBrowser
    handleAction cfg AskBar
    for_ (cfg.chipOf out) \cv -> do
      H.modify_ _ { chip = cv }
      publishStage cfg
      announce cfg
    -- The machine marked: ask every other open page for its machine's text.
    for_ (cfg.markOf out) \at -> do
      st <- H.get
      for_ (Stage.slotOf cfg.which) \by -> for_ st.bus \bus ->
        liftEffect $ Bus.post bus (Bus.Marked { at, by })
    -- The machine armed or disarmed itself: the transport follows it, and the
    -- sounding it derives goes back down, as the Triggerfish shell does.
    for_ (cfg.armOf out) \on -> do
      st <- H.get
      when (on /= st.playing) do
        H.modify_ _ { playing = on }
        pushSounding cfg
  Key e -> for_ (KE.fromEvent e) \ke -> unless (targetIsField e || KE.metaKey ke || KE.ctrlKey ke) do
    -- The tempo hotkeys, by the key's position (Triggerfish.Tempo.hotkey).
    case Tempo.hotkey ke of
      Just d -> do
        liftEffect $ E.preventDefault e
        handleAction cfg (BumpTempo d)
      Nothing
        | KE.altKey ke -> pure unit
        | otherwise -> case KE.key ke of
            "c" | cfg.capturable -> handleAction cfg Capture
            -- the browser, by the suite's one key for it
            "b" -> do
              st <- H.get
              when (isJust st.browser) (handleAction cfg (FromDrawer (Drawer.Toggled (not st.drawer.open))))
            -- the console key (Vetula has `l`): by position, so any layout
            _ | KE.code ke == "Backquote" -> handleAction cfg ToggleLimulus
            "Escape" -> whenM (H.gets _.limulus) (handleAction cfg ToggleLimulus)
            " " | cfg.playable -> do
              liftEffect $ E.preventDefault e
              handleAction cfg TogglePlay
            _ -> pure unit

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
  void $ H.query _machine unit (SQ.SyncFree st.freeT0 st.bpm unit)

targetIsField :: E.Event -> Boolean
targetIsField e = case E.target e of
  Just t -> isJust (HInput.fromEventTarget t) || isJust (HTextArea.fromEventTarget t)
  Nothing -> false

render :: forall o. Config o -> State -> H.ComponentHTML (Action o) (Slots o) Aff
render cfg st =
  -- `--tf-left`: what the drawer takes on the left; the machine's panel stands
  -- right of it, so the drawer pushes it rather than covering it
  -- and `--tf-right`, Limulus's drawer on the right, the same way
  HH.div [ style ("min-height:100vh;background:#fafafa;--tf-left:" <> show drawerSpan <> "px;--tf-right:" <> show limSpan <> "px;transition:--tf-left 180ms ease-out,--tf-right 180ms ease-out") ]
    ( [ HH.element (HH.ElemName "style") []
          [ HH.text "@property --tf-left{syntax:'<length>';inherits:true;initial-value:0px}@property --tf-right{syntax:'<length>';inherits:true;initial-value:0px}@media (prefers-reduced-motion:reduce){#tf-browser,#tf-limulus,[style*=--tf-left]{transition:none!important}}" ]
      , bar cfg st
      , HH.slot _machine unit cfg.component unit FromMachine
      ]
      <> (case st.browser of
            Just b -> [ browserDrawer st b ]
            Nothing -> [])
      <> (if limDrawer then [ limulusRail ] else [])
      <> (if st.limulusMade || docked then [ limulusPanel { docked, open: st.limulus || docked, width: limW, rail: (limulusInput st).railWidth } ] else [])
    )
  where
  -- Limulus as a drawer from the right, in Atlantis, unless a region keeps
  -- it open (Vetula's Perform)
  limDrawer = st.mode == Atlantis && not docked
  limW = Drawer.clampWidth (limulusInput st) st.limWidth
  limSpan = if limDrawer then (if st.limulus then limW else 0.0) + (limulusInput st).railWidth else 0.0
  limulusRail =
    HH.div
      [ style "position:fixed;top:var(--tf-bar);right:0;bottom:0;z-index:61;display:flex;background:linear-gradient(#ece7da,#e2dccb);border-left:1px solid #b3ae9c" ]
      [ HH.slot _limdrawer unit Drawer.component (limulusInput st) FromLimDrawer ]
  -- a region that keeps Limulus open, in Atlantis (Limulus needs the rig)
  docked = st.limulusAlways && st.mode == Atlantis
  drawerSpan = case st.browser of
    Nothing -> 0.0
    Just _ -> (if st.drawer.open then Drawer.clampWidth (drawerInput st) st.drawer.width else 0.0) + (drawerInput st).railWidth

-- | Limulus's drawer, from the right: the arrow and the grip.
limulusInput :: State -> Drawer.Input
limulusInput st = (Drawer.defaultInput "Limulus")
  { open = st.limulus, width = st.limWidth, resizable = true, edge = Drawer.Right
  , minWidth = 360.0, maxWidth = 1000.0, panelId = "tf-limulus"
  , showLabel = "Show Limulus (`)", hideLabel = "Hide Limulus (`)" }

limKey :: forall o. Config o -> String
limKey cfg = "triggerfish.limulus." <> cfg.nameplate

-- | Where the drawer's place is kept: per page.
drawerKey :: forall o. Config o -> String
drawerKey cfg = "triggerfish.browser." <> cfg.nameplate

drawerInput :: State -> Drawer.Input
drawerInput st = (Drawer.defaultInput "Presets")
  { open = st.drawer.open, width = st.drawer.width, resizable = true
  , minWidth = 180.0, maxWidth = 420.0, panelId = "tf-browser"
  , showLabel = "Show presets (b)", hideLabel = "Hide presets (b)" }

-- | The browser drawer: under the bar on the left, the machine's kept things,
-- | name first, the rebus small beside it (docs/kb/plans/the-deck.md). With
-- | `modes` (Odonus), each row has the 2×2 recall square, its axes in a key
-- | at the top: across, running or frozen; down, as saved or in key.
browserDrawer :: forall o. State -> Browser.Browser -> H.ComponentHTML (Action o) (Slots o) Aff
browserDrawer st b =
  HH.div
    [ style $ "position:fixed;top:var(--tf-bar);left:0;bottom:0;z-index:45;display:flex;"
        <> "font-family:Georgia,serif;background:linear-gradient(#ece7da,#e2dccb);border-right:1px solid #b3ae9c" ]
    [ HH.element (HH.ElemName "style") []
        [ HH.text ".tfb-act{display:none;font:10px Georgia,serif;padding:0 5px;border:1px solid #00000026;border-radius:3px;background:#f6f2e7;cursor:pointer;color:#5a5648}.tfb-row:hover .tfb-act,.tfb-act.ask{display:inline-block}.tfb-act.ask{color:#b3261e;border-color:#b3261e}.tfb-row:hover{background:#00000010}.tfb-q{width:9px;height:9px;border:1px solid #8a8270;background:#f6f2e7;cursor:pointer;padding:0}.tfb-q:hover{background:#2d5670;border-color:#2d5670}.tfb-q.last{background:#c9b98d}.tfb-g i{font-size:11px!important}" ]
    , body
    , HH.slot _drawer unit Drawer.component d FromDrawer
    ]
  where
  d = drawerInput st
  w = Drawer.clampWidth d st.drawer.width
  -- eased open and shut (Halogen.Widgets.Motion's 180 ms, as Conspicillum's
  -- drawer), the rows at their full width throughout so they do not reflow
  body =
    HH.div
      ( [ HP.id "tf-browser"
        , HE.handler (E.EventType "dragover") DrawerDragOver
        , HE.handler (E.EventType "drop") DrawerDrop
        , style $ "width:" <> (if st.drawer.open then show w else "0") <> "px;overflow:hidden;transition:width 180ms ease-out;flex:none" ]
          <> (if st.drawer.open then [] else [ HP.attr (HH.AttrName "inert") "" ])
      )
      [ HH.div
      [ style $ "width:" <> show w <> "px;height:100%;box-sizing:border-box;overflow-y:auto;padding:14px 12px 20px" ]
      ( [ HH.div [ style "display:flex;align-items:baseline;gap:8px;border-bottom:1px solid #00000018;padding-bottom:6px;margin-bottom:8px" ]
            [ HH.span [ style (engrave <> ";font-size:12px;letter-spacing:0.16em;color:#3f3c33") ] [ HH.text (String.toUpper b.title) ]
            , HH.span [ style "flex:1" ] []
            , HH.button
                [ HE.onClick \_ -> KeepRow, HP.title "Keep what the machine has now, as a new row"
                , style "font:11px Georgia,serif;padding:2px 8px;border:1px solid #00000033;border-radius:4px;background:#f6f2e7;cursor:pointer" ]
                [ HH.text b.keep ]
            ]
        ]
          <> (if b.notice == "" then [] else [ notice ])
          <> (if b.modes then [ key ] else [])
          <> (if Array.null b.rows then [ HH.p [ style "font-size:12px;font-style:italic;color:#6a6657" ] [ HH.text ("Nothing here yet: " <> b.keep <> " adds what is playing.") ] ] else [])
          <> Array.concatMap section (Array.nub (map _.section b.rows))
      ) ]
  -- a word from the machine after something it can take back
  notice =
    HH.div [ style "display:flex;align-items:baseline;gap:8px;margin:0 0 10px;padding:6px 8px;border-radius:4px;background:#2d567018;font-size:12px;color:#1c1a12" ]
      [ HH.span [ style "flex:1" ] [ HH.text b.notice ]
      , HH.button
          [ HE.onClick \_ -> UndoRow
          , style "font:11px Georgia,serif;padding:1px 8px;border:1px solid #2d5670;border-radius:4px;background:#f6f2e7;cursor:pointer;color:#2d5670" ]
          [ HH.text "undo" ]
      ]
  -- a section's rows under its heading (none for "")
  section name =
    (if name == "" then [] else [ HH.div [ style (engrave <> ";font-size:10px;letter-spacing:0.14em;color:#6a6657;margin:12px 0 4px 4px") ] [ HH.text (String.toUpper name) ] ])
      <> map row (Array.filter (\r -> r.section == name) b.rows)
  -- the 2×2's axes, once
  key =
    HH.div [ style "display:grid;grid-template-columns:auto 11px 11px;gap:2px 3px;align-items:center;font-size:10px;color:#6a6657;margin:0 0 8px 2px" ]
      [ HH.span_ [], HH.span [ style "writing-mode:vertical-rl;transform:rotate(180deg);font-size:9px" ] [ HH.text "run" ], HH.span [ style "writing-mode:vertical-rl;transform:rotate(180deg);font-size:9px" ] [ HH.text "freeze" ]
      , HH.span [ style "padding-right:4px" ] [ HH.text "as saved" ], cell, cell
      , HH.span [ style "padding-right:4px" ] [ HH.text "in key" ], cell, cell
      ]
  cell = HH.span [ style "width:9px;height:9px;border:1px solid #8a8270" ] []
  row r =
    HH.div
      ( [ HP.class_ (HH.ClassName "tfb-row")
        , style $ "display:flex;align-items:center;gap:8px;padding:3px 4px;border-radius:3px;"
            <> (if r.current then "background:#00000018;" else "")
            <> (if r.drag == "" then "" else "cursor:grab;") ]
          -- a module is dragged onto a bank on the page
          <> (if r.drag == "" then [] else [ HP.attr (HH.AttrName "draggable") "true", HE.handler (E.EventType "dragstart") (DragRow r.drag) ])
      )
      ( (if b.modes then [ square r ] else [])
          <>
            [ case st.renaming of
                Just rn | rn.slot == r.slot ->
                  HH.input
                    [ HP.value rn.text, HE.onValueInput RenameInput, HE.onKeyDown RenameKey, HE.onBlur \_ -> CommitRename
                    , HP.ref renameRef, style "flex:1;min-width:0;font:13px Georgia,serif;padding:1px 3px" ]
                _ ->
                  HH.span
                    ( [ HE.onClick \_ -> RecallRow r.slot st.lastRecall
                      , HP.title (if r.drag /= "" then "Drag it onto a bank." else "Click: recall it, the way you last did. Double-click: rename it.")
                      , style $ "flex:1;min-width:0;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;cursor:pointer;font-size:13px;color:#1c1a12;"
                          <> (if r.current then "font-weight:bold;" else "") ]
                        <> (if r.builtin then [] else [ HE.onDoubleClick \_ -> StartRename r ]) )
                    [ HH.text r.name ]
            ]
          <> (if r.tag == "" then [] else [ HH.span [ style "flex:none;font-size:10px;letter-spacing:0.08em;text-transform:uppercase;color:#6a6657" ] [ HH.text r.tag ] ])
          <> [ HH.span [ HP.class_ (HH.ClassName "tfb-g"), style "flex:none;display:inline-flex;gap:2px;opacity:0.75" ] (map faIcon r.icons) ]
          <> map (\act ->
                let asking = st.confirming == Just { slot: r.slot, act }
                in HH.button
                     [ HP.class_ (HH.ClassName ("tfb-act" <> if asking then " ask" else ""))
                     , HE.onClick \_ -> RowAction r.slot act
                     , HP.title (if asking then "Click again to " <> act <> " it" else act) ]
                     [ HH.text (if asking then act <> "?" else act) ]) r.actions
      )
  -- four ways to recall: across, running or frozen; down, as saved or in key
  square r =
    HH.span [ style "display:grid;grid-template-columns:9px 9px;gap:2px;flex:none" ]
      [ q r false false "as saved, running", q r true false "as saved, generators paused"
      , q r false true "in the live key, running", q r true true "in the live key, generators paused" ]
  q r frozen inKey label =
    HH.button
      [ HP.class_ (HH.ClassName ("tfb-q" <> if st.lastRecall == { frozen, inKey } then " last" else ""))
      , HE.onClick \_ -> RecallRow r.slot { frozen, inKey }
      , HP.title ("Recall " <> r.name <> ": " <> label) ]
      []
-- | Limulus beside the machine: the same editor and buffer as its own tab
-- | (same origin), in the machine's paper look. A drawer from the right,
-- | eased open and shut beside its rail (as the browser on the left), pushing
-- | the page by `--tf-right`; or, where the page keeps a region for it
-- | (`data-limulus-dock="always"`, Vetula's Perform), laid over that region
-- | (`watchDocks`). One frame either way, made once and kept, so its log and
-- | undo survive.
limulusPanel :: forall w i. { docked :: Boolean, open :: Boolean, width :: Number, rail :: Number } -> HH.HTML w i
limulusPanel p =
  HH.div
    ( [ HP.id "tf-limulus"
      , style $ "position:fixed;z-index:60;box-sizing:border-box;overflow:hidden;background:#f6f2e7;"
          <> if p.docked then
               "left:var(--lim-left,auto);right:var(--lim-right,0);top:var(--lim-top,var(--tf-bar));"
                 <> "width:var(--lim-width,50vw);height:var(--lim-height,calc(100vh - var(--tf-bar)));"
                 <> "box-shadow:var(--lim-shadow,none);border-left:var(--lim-edge,none);"
             else
               "right:" <> show p.rail <> "px;top:var(--tf-bar);height:calc(100vh - var(--tf-bar));"
                 <> "width:" <> (if p.open then show p.width else "0") <> "px;transition:width 180ms ease-out;"
                 <> "border-left:" <> (if p.open then "1px solid #b3ae9c" else "0") <> ";"
      ] <> (if p.open then [] else [ HP.attr (HH.AttrName "inert") "" ])
    )
    [ HH.iframe
        [ HP.src "/limulus/?embed&look=paper", HP.ref limulusRef, HP.title "Limulus"
        , style ("height:100%;border:0;display:block;width:" <> (if p.docked then "100%" else show p.width <> "px"))
        ]
    ]

renameRef :: H.RefLabel
renameRef = H.RefLabel "rename"

limulusRef :: H.RefLabel
limulusRef = H.RefLabel "limulus"

bar :: forall o. Config o -> State -> H.ComponentHTML (Action o) (Slots o) Aff
bar cfg st =
  HH.div
    [ style $ "position:fixed;top:0;left:0;right:0;height:var(--tf-bar);z-index:50;box-sizing:border-box;"
        <> "display:flex;align-items:center;gap:14px;padding:0 16px;overflow:hidden;"
        <> "border-bottom:1px solid #00000026;background:linear-gradient(#f1eee5,#e6e2d6)" ]
    ( [ HH.span [ style (engrave <> ";font-size:11px") ] [ HH.text cfg.nameplate ]
      ]
      -- The mode is rig-wide, set on the dashboard; a page only follows it.
      -- Solo, the base case, needs no word; in Atlantis a quiet tag says that
      -- the rig is the one playing.
      <> (if st.mode == Atlantis then [ atlantisTag ] else [])
      <> (if cfg.playable then [ button (if st.playing then "■ Stop" else "▶ Play") TogglePlay ] else [])
      <> (if cfg.capturable then [ button "Capture (c)" Capture ] else [])
      -- Limulus combines the machines, on the rig: its drawer on the right,
      -- in Atlantis
      <> [ HH.span [ style "display:flex;align-items:center;min-width:40px" ] [ chipIcons st.chip ] ]
      <> maybe [] machineControls st.machineBar
      <> [ HH.span [ style "flex:1" ] [] ]
      <> [ button "Panic" Panic ]
      <> case st.tempoFlash of
        Just f -> [ tempoFlash f.bpm ]
        Nothing -> []
    )
  where
  -- What a tempo hotkey set, for a moment, where the bar's middle is free.
  tempoFlash bpm =
    HH.span
      [ HP.title Tempo.hotkeyHelp
      , style $ "position:absolute;left:50%;transform:translateX(-50%);padding:3px 12px;border-radius:4px;"
          <> "font:13px 'SF Mono',Menlo,monospace;font-variant-numeric:tabular-nums;color:#1c1a12;"
          <> "background:#fffdf6;border:1px solid #00000033;box-shadow:0 2px 8px #00000022"
      ]
      [ HH.text (Tempo.showTempo bpm <> " bpm") ]
  atlantisTag =
    HH.span
      [ HP.title "Atlantis: the rig plays; this page sends it what to play. The mode is set on the dashboard."
      , style $ "padding:3px 9px;border-radius:4px;font-size:10px;letter-spacing:0.16em;"
          <> "text-transform:uppercase;color:#eaf3fa;background:linear-gradient(#3a6b8a,#2d5670)"
      ]
      [ HH.text "Atlantis" ]
  -- the machine's own controls (Triggerfish.Bar): its stage tabs, ◆ mark
  -- with the counts and clear, and its rebus, drawn here as the drawer draws
  -- its rows
  machineControls b =
    [ HH.div [ style "display:flex;flex:0 0 auto;border:1px solid #00000026;border-radius:6px;overflow:hidden;box-shadow:0 1px 2px #0000001a" ]
        (map (\t -> HH.button
            [ HE.onClick \_ -> PressBar ("stage:" <> t.id), HP.title t.tip
            , style $ "padding:5px 13px;border:none;cursor:pointer;font-family:Georgia,serif;font-size:11px;letter-spacing:0.12em;"
                <> (if t.active then "background:linear-gradient(#c8a86a,#b8975a);color:#1c1a12;font-weight:600" else "background:linear-gradient(#f4f1e8,#e2ddcf);color:#5a564b") ]
            [ HH.text t.label ]) b.tabs)
    ]
      <> (if b.marks == "" then [] else
        [ HH.button
            [ HE.onClick \_ -> PressBar "mark", HP.title "flag the last couple of bars as a good bit"
            , style "padding:4px 11px;border:1px solid #d8c98a;border-radius:5px;cursor:pointer;background:#fdf7e4;color:#8a6a10;font-size:11px;font-family:Georgia,serif;white-space:nowrap" ]
            [ HH.text "\x25c6 mark" ]
        , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:10px;color:#8a8576;white-space:nowrap" ] [ HH.text b.marks ]
        , HH.button
            [ HE.onClick \_ -> PressBar "clear", HP.title "clear the Review surface: its notes, marks and loops"
            , style "padding:4px 9px;border:1px solid #00000018;border-radius:5px;cursor:pointer;background:transparent;color:#8a8576;font-size:10px;font-family:Georgia,serif" ]
            [ HH.text "clear" ]
        ])
      <> (if Array.null b.icons then [] else
        [ HH.button
            [ HE.onClick \_ -> PressBar "rebus", HP.title b.rebusTip
            , style "display:inline-flex;align-items:center;gap:4px;padding:3px 8px;border:1px solid #00000022;border-radius:5px;cursor:pointer;background:#faf7ee" ]
            (map faIcon b.icons)
        ])
      <> map (\c -> HH.button
            [ HE.onClick \_ -> PressBar ("chip:" <> c.id), HP.title c.tip
            , style $ "padding:4px 10px;border:1px solid #00000022;border-radius:5px;cursor:pointer;font-size:11px;font-family:Georgia,serif;white-space:nowrap;"
                <> (if c.active then "background:#eef3f1;color:#3d5a52" else "background:transparent;color:#8a8576") ]
            [ HH.text c.label ]) b.chips
  button label act =
    HH.button
      [ HE.onClick \_ -> act
      , style $ "padding:5px 12px;border:1px solid #00000033;border-radius:5px;cursor:pointer;"
          <> "font-size:10px;letter-spacing:0.14em;text-transform:uppercase;color:#1c1a12;"
          <> "background:linear-gradient(#f4f1e8,#e2ddcf)"
      ]
      [ HH.text label ]

rigUrl :: String
rigUrl = "ws://127.0.0.1:3012/ws"
