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
import Control.Monad (whenM)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
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
import Triggerfish.GlyphView (chipIcons)
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
  }

-- | The panel asks to close (Escape inside it).
foreign import limulusAskedClose :: E.Event -> Boolean
foreign import focusFrame :: HTMLElement -> Effect Unit
foreign import focusSelf :: Effect Unit

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
  | FromFrame E.Event

type Slots o = (machine :: H.Slot SQ.Query o Unit)

_machine :: Proxy "machine"
_machine = Proxy

root :: forall q i o' o. Config o -> H.Component q i o' Aff
root cfg = H.mkComponent
  { initialState: \_ ->
      { mode: Solo, playing: false, bpm: 120.0, tempoFlash: Nothing, freeT0: 0.0, chip: Nothing, rig: Nothing
      , rigUp: false, table: RM.defaultTable, staged: Nothing, bus: Nothing, limulus: false, limulusMade: false }
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
  Capture -> void $ H.query _machine unit (SQ.Capture unit)
  ToggleLimulus -> do
    st <- H.get
    let open = not st.limulus && st.mode == Atlantis
    H.modify_ _ { limulus = open, limulusMade = st.limulusMade || open }
    if open then H.getHTMLElementRef limulusRef >>= traverse_ (liftEffect <<< focusFrame)
    else liftEffect focusSelf
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
            "c" -> handleAction cfg Capture
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
  HH.div [ style "min-height:100vh;background:#fafafa" ]
    ( [ bar cfg st
      , HH.slot _machine unit cfg.component unit FromMachine
      ]
      <> (if st.limulusMade then [ limulusPanel st.limulus ] else [])
    )

-- | Limulus beside the machine: the same editor and buffer as its own tab
-- | (same origin), on the right, under the bar. Hidden rather than removed
-- | when closed.
limulusPanel :: forall w i. Boolean -> HH.HTML w i
limulusPanel open =
  HH.div
    [ style $ "position:fixed;top:var(--tf-bar);right:0;bottom:0;width:min(720px,max(420px,46vw));z-index:60;"
        <> "box-shadow:-6px 0 18px #00000040;border-left:1px solid #000;background:#000;"
        <> (if open then "" else "display:none;")
    ]
    [ HH.iframe
        [ HP.src "/limulus/?embed", HP.ref limulusRef, HP.title "Limulus"
        , style "width:100%;height:100%;border:0;display:block"
        ]
    ]

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
      <> [ button "Capture (c)" Capture ]
      -- Limulus combines the machines, on the rig: Atlantis only
      <> (if st.mode == Atlantis then [ button (if st.limulus then "Close Limulus (`)" else "Limulus (`)") ToggleLimulus ] else [])
      <> [ HH.span [ style "display:flex;align-items:center;min-width:40px" ] [ chipIcons st.chip ]
      , HH.span [ style "flex:1" ] []
      ]
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
