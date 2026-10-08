-- | Vetula, made to fit the one-machine shell (`Triggerfish.Standalone`).
-- |
-- | Vetula speaks its own query type, and has no continuous frame loop, so in
-- | the Triggerfish page the shell polled it: for its preset chip, and in
-- | Atlantis for its rig payload, re-pushed once an edit settles. This wrapper
-- | does that polling itself and answers the shell's `SourceQuery` by
-- | translating it.
-- |
-- | What passes from Vetula to Odonus (its key, a card's chords) goes by the
-- | harmony routes on the rig (`odonus_feeds`), not from tab to tab.
module Triggerfish.Vetula.Page
  ( Output(..)
  , component
  ) where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Timer (setInterval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Subscription as HS
import Triggerfish.Glyph (ChipView)
import Triggerfish.SourceQuery as SQ
import Data.Array as Array
import Data.String as String
import Triggerfish.Transport (Sounding(..))
import Type.Proxy (Proxy(..))
import Vetula.App as Vetula
import Binnacle (Binnacle)
import Binnacle as Binnacle
import Binnacle.Transport as Transport
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Triggerfish.Odonus.Samples as Samples
import Triggerfish.Vetula.Voices as Voices
import Triggerfish.Bar (Bar)

data Output
  = Chip (Maybe ChipView)
  | Armed Boolean
  | Marked Number
  -- the drawer's rows changed (Vetula's voices, from the rig)
  | BrowserChanged

type State =
  { sounding :: Sounding
  , chip :: Maybe (Maybe ChipView)
  -- The rig payload as last seen and as last sent: a change is re-pushed once
  -- it has held still for one poll, so a drag sends once, after it stops.
  , brushPrev :: String
  , brushSent :: String
  -- Vetula answers nothing until its lattice is built (tens of seconds), so
  -- the poll runs forked, and only one at a time.
  , busy :: Maybe (Ref Boolean)
  -- Vetula's voices as the rig publishes them, the harmony routes, and each
  -- voice's chords by name (Triggerfish.Vetula.Voices); this wrapper's own
  -- socket to the rig
  , bin :: Maybe Binnacle
  , voices :: Array Voices.Voice
  , routesText :: Maybe String
  , names :: Map Int (Array String)
  -- the controls Vetula last gave for the shell's bar, to say when they change
  , lastBar :: Maybe Bar
  }

data Action
  = Init
  | Poll
  | FromVetula Vetula.Output
  | RigFrame String

type Slots = (vet :: H.Slot Vetula.SourceQuery Vetula.Output Unit)

_vet :: Proxy "vet"
_vet = Proxy

component :: forall i. H.Component SQ.Query i Output Aff
component = H.mkComponent
  { initialState: \_ ->
      { sounding: Silent, chip: Nothing
      , brushPrev: "", brushSent: "", busy: Nothing
      , bin: Nothing, voices: [], routesText: Nothing, names: Map.empty, lastBar: Nothing }
  , render: \_ -> HH.slot _vet unit Vetula.component unit FromVetula
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , initialize = Just Init
      }
  }

type M = H.HalogenM State Action Slots Output Aff

handleAction :: Action -> M Unit
handleAction = case _ of
  Init -> do
    { emitter, listener } <- liftEffect HS.create
    _ <- H.subscribe emitter
    gate <- liftEffect (Ref.new false)
    H.modify_ _ { busy = Just gate }
    void $ liftEffect $ setInterval 100 (HS.notify listener Poll)
    -- The rig: Vetula's voices and the harmony routes, for the drawer
    bin <- liftEffect $ Binnacle.connect { url: "ws://127.0.0.1:3012/ws", tempo: 120.0 }
    { emitter: rigE, listener: rigL } <- liftEffect HS.create
    _ <- H.subscribe rigE
    liftEffect $ Binnacle.onAppMessage bin (HS.notify rigL <<< RigFrame)
    liftEffect $ Binnacle.onOpen bin (Transport.send (Binnacle.socket bin) "stage-text-subscribe")
    H.modify_ _ { bin = Just bin }

  RigFrame msg -> do
    for_ (Voices.stageText Voices.routesKey msg) \t -> do
      H.modify_ _ { routesText = t }
      H.raise BrowserChanged
    for_ (Voices.stageText Voices.harmoniesKey msg) \t -> do
      let voices = maybe [] Voices.parseVoices t
      H.modify_ _ { voices = voices }
      -- name each voice's chords: the rig samples its pattern
      st <- H.get
      for_ st.bin \b -> for_ voices \v -> liftEffect (Transport.send (Binnacle.socket b) (Voices.sampleRequest v))
      H.raise BrowserChanged
    for_ (Samples.readSamples msg) \smp ->
      for_ (String.stripPrefix (String.Pattern Voices.sampleKeyPrefix) smp.key >>= Int.fromString) \ch -> do
        H.modify_ \x -> x { names = Map.insert ch (Voices.chordsOf smp.inputs) x.names }
        H.raise BrowserChanged

  Poll -> do
    st <- H.get
    for_ st.busy \gate -> do
      busy <- liftEffect (Ref.read gate)
      unless busy do
        liftEffect (Ref.write true gate)
        void $ H.fork do
          poll
          liftEffect (Ref.write false gate)

  FromVetula out -> case out of
    Vetula.ArmChanged on -> H.raise (Armed on)
    Vetula.StageChanged _ -> pure unit
    Vetula.Marked at -> H.raise (Marked at)

-- | Every write is guarded on a change: this runs ten times a second.
poll :: M Unit
poll = do
  -- the bar's controls: the stage, the counts, the session's rebus
  mbar <- H.query _vet unit (Vetula.AskBar identity)
  stb <- H.get
  when (mbar /= stb.lastBar) do
    H.modify_ _ { lastBar = mbar }
    H.raise BrowserChanged
  mchip <- H.query _vet unit (Vetula.AskChip identity)
  for_ mchip \cv -> do
    st <- H.get
    when (st.chip /= Just cv) do
      H.modify_ _ { chip = Just cv }
      H.raise (Chip cv)
  -- Vetula has no incremental rig path: a settled change to its payload is
  -- pushed again, and only while it is the rig that sounds it.
  st <- H.get
  when (st.sounding == Rig) do
    msig <- H.query _vet unit (Vetula.AskBrushSig identity)
    for_ msig \sig -> do
      when (sig == st.brushPrev && sig /= st.brushSent) do
        void $ H.query _vet unit (Vetula.SetSounding Rig unit)
        H.modify_ _ { brushSent = sig }
      when (sig /= st.brushPrev) (H.modify_ _ { brushPrev = sig })

-- | The drawer's slots for saved progressions, above the voices' (`Voices.slotBase`).
progSlotBase :: Int
progSlotBase = 2000

-- | The shell's queries, in Vetula's terms. What Vetula has no counterpart for
-- | (lanes, a clock, the routing table, a pitch set to follow) is unanswered.
handleQuery :: forall a. SQ.Query a -> M (Maybe a)
handleQuery = case _ of
  -- The browser drawer carries Vetula's scenes as they are (docs/kb/plans/
  -- the-deck.md, 2026-10-05): name first, the key as the tag, the session's
  -- three-glyph as the rebus, so a session's scenes share it. Renaming waits
  -- for the revision of Vetula's saving (a scene's name is its Amphora label),
  -- so the rows are `builtin`: no rename is offered.
  SQ.AskBrowser reply -> do
    mscenes <- H.query _vet unit (Vetula.AskScenes identity)
    mprogs <- H.query _vet unit (Vetula.AskProgressions identity)
    st <- H.get
    -- the progressions you saved, by their frozen names (a glyph triple, one
    -- colour: a container, as a session is)
    let progRows = map (\p -> { slot: progSlotBase + p.slot, name: p.name
                              , icons: map (\icon -> { icon, color: "#2a2a2a" }) (Array.filter (_ /= "") (String.split (String.Pattern "-") (String.takeWhile (_ /= String.codePointFromChar '′') p.name))) <> p.rebus
                              , tag: p.key, current: p.current, section: "Progressions", builtin: true, drag: "vetula-progression " <> p.name, actions: [] })
                       (fromMaybe [] mprogs)
    let voiceRows = Voices.rows st.voices st.names st.routesText
    pure $ mscenes <#> \scenes -> reply
      { title: if Array.null voiceRows && Array.null progRows then "Scenes" else "Vetula", modes: false, keep: "save scene", notice: ""
      , rows: voiceRows <> progRows <> map (_ { section = if Array.null voiceRows && Array.null progRows then "" else "Scenes" }) (Array.mapWithIndex (\i sc -> { slot: i, name: sc.name, icons: map (\icon -> { icon, color: "#2a2a2a" }) (Array.filter (_ /= "") (String.split (String.Pattern "-") (sessionOf sc))), tag: sc.key, current: false, section: "", builtin: true, drag: "", actions: [] }) scenes) }
  -- a voice's row is not a scene: it recalls nothing
  SQ.BrowserRecall i _ next | i >= progSlotBase -> H.query _vet unit (Vetula.LoadEntry (i - progSlotBase) next)
  -- a voice's row: show its card in Limulus (adding the block if it is gone)
  SQ.BrowserRecall i _ next | i >= Voices.slotBase -> H.query _vet unit (Vetula.OpenChannelCard (i - Voices.slotBase) next)
  SQ.BrowserRecall i _ next -> H.query _vet unit (Vetula.LoadSceneAt i next)
  -- progression names are fixed, so the voices that name one keep finding it
  -- (their rows are `builtin`, so the drawer offers no rename)
  SQ.BrowserRename _ _ next -> pure (Just next)
  SQ.BrowserKeep next -> H.query _vet unit (Vetula.SaveSceneQ next)
  SQ.BrowserUndo next -> pure (Just next)
  -- shape / colour Odonus: a route line, as Limulus writes it
  SQ.BrowserAction slot act next -> do
    st <- H.get
    when (slot >= Voices.slotBase) $
      for_ (Voices.toggleLine (slot - Voices.slotBase) act st.routesText) \line ->
        for_ st.bin \b -> liftEffect (Transport.send (Binnacle.socket b) line)
    pure (Just next)
  SQ.BrowserDrop _ next -> pure (Just next)
  SQ.AskBar reply -> H.query _vet unit (Vetula.AskBar reply)
  SQ.BarAction act next -> H.query _vet unit (Vetula.BarAct act next)
  SQ.AskSource k -> H.query _vet unit (Vetula.AskSource k)
  SQ.AskMarkText k -> H.query _vet unit (Vetula.AskMarkText k)
  SQ.AddMarkSnapshot at m t a -> H.query _vet unit (Vetula.AddMarkSnapshot at m t a)
  SQ.SetStagePath segs a -> H.query _vet unit (Vetula.SetStagePath segs a)
  SQ.SyncFree t0 tempo a -> H.query _vet unit (Vetula.SyncFree t0 tempo a)
  SQ.SetSounding s a -> do
    H.modify_ _ { sounding = s }
    H.query _vet unit (Vetula.SetSounding s a)
  SQ.AskSounding k -> H.query _vet unit (Vetula.AskSounding k)
  SQ.AskLibrary k -> H.query _vet unit (Vetula.AskLibrary k)
  SQ.LoadEntry i a -> H.query _vet unit (Vetula.LoadEntry i a)
  SQ.ImportText t k -> H.query _vet unit (Vetula.ImportText t k)
  SQ.Capture a -> H.query _vet unit (Vetula.Capture a)
  SQ.AskBank k -> H.query _vet unit (Vetula.AskBank k)
  SQ.RecallSlot i a -> H.query _vet unit (Vetula.RecallSlot i a)
  SQ.StarSlot i a -> H.query _vet unit (Vetula.StarSlot i a)
  SQ.DeleteSlot i a -> H.query _vet unit (Vetula.DeleteSlot i a)
  SQ.PutLane _ _ _ -> pure Nothing
  SQ.PutSource _ _ -> pure Nothing
  SQ.AskClock _ -> pure Nothing
  SQ.SetRouting _ _ -> pure Nothing

-- | A scene's session, for its rebus: the `session:` tag, else the name's head
-- | (scenes are named `⟨session⟩ #N`).
sessionOf :: { name :: String, session :: String, key :: String } -> String
sessionOf sc
  | sc.session /= "" = sc.session
  | otherwise =
      let head = fromMaybe "" (Array.head (String.split (String.Pattern " #") sc.name))
      -- an alias is icon names joined by "-": a scene from before sessions
      -- ("scene · C locrian") has none
      in if String.contains (String.Pattern " ") head then "" else head
