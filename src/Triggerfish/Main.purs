-- | Triggerfish shell. Triggerfish is a rack of direct-manipulation
-- | instruments over the BEAM-native modules; this root holds a small
-- | instrument selector and shows one at a time.
-- |
-- | All four instruments stay MOUNTED at once — the selector only toggles which
-- | is VISIBLE (`display:none` for the rest). So each module keeps playing when
-- | you switch away: start Odonus, switch to Balistes and start it too, and
-- | they run together as a four-module rig jam (each on its own MIDI channel /
-- | CV, so no conflict). With the rig's Link clock all four phase-lock to the
-- | same anchor; in free-run (no rig) they each run at 120 from their own mount
-- | time and can drift, so a tight multi-module jam wants the rig.
-- |
-- | A fifth selector, TIDAL, is not an instrument but a read-only aggregate: it
-- | pulls each module's current source (via the SourceQuery every instrument
-- | answers) and concatenates them into one document — a one-stop copy of the
-- | whole playing surface, for pasting into Calypso or an editor. Because every
-- | module stays mounted, the music keeps playing while you read or copy it.
module Triggerfish.Main where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Common (joinWith)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Timer (setInterval)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Type.Proxy (Proxy(..))
import Binnacle.Audio (armAudioKeepAlive)
import Binnacle.Time (dateNow)
import Triggerfish.Odonus.Grid as Odonus
import Triggerfish.Balistes.Component as Balistes
import Triggerfish.Selene.Component as Selene
import Triggerfish.SourceQuery as SQ
import Vetula.App as Vetula
import Vetula.Clipboard (copyText)

-- One free-run tempo for the whole rack with no rig. (On the rig the forwarded
-- Link anchor overrides it.) A shell BPM control could drive this later.
freeTempo :: Number
freeTempo = 120.0

main :: Effect Unit
main = HA.runHalogenAff do
  liftEffect armAudioKeepAlive   -- keep the tab audible so background play survives
  body <- HA.awaitBody
  void $ runUI root unit body

data Which = Odo | Bal | Sel | Vet | Tid

derive instance Eq Which

data RAction = Init | SyncTick | Pick Which | RefreshTidal | CopyTidal | PatchVetula | ToggleMaster

-- `playing` is the MASTER transport. Each module's own run button is a sticky
-- arm/cue toggle; a module sounds only when master `playing` AND it is armed. So
-- PLAY starts every armed module together on the shared downbeat, and arming a
-- stopped rack is silent until PLAY.
type RState = { which :: Which, tidalDoc :: String, freeT0 :: Number, playing :: Boolean }

type Slots =
  ( odo :: H.Slot SQ.Query Void Unit
  , bal :: H.Slot SQ.Query Void Unit
  , sel :: H.Slot SQ.Query Void Unit
  , vet :: H.Slot Vetula.SourceQuery Void Unit
  )

_odo :: Proxy "odo"
_odo = Proxy

_bal :: Proxy "bal"
_bal = Proxy

_sel :: Proxy "sel"
_sel = Proxy

_vet :: Proxy "vet"
_vet = Proxy

root :: forall q i o m. MonadAff m => H.Component q i o m
root =
  H.mkComponent
    { initialState: \_ -> { which: Bal, tidalDoc: "", freeT0: 0.0, playing: false }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Init }
    }

handleAction :: forall o m. MonadAff m => RAction -> H.HalogenM RState RAction Slots o m Unit
handleAction = case _ of
  -- Pick one shared free-run epoch for the rack, then keep re-asserting it on a
  -- slow timer so every (mounted, possibly late-initialised) module shares the
  -- same downbeat. Idempotent; a no-op on any module currently Link-locked.
  Init -> do
    now <- liftEffect dateNow
    H.modify_ _ { freeT0 = now * 1000.0 }
    { emitter, listener } <- liftEffect HS.create
    _ <- H.subscribe emitter
    _ <- liftEffect $ setInterval 1500 (HS.notify listener SyncTick)
    handleAction SyncTick
    -- Take control of the rack's transport: every armed module stays silent until
    -- the master PLAY. (Vetula defaults to master=true for standalone, so we must
    -- push the real state here.)
    broadcastMaster false
  -- One master PLAY/STOP for the whole rack: flip it and tell every module, which
  -- then sounds iff (master && its own arm).
  ToggleMaster -> do
    p <- not <$> H.gets _.playing
    H.modify_ _ { playing = p }
    broadcastMaster p
  SyncTick -> do
    t0 <- H.gets _.freeT0
    _ <- H.query _odo unit (SQ.SyncFree t0 freeTempo unit)
    _ <- H.query _bal unit (SQ.SyncFree t0 freeTempo unit)
    _ <- H.query _sel unit (SQ.SyncFree t0 freeTempo unit)
    _ <- H.query _vet unit (Vetula.SyncFree t0 freeTempo unit)
    pure unit
  -- Opening TIDAL pulls a fresh aggregate; the modules keep playing meanwhile.
  Pick Tid -> do
    H.modify_ _ { which = Tid }
    refreshTidal
  Pick w -> H.modify_ _ { which = w }
  RefreshTidal -> refreshTidal
  CopyTidal -> H.gets _.tidalDoc >>= (liftEffect <<< copyText)
  -- The Vetula bridge: pull Vetula's current progression as PC sets and feed it
  -- to Odonus's chord quantiser (which turns on, so it's audible immediately).
  PatchVetula -> do
    mchords <- H.query _vet unit (Vetula.AskChords identity)
    case mchords of
      Just chords -> void $ H.query _odo unit (SQ.FeedChords chords unit)
      Nothing -> pure unit

-- Push the master transport to every module. The three SourceQuery modules and
-- Vetula (its own query type) all answer SetMaster; Selene's is a no-op.
broadcastMaster :: forall o m. MonadAff m => Boolean -> H.HalogenM RState RAction Slots o m Unit
broadcastMaster b = do
  _ <- H.query _odo unit (SQ.SetMaster b unit)
  _ <- H.query _bal unit (SQ.SetMaster b unit)
  _ <- H.query _sel unit (SQ.SetMaster b unit)
  _ <- H.query _vet unit (Vetula.SetMaster b unit)
  pure unit

-- Query each mounted instrument for its current source and stitch the four
-- into one labelled document.
refreshTidal :: forall o m. H.HalogenM RState RAction Slots o m Unit
refreshTidal = do
  o <- H.query _odo unit (SQ.AskSource identity)
  b <- H.query _bal unit (SQ.AskSource identity)
  s <- H.query _sel unit (SQ.AskSource identity)
  v <- H.query _vet unit (Vetula.AskSource identity)
  H.modify_ _
    { tidalDoc = assemble
        [ Tuple "ODONUS" o, Tuple "BALISTES" b, Tuple "SELENE" s, Tuple "VETULA" v ] }

assemble :: Array (Tuple String (Maybe String)) -> String
assemble = joinWith "\n\n\n" <<< map section
  where
  section (Tuple name msrc) =
    "-- ═══════════════  " <> name <> "  ═══════════════\n\n" <> fromMaybe "(no source)" msrc

render :: forall m. MonadAff m => RState -> H.ComponentHTML RAction Slots m
render st =
  HH.div_
    [ masterBar st
    , switchBar st
    -- All four are always in the tree (hence always mounted + running); the
    -- active one is shown, the rest are display:none but keep playing. On the
    -- TIDAL tab all four are hidden but still alive (and queryable).
    , pane (st.which == Odo) (HH.slot_ _odo unit Odonus.component unit)
    , pane (st.which == Bal) (HH.slot_ _bal unit Balistes.component unit)
    , pane (st.which == Sel) (HH.slot_ _sel unit Selene.component unit)
    , pane (st.which == Vet) (HH.slot_ _vet unit Vetula.component unit)
    , if st.which == Tid then tidalView st else HH.text ""
    -- On the Odonus tab, a patch button pulls Vetula's progression into its
    -- chord quantiser.
    , if st.which == Odo then patchButton else HH.text ""
    ]

-- Pull Vetula's current progression into Odonus's chord quantiser.
patchButton :: forall m. H.ComponentHTML RAction Slots m
patchButton =
  HH.button
    [ HE.onClick \_ -> PatchVetula
    , style $ "position:fixed;top:44px;right:14px;z-index:50;padding:5px 12px;cursor:pointer;"
        <> "border:1px solid #b8975a;border-radius:6px;font-family:Georgia,serif;"
        <> "font-size:10px;letter-spacing:0.1em;text-transform:uppercase;color:#5a4a22;"
        <> "background:linear-gradient(#f3ecd9,#e9e0c6);box-shadow:0 1px 4px #0000002a" ]
    [ HH.text "◄ Vetula chords" ]

-- A mounted-but-maybe-hidden pane. `display:none` keeps the component alive
-- (and its scheduler/MIDI running) while removing it from layout.
pane :: forall m. Boolean -> H.ComponentHTML RAction Slots m -> H.ComponentHTML RAction Slots m
pane visible content =
  HH.div [ style (if visible then "" else "display:none") ] [ content ]

-- The read-only aggregate of all four modules' source, for copy / paste into
-- Calypso or an editor.
tidalView :: forall m. RState -> H.ComponentHTML RAction Slots m
tidalView st =
  HH.div
    [ style "max-width:880px;margin:54px auto 40px;padding:0 16px;font-family:Georgia,serif" ]
    [ HH.div
        [ style "display:flex;align-items:baseline;gap:14px;margin-bottom:12px" ]
        [ HH.span
            [ style "font-size:13px;letter-spacing:0.14em;text-transform:uppercase;color:#5a564b" ]
            [ HH.text "Tidal — the whole playing surface" ]
        , barBtn "copy" CopyTidal
        , barBtn "refresh" RefreshTidal
        ]
    , HH.pre
        [ style $ "margin:0;padding:16px 18px;background:#ffffff;border:1px solid #e3dfd2;"
            <> "border-radius:6px;box-shadow:0 1px 4px #00000012;overflow:auto;max-height:78vh;"
            <> "font-family:'SF Mono',Menlo,Consolas,monospace;font-size:12px;line-height:1.55;"
            <> "color:#2a271e;white-space:pre;-webkit-user-select:text;user-select:text" ]
        [ HH.text (if st.tidalDoc == "" then "(refresh to gather the four modules)" else st.tidalDoc) ]
    ]

barBtn :: forall m. String -> RAction -> H.ComponentHTML RAction Slots m
barBtn label act =
  HH.button
    [ HE.onClick \_ -> act
    , style $ "padding:3px 11px;border:1px solid #cdbb96;border-radius:4px;cursor:pointer;"
        <> "font-size:10px;letter-spacing:0.1em;text-transform:uppercase;color:#5a4a22;"
        <> "background:linear-gradient(#f3ecd9,#e9e0c6)" ]
    [ HH.text label ]

-- The master transport, top-left: one PLAY/STOP for the whole rack. Each module
-- arms itself; this gates whether the armed ones sound.
masterBar :: forall m. RState -> H.ComponentHTML RAction Slots m
masterBar st =
  HH.div
    [ style "position:fixed;top:10px;left:14px;z-index:50;font-family:Georgia,serif" ]
    [ HH.button
        [ HE.onClick \_ -> ToggleMaster
        , style $ "padding:6px 20px;border:1px solid #00000033;border-radius:7px;cursor:pointer;"
            <> "font-size:12px;letter-spacing:0.16em;text-transform:uppercase;box-shadow:0 1px 4px #0000002a;"
            <> "color:" <> (if st.playing then "#fbeae7" else "#1c1a12")
            <> ";background:" <> (if st.playing then "linear-gradient(#b23b28,#9a3120)" else "linear-gradient(#c8a86a,#b8975a)") ]
        [ HH.text (if st.playing then "■ STOP" else "▶ PLAY") ]
    ]

-- A small floating selector, top-right, in the Hainbach idiom.
switchBar :: forall m. RState -> H.ComponentHTML RAction Slots m
switchBar st =
  HH.div
    [ style $ "position:fixed;top:10px;right:14px;z-index:50;display:flex;gap:0;"
        <> "border:1px solid #00000033;border-radius:7px;overflow:hidden;"
        <> "box-shadow:0 1px 4px #0000002a;font-family:Georgia,serif" ]
    [ seg "ODONUS" (st.which == Odo) (Pick Odo)
    , seg "BALISTES" (st.which == Bal) (Pick Bal)
    , seg "SELENE" (st.which == Sel) (Pick Sel)
    , seg "VETULA" (st.which == Vet) (Pick Vet)
    , seg "TIDAL" (st.which == Tid) (Pick Tid)
    ]

seg :: forall m. String -> Boolean -> RAction -> H.ComponentHTML RAction Slots m
seg label active act =
  HH.button
    [ HE.onClick \_ -> act
    , style $ "padding:6px 14px;border:0;cursor:pointer;font-size:11px;letter-spacing:0.12em;"
        <> "text-transform:uppercase;color:" <> (if active then "#1c1a12" else "#5a564b")
        <> ";background:" <> (if active then "linear-gradient(#c8a86a,#b8975a)" else "linear-gradient(#e9e5d9,#dcd8c9)") ]
    [ HH.text label ]

style :: forall r i. String -> HP.IProp r i
style = HP.attr (H.AttrName "style")
