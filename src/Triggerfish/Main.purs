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

import Data.Array (filter, mapWithIndex, null)
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

data RAction
  = Init | SyncTick | PollVetula | Pick Which | RefreshTidal | CopyTidal | ToggleMaster
  | LoadFromLib Which Int       -- A5: make a saved entry active in its instrument
  | CopyEntry String            -- copy one entry's eDSL text
  | SetImportText String
  | ImportInto Which            -- route the paste box to one instrument's library

-- One saved preset gathered from an instrument, for the cross-instrument LIBRARY
-- manager on the TIDAL page. `text` is the entry rendered to Lepidoptera eDSL
-- (the transferable form); `idx` is its position in that instrument's library.
type LibRow = { inst :: Which, idx :: Int, name :: String, text :: String }

-- `playing` is the MASTER transport. Each module's own run button is a sticky
-- arm/cue toggle; a module sounds only when master `playing` AND it is armed. So
-- PLAY starts every armed module together on the shared downbeat, and arming a
-- stopped rack is silent until PLAY.
type RState =
  { which :: Which, tidalDoc :: String, freeT0 :: Number, playing :: Boolean
  , library :: Array LibRow, importText :: String, importMsg :: String }

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
    { initialState: \_ ->
        { which: Bal, tidalDoc: "", freeT0: 0.0, playing: false
        , library: [], importText: "", importMsg: "" }
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
    -- Poll Vetula's Odonus-bound performance voices ~10×/s and feed each one's
    -- current block chord to Odonus, so its quantiser follows the live conductor.
    _ <- liftEffect $ setInterval 100 (HS.notify listener PollVetula)
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
  -- Opening TIDAL pulls a fresh aggregate + library; the modules keep playing.
  Pick Tid -> do
    H.modify_ _ { which = Tid }
    refreshTidal
    refreshLibrary
  Pick w -> H.modify_ _ { which = w }
  RefreshTidal -> refreshTidal *> refreshLibrary
  CopyTidal -> H.gets _.tidalDoc >>= (liftEffect <<< copyText)
  -- A5 manager: load a saved entry into its instrument, and switch to it so the
  -- change is visible. Copy exports one entry's eDSL text.
  LoadFromLib w i -> do
    _ <- queryLoad w i
    H.modify_ _ { which = w }
  CopyEntry txt -> liftEffect (copyText txt)
  SetImportText t -> H.modify_ _ { importText = t }
  -- Route the paste box to one instrument; it accepts iff the text is one of its
  -- own presets. On success, clear the box and re-gather the library.
  ImportInto w -> do
    txt <- H.gets _.importText
    accepted <- queryImport w txt
    let ok = fromMaybe false accepted
    H.modify_ _
      { importMsg = if ok then "✓ imported into " <> whichName w
                    else "✗ not a valid " <> whichName w <> " preset" }
    when ok do
      H.modify_ _ { importText = "" }
      refreshLibrary
  -- The live Vetula→Odonus bridge: pull each Odonus-bound voice's current block
  -- chord and feed the set to Odonus, whose KEY pane picks one (or none) to follow.
  PollVetula -> do
    mfeed <- H.query _vet unit (Vetula.AskVoiceChords identity)
    case mfeed of
      Just feed -> void $ H.query _odo unit (SQ.FeedVoiceChords feed unit)
      Nothing -> pure unit

-- Push the master transport to every module. Odonus/Balistes/Selene answer via
-- the shared SourceQuery; Vetula via its own query type. Each sounds iff
-- master && its own ARM.
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

-- Gather every instrument's saved presets (via AskLibrary) into one flat list,
-- tagged by instrument + index — the data the LIBRARY manager renders.
refreshLibrary :: forall o m. H.HalogenM RState RAction Slots o m Unit
refreshLibrary = do
  o <- H.query _odo unit (SQ.AskLibrary identity)
  b <- H.query _bal unit (SQ.AskLibrary identity)
  s <- H.query _sel unit (SQ.AskLibrary identity)
  v <- H.query _vet unit (Vetula.AskLibrary identity)
  H.modify_ _ { library = rows Odo o <> rows Bal b <> rows Sel s <> rows Vet v }
  where
  rows w m = mapWithIndex (\i e -> { inst: w, idx: i, name: e.name, text: e.text }) (fromMaybe [] m)

-- Dispatch a LoadEntry / ImportText to the right slot (the two query types — the
-- shared SourceQuery and Vetula's own — agree on these constructors' shapes).
queryLoad :: forall o m. Which -> Int -> H.HalogenM RState RAction Slots o m (Maybe Unit)
queryLoad w i = case w of
  Odo -> H.query _odo unit (SQ.LoadEntry i unit)
  Bal -> H.query _bal unit (SQ.LoadEntry i unit)
  Sel -> H.query _sel unit (SQ.LoadEntry i unit)
  Vet -> H.query _vet unit (Vetula.LoadEntry i unit)
  Tid -> pure Nothing

queryImport :: forall o m. Which -> String -> H.HalogenM RState RAction Slots o m (Maybe Boolean)
queryImport w txt = case w of
  Odo -> H.query _odo unit (SQ.ImportText txt identity)
  Bal -> H.query _bal unit (SQ.ImportText txt identity)
  Sel -> H.query _sel unit (SQ.ImportText txt identity)
  Vet -> H.query _vet unit (Vetula.ImportText txt identity)
  Tid -> pure Nothing

render :: forall m. MonadAff m => RState -> H.ComponentHTML RAction Slots m
render st =
  HH.div_
    [ shellBar st
    -- All four are always in the tree (hence always mounted + running); the
    -- active one is shown, the rest are display:none but keep playing. On the
    -- TIDAL tab all four are hidden but still alive (and queryable). The three
    -- machine instruments inset their own root below the bar (position:fixed
    -- top:var(--tf-bar)); the in-flow Vetula pane is padded down to clear it.
    , pane (st.which == Odo) "" (HH.slot_ _odo unit Odonus.component unit)
    , pane (st.which == Bal) "" (HH.slot_ _bal unit Balistes.component unit)
    , pane (st.which == Sel) "" (HH.slot_ _sel unit Selene.component unit)
    , pane (st.which == Vet) "padding-top:var(--tf-bar)" (HH.slot_ _vet unit Vetula.component unit)
    , if st.which == Tid then tidalView st else HH.text ""
    ]

-- A mounted-but-maybe-hidden pane. `display:none` keeps the component alive
-- (and its scheduler/MIDI running) while removing it from layout. `extra` adds
-- per-pane style (the in-flow Vetula pane pads itself below the shell bar; the
-- fixed-root machine instruments need nothing).
pane :: forall m. Boolean -> String -> H.ComponentHTML RAction Slots m -> H.ComponentHTML RAction Slots m
pane visible extra content =
  HH.div [ style ((if visible then "" else "display:none;") <> extra) ] [ content ]

-- The read-only aggregate of all four modules' source, for copy / paste into
-- Calypso or an editor.
-- The TIDAL page, two columns: the cross-instrument LIBRARY manager (browse /
-- load / export / import the Lepidoptera presets) on the left, the read-only
-- SOURCE aggregate full-length on the right (uncapped — read the whole rack).
tidalView :: forall m. RState -> H.ComponentHTML RAction Slots m
tidalView st =
  HH.div
    [ style $ "max-width:1440px;margin:calc(var(--tf-bar) + 18px) auto 40px;padding:0 20px;"
        <> "font-family:Georgia,serif;display:flex;gap:26px;align-items:flex-start" ]
    [ HH.div [ style "flex:0 0 400px;min-width:0" ] [ libraryPanel st ]
    , HH.div [ style "flex:1 1 auto;min-width:0" ] [ sourcePanel st ]
    ]

-- The library manager: each instrument's saved presets, grouped, each loadable
-- and copyable (copy = export the Lepidoptera text, e.g. into Calypso); plus a
-- paste box that imports into a chosen instrument.
libraryPanel :: forall m. RState -> H.ComponentHTML RAction Slots m
libraryPanel st =
  HH.div [ style "margin-bottom:26px" ]
    [ HH.div
        [ style "display:flex;align-items:baseline;gap:14px;margin-bottom:12px" ]
        [ HH.span
            [ style "font-size:13px;letter-spacing:0.14em;text-transform:uppercase;color:#5a564b" ]
            [ HH.text "Library — presets across the rack" ]
        , barBtn "refresh" RefreshTidal
        ]
    , if null st.library
        then HH.div [ style "color:#8a8576;font-size:12px;font-style:italic;margin-bottom:14px" ]
               [ HH.text "(refresh to gather each instrument's saved presets)" ]
        else HH.div_ (map (groupSection st) [ Odo, Bal, Sel, Vet ])
    , importBox st
    ]

groupSection :: forall m. RState -> Which -> H.ComponentHTML RAction Slots m
groupSection st w =
  let rows = filter (\r -> r.inst == w) st.library
  in if null rows then HH.text ""
     else HH.div [ style "margin-bottom:12px" ]
       ( [ HH.div
             [ style "font-size:10px;letter-spacing:0.16em;text-transform:uppercase;color:#8a7a4a;margin-bottom:5px" ]
             [ HH.text (whichName w) ]
         ] <> map entryRow rows )

entryRow :: forall m. LibRow -> H.ComponentHTML RAction Slots m
entryRow r =
  HH.div
    [ style $ "display:flex;align-items:center;gap:10px;padding:6px 10px;margin-bottom:3px;"
        <> "background:#ffffff;border:1px solid #e3dfd2;border-radius:5px" ]
    [ HH.span [ style "flex:1 1 auto;font-size:12px;color:#2a271e" ] [ HH.text r.name ]
    , barBtn "load" (LoadFromLib r.inst r.idx)
    , barBtn "copy" (CopyEntry r.text)
    ]

importBox :: forall m. RState -> H.ComponentHTML RAction Slots m
importBox st =
  HH.div
    [ style "margin-top:16px;padding:12px;background:#f3efe4;border:1px solid #e3dfd2;border-radius:6px" ]
    [ HH.div
        [ style "font-size:10px;letter-spacing:0.12em;text-transform:uppercase;color:#7a7363;margin-bottom:7px" ]
        [ HH.text "Import — paste Lepidoptera eDSL, then choose its instrument" ]
    , HH.textarea
        [ HP.value st.importText
        , HE.onValueInput SetImportText
        , HP.placeholder "balistesPattern \"…\"  ·  odonusPatch \"…\"  ·  a Selene rack  ·  a Vetula  note \"<…>\""
        , HP.spellcheck false
        , style $ "width:100%;box-sizing:border-box;min-height:84px;resize:vertical;padding:8px 10px;"
            <> "border:1px solid #cdbb96;border-radius:5px;background:#fffdf8;"
            <> "font-family:'SF Mono',Menlo,Consolas,monospace;font-size:11px;line-height:1.5;color:#2a271e" ]
    , HH.div
        [ style "display:flex;align-items:center;gap:7px;margin-top:8px" ]
        [ HH.span [ style "font-size:10px;letter-spacing:0.1em;text-transform:uppercase;color:#7a7363" ] [ HH.text "import →" ]
        , barBtn "Odonus" (ImportInto Odo)
        , barBtn "Balistes" (ImportInto Bal)
        , barBtn "Selene" (ImportInto Sel)
        , barBtn "Vetula" (ImportInto Vet)
        , if st.importMsg == "" then HH.text ""
          else HH.span [ style "font-size:11px;color:#5a4a22;margin-left:4px" ] [ HH.text st.importMsg ]
        ]
    ]

-- The read-only aggregate of all four modules' source, for copy / paste into
-- Calypso or an editor.
sourcePanel :: forall m. RState -> H.ComponentHTML RAction Slots m
sourcePanel st =
  HH.div_
    [ HH.div
        [ style "display:flex;align-items:baseline;gap:14px;margin-bottom:12px" ]
        [ HH.span
            [ style "font-size:13px;letter-spacing:0.14em;text-transform:uppercase;color:#5a564b" ]
            [ HH.text "Source — the whole playing surface" ]
        , barBtn "copy" CopyTidal
        , barBtn "refresh" RefreshTidal
        ]
    , HH.pre
        [ style $ "margin:0;padding:16px 18px;background:#ffffff;border:1px solid #e3dfd2;"
            <> "border-radius:6px;box-shadow:0 1px 4px #00000012;overflow-x:auto;"
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

-- The shared shell bar: one fixed strip across every tab — master transport
-- (left), the rack/instrument nameplate (centred), the switcher (right). It
-- reserves `--tf-bar` of height so no instrument's own top content collides
-- with it, and gives the rack one identity over both the machine and oracle
-- aesthetics underneath.
shellBar :: forall m. RState -> H.ComponentHTML RAction Slots m
shellBar st =
  HH.div
    [ style $ "position:fixed;top:0;left:0;right:0;height:var(--tf-bar);z-index:50;box-sizing:border-box;"
        <> "display:flex;align-items:center;justify-content:space-between;padding:0 12px;"
        <> "background:linear-gradient(#d4cfc0,#c2bcab);border-bottom:1px solid #00000026;"
        <> "box-shadow:0 1px 4px #00000018;font-family:Georgia,serif" ]
    [ HH.button
        [ HE.onClick \_ -> ToggleMaster
        , style $ "padding:6px 18px;border:1px solid #00000033;border-radius:6px;cursor:pointer;"
            <> "font-size:11px;letter-spacing:0.16em;text-transform:uppercase;box-shadow:0 1px 3px #00000022;"
            <> "color:" <> (if st.playing then "#fbeae7" else "#1c1a12")
            <> ";background:" <> (if st.playing then "linear-gradient(#b23b28,#9a3120)" else "linear-gradient(#c8a86a,#b8975a)") ]
        [ HH.text (if st.playing then "■ STOP" else "▶ PLAY") ]
    , HH.div
        [ style $ "position:absolute;left:50%;transform:translateX(-50%);pointer-events:none;"
            <> "font-size:11px;letter-spacing:0.22em;text-transform:uppercase;color:#4a463b" ]
        [ HH.text ("Triggerfish · " <> whichName st.which) ]
    , HH.div
        [ style $ "display:flex;gap:0;border:1px solid #00000033;border-radius:6px;overflow:hidden;"
            <> "box-shadow:0 1px 3px #00000022" ]
        [ seg "ODONUS" (st.which == Odo) (Pick Odo)
        , seg "BALISTES" (st.which == Bal) (Pick Bal)
        , seg "SELENE" (st.which == Sel) (Pick Sel)
        , seg "VETULA" (st.which == Vet) (Pick Vet)
        , seg "TIDAL" (st.which == Tid) (Pick Tid)
        ]
    ]

whichName :: Which -> String
whichName = case _ of
  Odo -> "Odonus"
  Bal -> "Balistes"
  Sel -> "Selene"
  Vet -> "Vetula"
  Tid -> "Tidal"

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
