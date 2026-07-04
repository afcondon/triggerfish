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

import Data.Array (filter, mapWithIndex, null, replicate)
import Data.Foldable (foldl, for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
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
import Triggerfish.Transport (Which(..), Mode(..), Sounding(..), soundingOf, anyArmed, allMachines)

-- One free-run tempo for the whole rack with no rig. (On the rig the forwarded
-- Link anchor overrides it.) A shell BPM control could drive this later.
freeTempo :: Number
freeTempo = 120.0

main :: Effect Unit
main = HA.runHalogenAff do
  liftEffect armAudioKeepAlive   -- keep the tab audible so background play survives
  body <- HA.awaitBody
  void $ runUI root unit body

-- `Which`, `Mode`, and the `Sounding` authority model now live in the pure
-- Triggerfish.Transport module (the MISU core — see docs/DESIGN-transport-misu.md).
-- The shell's entire transport state is `mode :: Mode` + `armed :: Set Which`;
-- each machine's `Sounding` is `soundingOf mode armed w`, pushed via SetSounding.

data RAction
  = Init | SyncTick | PollVetula | Pick Which | RefreshTidal | CopyTidal | ToggleMaster
  | SetMode Mode                -- flip the SOLO⟷ATLANTIS authority
  | ArmTab Which                -- toggle one instrument's ARM from the switcher dot
  | JumpVetula Int              -- nav strip: jump Vetula's progression to a chord (live)
  | LoadFromLib Which Int       -- A5: make a saved entry active in its instrument
  | CopyEntry String            -- copy one entry's eDSL text
  | SetImportText String
  | ImportInto Which            -- route the paste box to one instrument's library

-- One saved preset gathered from an instrument, for the cross-instrument LIBRARY
-- manager on the TIDAL page. `text` is the entry rendered to Lepidoptera eDSL
-- (the transferable form); `idx` is its position in that instrument's library.
type LibRow = { inst :: Which, idx :: Int, name :: String, text :: String }

-- The shell's ENTIRE transport state (MISU refactor): the authority `mode` plus
-- the set of armed machines. A machine's behaviour is `soundingOf mode armed w`
-- — no separate master/playing/audible/rigOn booleans that can contradict it.
-- "Master playing" is derived (`anyArmed armed`); rig-voice running is derived
-- (`soundingOf … == Rig`) and each instrument edge-detects its own transitions.
type RState =
  { which :: Which, tidalDoc :: String, freeT0 :: Number
  , library :: Array LibRow, importText :: String, importMsg :: String
  -- The authority mode + the live harmonic-context strip shown in the top nav.
  -- `harm` is polled from Vetula: voice-0's bars-per-chord dwell schedule and the
  -- current playhead (-1 = none). Rendered as a glyph visible in every pane.
  , mode :: Mode
  , harm :: { durs :: Array Int, active :: Int, chord :: String }
  -- Vetula auto-resync (ATLANTIS): the shell polls Vetula's rig payload and, when
  -- it settles on a new value, re-pushes (SetSounding Rig re-voices) — so the
  -- progression re-voices live with no manual button. `brushSent` = last value
  -- pushed; `brushPrev` = last poll's value (a one-tick settle coalesces a drag).
  , brushSent :: String
  , brushPrev :: String
  -- The armed set: the single source of truth for the switcher's per-tab dots and
  -- the master button label. Reconciled from the instruments on SyncTick (a machine
  -- can self-disarm, e.g. Vetula unloading a progression).
  , armed :: Set Which }

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
        { which: Bal, tidalDoc: "", freeT0: 0.0
        , library: [], importText: "", importMsg: ""
        , mode: Solo, harm: { durs: [], active: -1, chord: "" }
        , brushSent: "", brushPrev: ""
        , armed: Set.empty }
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
    -- One source of truth: push each machine its derived Sounding (all Silent now —
    -- nothing armed). Arm/mode changes re-derive and re-push; the instruments
    -- edge-detect their own local-mute / rig-handoff transitions.
    pushAll
  -- Master ▶/■ = arm ALL / disarm ALL: arm every machine if none is armed, else
  -- disarm every machine. The button label is `anyArmed`. pushAll re-derives each
  -- machine's Sounding (in ATLANTIS that hands off / stops rig voices too).
  ToggleMaster -> do
    a <- H.gets _.armed
    H.modify_ _ { armed = if anyArmed a then Set.empty else allMachines }
    pushAll
  -- Flip the SOLO⟷ATLANTIS authority. Re-deriving every machine's Sounding IS the
  -- whole transition: an armed rig machine goes Local⟷Rig (its SetSounding handler
  -- hands off on entering Rig, stops its voice on leaving), Selene stays Local,
  -- unarmed stay Silent. No manual handoff/hush ordering to get wrong.
  SetMode m -> do
    H.modify_ _ { mode = m }
    pushAll
  -- The switcher's per-tab play/pause dot: toggle just this machine's arm, then
  -- push its (re-derived) Sounding.
  ArmTab w -> do
    a <- H.gets _.armed
    H.modify_ _ { armed = if Set.member w a then Set.delete w a else Set.insert w a }
    pushSounding w
  -- Nav harmonic strip: jump Vetula's progression to a chord live. Playing → the
  -- ensemble advances there; stopped → the → odo feed moves, re-quantising Odonus.
  JumpVetula i -> void $ H.query _vet unit (Vetula.JumpChord i unit)
  SyncTick -> do
    t0 <- H.gets _.freeT0
    _ <- H.query _odo unit (SQ.SyncFree t0 freeTempo unit)
    _ <- H.query _bal unit (SQ.SyncFree t0 freeTempo unit)
    _ <- H.query _sel unit (SQ.SyncFree t0 freeTempo unit)
    _ <- H.query _vet unit (Vetula.SyncFree t0 freeTempo unit)
    -- OBSERVE: reconcile the armed set with the instruments' EFFECTIVE sounding.
    -- A machine can self-disarm (Vetula unloading a progression) — it reports
    -- `Silent`, so we drop it from the set. Odo/Bal/Sel only ever echo what we
    -- pushed, so they're stable. `Nothing` (query miss) leaves that machine as-is.
    o <- askSounding Odo
    b <- askSounding Bal
    s <- askSounding Sel
    v <- askSounding Vet
    st0 <- H.get
    let armed' = reconcileArmed st0.armed [ Tuple Odo o, Tuple Bal b, Tuple Sel s, Tuple Vet v ]
    when (armed' /= st0.armed) do
      H.modify_ _ { armed = armed' }
      -- Re-push any machine whose membership changed so its Sounding (and thus its
      -- rig voice) matches the reconciled truth. A Vetula self-disarm this way gets
      -- SetSounding Silent → its handler sends vetula-stop (what reconcileRig did).
      for_ [ Odo, Bal, Sel, Vet ] \w ->
        when (Set.member w armed' /= Set.member w st0.armed) (pushSounding w)
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
    -- Pull Vetula's progression + playhead for the nav harmonic-context strip.
    mharm <- H.query _vet unit (Vetula.AskHarmonic identity)
    case mharm of
      Just h -> H.modify_ _ { harm = h }
      Nothing -> pure unit
    -- Vetula auto-resync (ATLANTIS only): Vetula has no incremental rig path, so the
    -- shell diffs its payload and re-pushes on a SETTLED change (payload stable for
    -- one poll AND different from what was last sent). A drag coalesces into one push
    -- ~one tick after it stops; glitchless because the rig re-push phase-aligns.
    -- Only when the Vetula voice is actually running on the rig (armed in ATLANTIS).
    msig <- H.query _vet unit (Vetula.AskBrushSig identity)
    for_ msig \sig -> do
      st <- H.get
      -- Re-push (SetSounding Rig re-voices) only when Vetula is actually rig-
      -- authoritative — `soundingOf … Vet == Rig` already implies armed + ATLANTIS.
      when (soundingOf st.mode st.armed Vet == Rig && sig == st.brushPrev && sig /= st.brushSent) do
        _ <- H.query _vet unit (Vetula.SetSounding Rig unit)
        H.modify_ _ { brushSent = sig }
      H.modify_ _ { brushPrev = sig }

-- Push one machine its DERIVED Sounding (soundingOf mode armed). The instrument
-- edge-detects the transition itself: local-mute on leaving Local, rig handoff on
-- entering Rig, rig-stop on leaving Rig. This one call replaces the old
-- broadcastMaster / broadcastAudible / broadcastSyncToRig / broadcastStopRig /
-- reconcileRig — the shell no longer tracks a separate rig-running mirror.
pushSounding :: forall o m. MonadAff m => Which -> H.HalogenM RState RAction Slots o m Unit
pushSounding w = do
  st <- H.get
  void $ querySounding w (soundingOf st.mode st.armed w)

querySounding :: forall o m. Which -> Sounding -> H.HalogenM RState RAction Slots o m (Maybe Unit)
querySounding w s = case w of
  Odo -> H.query _odo unit (SQ.SetSounding s unit)
  Bal -> H.query _bal unit (SQ.SetSounding s unit)
  Sel -> H.query _sel unit (SQ.SetSounding s unit)
  Vet -> H.query _vet unit (Vetula.SetSounding s unit)
  Tid -> pure Nothing

-- Re-derive and push every machine's Sounding (on arm-all / mode flip / init).
pushAll :: forall o m. MonadAff m => H.HalogenM RState RAction Slots o m Unit
pushAll = for_ [ Odo, Bal, Sel, Vet ] pushSounding

-- Observe: ask one machine its EFFECTIVE sounding (Silent ⇒ not armed).
askSounding :: forall o m. Which -> H.HalogenM RState RAction Slots o m (Maybe Sounding)
askSounding w = case w of
  Odo -> H.query _odo unit (SQ.AskSounding identity)
  Bal -> H.query _bal unit (SQ.AskSounding identity)
  Sel -> H.query _sel unit (SQ.AskSounding identity)
  Vet -> H.query _vet unit (Vetula.AskSounding identity)
  Tid -> pure Nothing

-- Fold observed soundings into the armed set: `Silent` drops a machine, any other
-- sounding adds it, a query miss (`Nothing`) leaves it unchanged.
reconcileArmed :: Set Which -> Array (Tuple Which (Maybe Sounding)) -> Set Which
reconcileArmed = foldl \acc (Tuple w ms) -> case ms of
  Just Silent -> Set.delete w acc
  Just _ -> Set.insert w acc
  Nothing -> acc

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
            <> "color:" <> (if anyArmed st.armed then "#fbeae7" else "#1c1a12")
            <> ";background:" <> (if anyArmed st.armed then "linear-gradient(#b23b28,#9a3120)" else "linear-gradient(#c8a86a,#b8975a)") ]
        [ HH.text (if anyArmed st.armed then "■ STOP" else "▶ PLAY") ]
    -- The centre strip: the wordmark, the SOLO⟷ATLANTIS authority toggle, and the
    -- live harmonic-context glyph. Replaces the old `Triggerfish · <instrument>`
    -- label (the instrument is already named by the switcher on the right); the
    -- two things worth showing in EVERY pane are the mode and the progression.
    , HH.div
        [ style $ "position:absolute;left:50%;transform:translateX(-50%);"
            <> "display:flex;align-items:center;gap:16px" ]
        [ HH.span
            [ style "font-size:11px;letter-spacing:0.22em;text-transform:uppercase;color:#4a463b" ]
            [ HH.text "Triggerfish" ]
        , modeToggle st
        , harmStrip st
        ]
    , HH.div
        [ style $ "display:flex;gap:0;border:1px solid #00000033;border-radius:6px;overflow:hidden;"
            <> "box-shadow:0 1px 3px #00000022" ]
        [ armSeg st Odo "ODONUS"
        , armSeg st Bal "BALISTES"
        , armSeg st Sel "SELENE"
        , armSeg st Vet "VETULA"
        -- TIDAL is a read-only aggregate, not an instrument — no play/pause dot.
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

-- An instrument tab: a play/pause dot + the name, as two independent click targets
-- (like a browser tab's close button). The dot toggles that instrument's ARM
-- WITHOUT switching panes (▶ = armed-off, click to start; ❚❚ = armed, click to
-- stop); the name switches to the pane. The dot glows when armed.
armSeg :: forall m. RState -> Which -> String -> H.ComponentHTML RAction Slots m
armSeg st w label =
  let active = st.which == w
      isArmed = Set.member w st.armed
      bg = if active then "linear-gradient(#c8a86a,#b8975a)" else "linear-gradient(#e9e5d9,#dcd8c9)"
  in HH.div
      [ style ("display:flex;align-items:center;background:" <> bg) ]
      [ HH.span
          [ HE.onClick \_ -> ArmTab w
          , style $ "padding:6px 3px 6px 9px;cursor:pointer;font-size:9px;line-height:1;"
              <> "color:" <> (if isArmed then "#2f7a3f" else "#9a9488") ]
          [ HH.text (if isArmed then "❚❚" else "▶") ]
      , HH.span
          [ HE.onClick \_ -> Pick w
          , style $ "padding:6px 13px 6px 5px;cursor:pointer;font-size:11px;letter-spacing:0.12em;"
              <> "text-transform:uppercase;color:" <> (if active then "#1c1a12" else "#5a564b") ]
          [ HH.text label ]
      ]

-- The SOLO⟷ATLANTIS authority toggle. Each mode carries its own colour so the
-- active authority reads at a glance: SOLO warm/gold (a standalone instrument),
-- ATLANTIS deep sea-blue (the rig is the sound). Clicking a segment sets that mode.
modeToggle :: forall m. RState -> H.ComponentHTML RAction Slots m
modeToggle st =
  HH.div
    [ style $ "display:flex;border:1px solid #00000033;border-radius:5px;overflow:hidden;"
        <> "box-shadow:0 1px 2px #00000022" ]
    [ modeSeg "SOLO" (st.mode == Solo) "#1c1a12" "linear-gradient(#c8a86a,#b8975a)" (SetMode Solo)
    , modeSeg "ATLANTIS" (st.mode == Atlantis) "#eaf3fa" "linear-gradient(#3a6b8a,#2d5670)" (SetMode Atlantis)
    ]

modeSeg :: forall m. String -> Boolean -> String -> String -> RAction -> H.ComponentHTML RAction Slots m
modeSeg label active onColor onBg act =
  HH.button
    [ HE.onClick \_ -> act
    , style $ "padding:5px 13px;border:0;cursor:pointer;font-size:10px;letter-spacing:0.16em;"
        <> "text-transform:uppercase;color:" <> (if active then onColor else "#5a564b")
        <> ";background:" <> (if active then onBg else "linear-gradient(#e9e5d9,#dcd8c9)") ]
    [ HH.text label ]

-- The live harmonic-context glyph, polled from Vetula: one mark per chord in the
-- performance progression — ● the chord the playhead is on, ○ the others — with a
-- trailing ‑ per extra bar of dwell and a · for a skipped chord. Visible in every
-- pane, so the progression is always in view (the palette animation, surfaced).
harmStrip :: forall m. RState -> H.ComponentHTML RAction Slots m
harmStrip st =
  HH.div
    [ style "display:flex;align-items:baseline;gap:12px" ]
    -- The progress row: where the playhead is, as PER-CHORD click targets — click a
    -- chord to jump Vetula's progression there live (advance the playhead when
    -- playing; move the Odonus feed when stopped). See RAction.JumpVetula.
    [ HH.div
        [ style $ "display:flex;align-items:baseline;font-family:'SF Mono',Menlo,Consolas,monospace;"
            <> "font-size:13px;line-height:1;letter-spacing:0.14em" ]
        (if null st.harm.durs
           then [ HH.span [ style "color:#4a463b" ] [ HH.text "—" ] ]
           else mapWithIndex (harmChip st.harm.active) st.harm.durs)
    -- The content row: the notes of the chord under the playhead (bass-up), so the
    -- strip shows both WHERE we are and WHAT is sounding — the progression view's
    -- pitch content, time-multiplexed through the playhead.
    , HH.span
        [ style $ "font-family:Georgia,serif;font-size:12px;letter-spacing:0.1em;"
            <> "color:#2d5670;min-width:96px" ]
        [ HH.text st.harm.chord ]
    ]

-- One chord in the nav strip: its glyph (● at the playhead, ○ elsewhere) plus its
-- dwell dashes, as a click target that jumps the progression to that chord. A
-- skipped chord (dwell 0) is a dim, non-interactive dot.
harmChip :: forall m. Int -> Int -> Int -> H.ComponentHTML RAction Slots m
harmChip active i d =
  if d <= 0
    then HH.span [ style "color:#b0aa9c" ] [ HH.text "·" ]
    else HH.span
      [ HE.onClick \_ -> JumpVetula i
      , style $ "cursor:pointer;color:" <> (if i == active then "#b23b28" else "#4a463b") ]
      [ HH.text ((if i == active then "●" else "○") <> joinWith "" (replicate (d - 1) "‑")) ]

style :: forall r i. String -> HP.IProp r i
style = HP.attr (H.AttrName "style")
