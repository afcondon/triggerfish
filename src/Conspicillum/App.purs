-- | **Conspicillum, as a Triggerfish page.** One scene, held as reef's types (a
-- | preset's line, query and knobs), and three views of it:
-- |
-- | - **the module**: the ring, the cloud, the strip with its waveform, the
-- |   loupe, the sentence and four knobs; an instrument.
-- | - **the laboratory**: every control, generated from reef's catalogue.
-- | - **the library**: every preset, to categorise, tag and collect.
-- |
-- | The module and the laboratory edit the same scene, so what is turned in
-- | one is heard and drawn in the other. The scene the rig is sent, and what
-- | the ring draws, are both made from it by reef.
-- |
-- | It runs in the machines' shell (`Triggerfish.Standalone`, since
-- | 2026-10-29; it was a page of its own before): the shell's bar carries the
-- | views, Play and Stop, Panic and ⓘ; its drawer on the left lists the
-- | presets (`AskBrowser`), and its drawer on the right is Limulus. What is
-- | Conspicillum's own stays here: the scene, the shelf and library in
-- | Amphora, and the stage entry the rig keeps for it (`Conspicillum.Stage`),
-- | which carries the preset in hand so a reloaded page comes back to it.
-- |
-- | Presets come from reef (the factory) or from the shelf (kept in Amphora).
-- | A preset names its material, but the material can be held while presets
-- | change under it, since most of a preset is a treatment that would work on
-- | anything.
module Conspicillum.App (Output(..), component) where

import Prelude

import Binnacle (Binnacle)
import Binnacle as Binnacle
import Binnacle.Clock as Clock
import Binnacle.Transport as Transport
import Conspicillum.Corpora (Corpora, SampleSet, load, materialFor, sampleChoice, setNamed, withMaterial)
import Conspicillum.Frame (frames)
import Conspicillum.Library (Library)
import Conspicillum.Library as Library
import Conspicillum.Plan (Grain, Plan, emptyPlan, planCycle, sceneOf)
import Conspicillum.Shelf (Kept)
import Conspicillum.Shelf as Shelf
import Conspicillum.Stage (Held)
import Conspicillum.Stage as Stage
import Conspicillum.View.Circle (Dot, circle, live, spawn)
import Conspicillum.View.Knob (knob)
import Conspicillum.View.Laboratory (laboratory)
import Conspicillum.View.Library as LibraryView
import Conspicillum.View.Material (material)
import Conspicillum.View.Sentence (sentence)
import Conspicillum.View.Strip (loupe, loupeBackdrop, loupeSource, loupeView, strip, stripBackdrop)
import Conspicillum.View.Svg as Svg
import Control.Alt ((<|>))
import Data.Array (catMaybes, drop, elem, filter, find, findIndex, index, length, mapMaybe, mapWithIndex, nub, null, snoc, take, (..))
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Foldable (foldl, for_)
import Data.Int (floor, fromString, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Number (abs)
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event as HQE
import Halogen.Subscription as HS
import Reef.Conspicillum.Cloud (Spec)
import Reef.Conspicillum.Decimal (fixed)
import Reef.Conspicillum.Display (Material(..), materialName, segments)
import Reef.Conspicillum.Notation (Line, parse, print)
import Reef.Conspicillum.Parameter (Parameter(..), SendBus(..), denormalise, describe, format, normalise, read, set)
import Reef.Conspicillum.Preset (Preset, bankName)
import Reef.Conspicillum.Presets (presets)
import Reef.Conspicillum.Protocol (encodeScene)
import Reef.Conspicillum.Sentence (ruleRows, sentences)
import Triggerfish.Bar (Bar, TabLook(..))
import Triggerfish.Browser (Browser, Item)
import Triggerfish.Browser as Browser
import Triggerfish.SourceQuery as SQ
import Triggerfish.Transport (Sounding(..))
import Type.Proxy (Proxy(..))
import Unsafe.Reference (unsafeRefEq)
import Web.Event.Event as Event
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Location as Location
import Web.HTML.Window as Window
import Web.Storage.Storage as Storage
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

-- ── what the shell hears ─────────────────────────────────────────────────────

-- | `Armed`: the page started or stopped playing by itself (a line written in
-- | Limulus, another page, a reload onto a playing rig), which the shell's
-- | transport follows. `WantsLimulus`: "→ Limulus" was pressed. `Changed`:
-- | the presets or the view changed, so the drawer and the bar are asked for
-- | again.
data Output
  = Armed Boolean
  | WantsLimulus
  | Changed

-- ── state ────────────────────────────────────────────────────────────────────

data Tab = ModuleTab | LaboratoryTab | LibraryTab

derive instance eqTab :: Eq Tab

type Slots = ( library :: H.Slot (Const Void) LibraryView.Output Unit )

-- | Where the preset in hand came from: reef's list, or the shelf. A scene
-- | written as a line has no origin.
data Origin = Factory Int | Shelved String

derive instance eqOrigin :: Eq Origin

-- | Dragging a number, in the sentence (across) or on a knob (up).
data Axis = Across | Up

derive instance eqAxis :: Eq Axis

type Drag = { parameter :: Parameter, axis :: Axis, x :: Number, y :: Number, from :: Number }

type State =
  { tab :: Tab
  , corpora :: Corpora
  , presets :: Array Preset
  , shelf :: Array Kept
  , shelfProblem :: Maybe String
  -- | How presets are organised: categories, tags, collections, and which
  -- | collection the drawer shows. Kept in Amphora; `libraryHash` is the
  -- | version this page last read or wrote.
  , library :: Library
  , libraryHash :: Maybe String
  , problem :: Maybe String
  , origin :: Maybe Origin
  -- | The scene as recalled, for "edited", revert, and a knob's double-click.
  , original :: Maybe Preset
  , current :: Maybe Preset
  -- | Keep the material when the preset changes.
  , held :: Boolean
  , rig :: Maybe Binnacle
  -- | The rig's host, as this page was reached: the store and the rig's
  -- | socket are on the same machine.
  , host :: String
  , connected :: Boolean
  -- | Subscribed to the stage on this connection. Not before the corpora are
  -- | loaded, so what the stage holds lands after the page's own first recall.
  , staged :: Boolean
  , playing :: Boolean
  -- | A scene changed while playing and not yet sent. Sent on the next frame
  -- | at least 120 ms after the last, so a drag is not a flood.
  , unsent :: Boolean
  , sentAt :: Number
  , tempo :: Number
  , locked :: Boolean
  , now :: Number
  , phase :: Number
  , plan :: Plan
  -- | The most lanes this scene has needed, so the strip does not twitch.
  , lanes :: Int
  , dots :: Array Dot
  -- | The segment in the loupe: the one last read.
  , loupeAt :: Int
  , drag :: Maybe Drag
  -- | While dragging, the sentence keeps the shape it had when the drag began,
  -- | so the number under the pointer cannot vanish from under it.
  , shape :: Maybe Line
  , highlighted :: Maybe Parameter
  -- | The line as the stage holds it (`conspicillum/line`), as far as this
  -- | page knows: what it last published or was sent. The page publishes its
  -- | own line when the two differ, not while a number is being dragged.
  , stagedLine :: Maybe String
  , linePublishedAt :: Number
  -- | A kept preset a link asked for, recalled once the shelf arrives.
  , wantedKept :: Maybe String
  -- | The welcome card: shown on a first visit, and from ⓘ.
  , welcome :: Boolean
  }

data Action
  = Initialise
  | Loaded (Either String Corpora)
  | ShelfLoaded (Either String (Array Kept))
  | LibraryLoaded (Either String { library :: Library, hash :: Maybe String })
  | FromLibrary LibraryView.Output
  | Frame Number
  | FromRig String
  | Recall Origin
  | Step Int
  | Revert
  | ChooseSet String
  | ChooseSample (Maybe Int)
  | Hold Boolean
  | Grab Parameter Axis Event.Event
  | Move MouseEvent
  | Release
  | Hover (Maybe Parameter)
  | Nudge Parameter KeyboardEvent
  | Reset Parameter
  | SetParameter Parameter Number
  | EditSpec (Spec -> Spec)
  | Pin Parameter
  | Key KeyboardEvent
  | CloseWelcome
  | Swallow Event.Event
  | NoOp

component :: H.Component SQ.Query Unit Output Aff
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction, handleQuery = handleQuery, initialize = Just Initialise }
  }

initialState :: Unit -> State
initialState _ =
  { tab: ModuleTab
  , corpora: []
  , presets: case presets of
      Right ps -> ps
      Left _ -> []
  , shelf: []
  , shelfProblem: Nothing
  , library: Library.empty
  , libraryHash: Nothing
  , problem: case presets of
      Left p -> Just ("A preset does not resolve: " <> p)
      Right _ -> Nothing
  , origin: Nothing
  , original: Nothing
  , current: Nothing
  , held: false
  , rig: Nothing
  , host: "localhost"
  , connected: false
  , staged: false
  , playing: false
  , unsent: false
  , sentAt: 0.0
  , tempo: 120.0
  , locked: false
  , now: 0.0
  , phase: 0.0
  , plan: emptyPlan
  , lanes: 1
  , dots: []
  , loupeAt: 0
  , drag: Nothing
  , shape: Nothing
  , highlighted: Nothing
  , stagedLine: Nothing
  , linePublishedAt: 0.0
  , wantedKept: Nothing
  , welcome: false
  }

-- ── what the state implies ───────────────────────────────────────────────────

storageKey :: String
storageKey = "conspicillum-at"

setOf :: State -> Preset -> Maybe SampleSet
setOf st p = setNamed st.corpora p.line.set

-- | The scene in hand, with the set it plays and what that set is as material.
type Working = { preset :: Preset, set :: SampleSet, material :: Material }

working :: State -> Maybe Working
working st = do
  preset <- st.current
  set <- setOf st preset
  pure { preset, set, material: materialFor set preset.line }

-- | Every preset: the shelf, then the factory. A preset's place in this list
-- | is its slot in the drawer.
entries :: State -> Array (Tuple Origin Preset)
entries st =
  (st.shelf <#> \k -> Tuple (Shelved k.hash) k.preset)
    <> mapWithIndex (\i p -> Tuple (Factory i) p) st.presets

-- | The presets the drawer lists and the arrows step through: the collection
-- | chosen in the library, or every preset.
onModule :: State -> Array (Tuple Origin Preset)
onModule st = case st.library.onModule >>= Library.collectionNamed st.library of
  Nothing -> entries st
  Just c -> mapMaybe (\ref -> find (\(Tuple o p) -> reference o p == ref) (entries st)) c.presets

-- | How the library refers to a preset: a factory one by its name in reef, a
-- | kept one by its hash.
reference :: Origin -> Preset -> String
reference origin p = case origin of
  Factory _ -> "reef:" <> p.name
  Shelved h -> "kept:" <> h

presetAt :: State -> Origin -> Maybe Preset
presetAt st = case _ of
  Factory i -> index st.presets i
  Shelved h -> _.preset <$> find (\k -> k.hash == h) st.shelf

-- | A preset plays only if its set is on this machine (or before the corpora
-- | have arrived, when nothing is known yet).
playable :: State -> Preset -> Boolean
playable st p = null st.corpora || isJust (setOf st p)

-- | The origin as a link: `#12` for the twelfth factory preset, `#k…` for a
-- | kept one.
token :: Origin -> String
token = case _ of
  Factory i -> show (i + 1)
  Shelved h -> "k" <> h

fromToken :: String -> Maybe Origin
fromToken t = case String.stripPrefix (String.Pattern "k") t of
  Just h | h /= "" -> Just (Shelved h)
  _ -> fromString t <#> \n -> Factory (n - 1)

replan :: State -> State
replan st = case working st of
  Nothing -> st { plan = emptyPlan }
  Just w ->
    let
      scene = sceneOf { set: w.set, line: w.preset.line, query: w.preset.query, tempo: st.tempo }
      plan = planCycle { scene, material: w.material, tempo: st.tempo } (max 0 st.plan.cycle)
    in
      st { plan = plan, lanes = max st.lanes plan.lanes }

sceneText :: State -> Maybe String
sceneText st = working st <#> \w ->
  encodeScene (sceneOf { set: w.set, line: w.preset.line, query: w.preset.query, tempo: st.tempo })

-- | The preset in hand, as the stage keeps it for the other pages.
heldOf :: State -> Maybe Held
heldOf st = st.current <#> \c ->
  { origin: token <$> st.origin, original: Shelf.stored <$> st.original, current: Shelf.stored c, held: st.held }

-- | What the preset in hand was loaded from in Amphora: a kept one's hash.
baseOf :: Maybe Origin -> Maybe String
baseOf = case _ of
  Just (Shelved h) -> Just h
  _ -> Nothing

edited :: State -> Boolean
edited st = case st.original, st.current of
  Just o, Just c -> print o.line /= print c.line || (o.knobs <#> _.parameter) /= (c.knobs <#> _.parameter)
  _, _ -> false

-- | The scene as a line of Tidal++, as Limulus writes it.
lineOf :: State -> Maybe String
lineOf st = st.current <#> \c -> "conspicillum $ " <> print c.line

-- ── for the shell: the drawer and the bar ───────────────────────────────────

-- | The drawer: the collection chosen in the library, in its order; or the
-- | kept presets, then the factory's by category.
browserOf :: State -> Browser
browserOf st =
  { title: "Presets"
  , modes: false
  , keep: "keep"
  , keepTip: "keep the scene as a preset of your own, named by the time until you rename it"
  , notice: ""
  , rows
  }
  where
  slotted = mapWithIndex (\i (Tuple o p) -> { slot: i, origin: o, preset: p }) (entries st)
  find' o = find (\e -> e.origin == o) slotted
  rows = case st.library.onModule >>= Library.collectionNamed st.library of
    Just c -> catMaybes (onModule st <#> \(Tuple o _) -> find' o <#> row c.name)
    Nothing -> mapMaybe (\e -> if playable st e.preset then Just (row (sectionOf e) e) else Nothing) slotted
  sectionOf e = case e.origin of
    Shelved _ -> "Kept"
    Factory _ -> Library.categoryOf st.library (reference e.origin e.preset) (bankName e.preset.bank)
  row :: String -> { slot :: Int, origin :: Origin, preset :: Preset } -> Item
  row section e =
    let
      kept = case e.origin of
        Shelved _ -> true
        Factory _ -> false
    in
      { slot: e.slot
      , name: e.preset.name
      , icons: []
      , tag: ""
      , current: st.origin == Just e.origin
      , section
      , builtin: not kept
      , drag: ""
      , hint: e.preset.about <> (if kept then " (double-click to rename)" else "")
      , actions: if kept then [ "delete" ] else []
      }

-- | The bar: the three views, "→ Limulus", and ⓘ.
barOf :: State -> Bar
barOf st =
  { tabs:
      [ tab ModuleTab "module" "The instrument: the ring, the material, the sentence and four knobs"
      , tab LaboratoryTab "laboratory" "Every control, section by section; a pin puts one on the module as a knob"
      , tab LibraryTab "library" "Every preset, to search, categorise, tag and collect"
      ]
  , marks: ""
  , icons: []
  , rebusTip: ""
  , chips:
      [ { id: "limulus", label: "→ Limulus", tip: "Put this scene in Limulus as a line, conspicillum $ …, and play it from there", icons: [], active: false, attention: false } ]
  , pickers: []
  , help: "What Conspicillum is, and how to play it"
  , helpPulse: false
  }
  where
  tab t label tip = { id: tabId t, label, active: st.tab == t, enabled: true, look: Ink, tip, accepts: [] }

tabId :: Tab -> String
tabId = case _ of
  ModuleTab -> "module"
  LaboratoryTab -> "laboratory"
  LibraryTab -> "library"

-- ── rendering ────────────────────────────────────────────────────────────────

cls :: forall r i. String -> HP.IProp (class :: String | r) i
cls = HP.class_ <<< HH.ClassName

render :: State -> H.ComponentHTML Action Slots Aff
render st =
  HH.div [ cls "consp" ]
    ( [ case st.tab of
          LibraryTab -> HH.div [ cls "instrument wide" ] [ maybe note (const (libraryPanel st)) (working st) ]
          _ -> HH.div [ cls "instrument" ] case working st of
            Nothing -> [ note ]
            Just w -> case st.tab of
              LaboratoryTab -> [ laboratoryPanel st w ]
              _ -> modulePanel st w
      ]
        <> (if st.welcome then [ welcomeCard ] else [])
    )
  where
  note = HH.p [ cls "note" ] [ HH.text (fromMaybe "Reading the corpora…" st.problem) ]

-- | What Conspicillum is, in one card: on a first visit, and from ⓘ.
welcomeCard :: forall w. HH.HTML w Action
welcomeCard =
  HH.div [ cls "welcome", HE.onClick \_ -> CloseWelcome ]
    [ HH.div [ cls "card", HE.handler (Event.EventType "click") Swallow ]
        [ HH.div [ cls "eyebrow" ] [ HH.text "Conspicillum" ]
        , HH.h2_ [ HH.text "A grain cloud over a set of samples, played as a pattern." ]
        , HH.dl_
            [ row "the ring" "one bar, clockwise from the top. Each grain is a wedge, coloured by where in the sound it reads; inside, the cloud: each grain as it plays, by pan and speed"
            , row "the strip" "the material: its colours, its waveform, and the grains' read spans under it. Below it, the loupe: the part being read, close up"
            , row "the sentence" "the scene in words. Drag a number, or turn it with the arrow keys; the four marked ¹ ² ³ ⁴ are the knobs"
            , row "presets" "in the drawer on the left (b): the factory's and your own. ← and → step through them; keep makes one of your own"
            , row "laboratory" "every control, section by section; a pin puts one on the module as a knob"
            , row "library" "every preset, to categorise, tag and gather into collections; the drawer shows the one you choose"
            , row "→ Limulus" "the scene as a line, conspicillum $ …, to play and change from code"
            ]
        , HH.div [ cls "foot" ]
            [ HH.span_ [ HH.text "It plays on the Atlantis rig, through SuperDirt. The rest is under ⓘ, top right." ]
            , HH.button [ HE.onClick \_ -> CloseWelcome ] [ HH.text "start" ]
            ]
        ]
    ]
  where
  row term what = HH.div_ [ HH.dt_ [ HH.text term ], HH.dd_ [ HH.text what ] ]

-- ── what is drawn once per change, not once per frame ───────────────────────
--
-- The page redraws every animation frame, for the hand, the cloud and the read
-- head. These parts do not move with the music, so they are memoized: redrawn
-- only when what they show has changed, compared by reference, which is what
-- an unchanged part of the state keeps.

type ChooserInput = { corpora :: Corpora, line :: Line, held :: Boolean }

chooserView :: forall m. ChooserInput -> H.ComponentHTML Action Slots m
chooserView = HH.memoized same \r -> case setNamed r.corpora r.line.set of
  Nothing -> HH.text ""
  Just set -> material
    { corpora: r.corpora
    , set
    , sample: r.line.n
    , held: r.held
    , chooseSet: ChooseSet
    , chooseSample: ChooseSample
    , hold: Hold
    }
  where
  same a b = unsafeRefEq a.corpora b.corpora && unsafeRefEq a.line b.line && a.held == b.held

type BackdropInput = { set :: SampleSet, line :: Line, segment :: Int }

sameBackdrop :: BackdropInput -> BackdropInput -> Boolean
sameBackdrop a b = unsafeRefEq a.set b.set && unsafeRefEq a.line b.line && a.segment == b.segment

stripBackdropView :: forall m. BackdropInput -> H.ComponentHTML Action Slots m
stripBackdropView = HH.memoized sameBackdrop \r -> Svg.element "g" [] (stripBackdrop (materialFor r.set r.line) r.set)

loupeBackdropView :: forall m. BackdropInput -> H.ComponentHTML Action Slots m
loupeBackdropView = HH.memoized sameBackdrop \r ->
  Svg.element "g" [] (loupeBackdrop { material: materialFor r.set r.line, set: r.set, spec: r.line.spec, segment: r.segment })

type SentenceInput = { preset :: Preset, set :: SampleSet, shape :: Maybe Line, highlighted :: Maybe Parameter }

sentenceView :: forall m. SentenceInput -> H.ComponentHTML Action Slots m
sentenceView = HH.memoized same \r ->
  let
    knobs = take 4 r.preset.knobs <#> _.parameter
  in
    sentence
      { said: sentences { line: fromMaybe r.preset.line r.shape, material: materialFor r.set r.preset.line, knobs }
      , rows: ruleRows r.preset.line.spec.rules
      , spec: r.preset.line.spec
      , knobs
      , highlighted: r.highlighted
      , problem: Nothing
      , grab: \p e -> Grab p Across e
      , hover: Hover
      , nudge: Nudge
      }
  where
  same a b = unsafeRefEq a.preset b.preset && unsafeRefEq a.set b.set && unsafeRefEq a.shape b.shape && a.highlighted == b.highlighted

type LaboratoryInput = { preset :: Preset, set :: SampleSet, tempo :: Number }

laboratoryView :: forall m. LaboratoryInput -> H.ComponentHTML Action Slots m
laboratoryView = HH.memoized same \r ->
  laboratory
    { editSpec: EditSpec, setParameter: SetParameter, pin: Pin, nothing: NoOp }
    { preset: r.preset
    , scene: encodeScene (sceneOf { set: r.set, line: r.preset.line, query: r.preset.query, tempo: r.tempo })
    }
  where
  same a b = unsafeRefEq a.preset b.preset && unsafeRefEq a.set b.set && a.tempo == b.tempo

-- ── the module ───────────────────────────────────────────────────────────────

modulePanel :: forall m. State -> Working -> Array (H.ComponentHTML Action Slots m)
modulePanel st w =
  let
    spec = w.preset.line.spec
    knobs = take 4 w.preset.knobs
  in
    [ HH.section [ cls "panel" ]
        [ HH.i [ cls "screw tl" ] [], HH.i [ cls "screw tr" ] [], HH.i [ cls "screw bl" ] [], HH.i [ cls "screw br" ] []
        , panelTop st w
        , HH.div [ cls "pbody" ]
            [ HH.div_
                [ circle { plan: st.plan, phase: st.phase, playing: st.playing, dots: st.dots, now: st.now }
                , HH.div [ cls "key" ]
                    [ HH.span_ [ HH.span [ cls "sw fwd" ] [], HH.text "forwards" ]
                    , HH.span_ [ HH.span [ cls "sw back" ] [], HH.text "backwards" ]
                    , HH.span_ [ HH.text "size is loudness" ]
                    , HH.span_ [ HH.text "colour is where it reads" ]
                    ]
                ]
            -- the knobs under the sentence that names them, beside the ring,
            -- so the whole face fits a laptop's screen
            , HH.div [ cls "pright" ]
                [ presetHead st w
                , sentenceView { preset: w.preset, set: w.set, shape: st.shape, highlighted: st.highlighted }
                , HH.div [ cls "pbottom" ]
                    ( (mapWithIndex (knobView st spec) knobs)
                        <> [ HH.span [ cls "sp" ] [] ]
                        <>
                          [ HH.div [ cls "jacks" ]
                              [ jack "OUT" (if st.playing then Just 0.3 else Nothing)
                              , jack "A" (Just (read (SendLevel SendA) spec))
                              , jack "B" (Just (read (SendLevel SendB) spec))
                              ]
                          ]
                    )
                ]
            ]
        , HH.div [ cls "stripwrap" ] ([ chooser st w ] <> soundView st w)
        ]
    ]

panelTop :: forall m. State -> Working -> H.ComponentHTML Action Slots m
panelTop st w =
  HH.div [ cls "ptop" ]
    [ HH.span [ cls "brand" ] [ HH.text "CONSPICILLUM" ]
    , HH.span [ cls "bankname" ]
        [ HH.text case st.origin of
            Just (Factory i) -> bankName w.preset.bank <> " · " <> show (i + 1) <> "/" <> show (length st.presets)
            Just (Shelved _) -> "kept"
            Nothing -> if w.preset.name == "From the line" then "from the line" else "not kept"
        ]
    , HH.span [ cls "sp" ] []
    , HH.span [ cls ("lamp" <> if st.connected then " on" else ""), HP.title (if st.connected then "the rig is there" else "no rig: retrying") ] []
    , HH.span [ cls "tempo" ] [ HH.text (cycleNote st w.material) ]
    ]

-- | The preset's name, what it is, and whether it has been changed, with the
-- | way back. Keeping, renaming and deleting are the drawer's.
presetHead :: forall m. State -> Working -> H.ComponentHTML Action Slots m
presetHead st w =
  HH.div_
    [ HH.div [ cls "display" ]
        [ HH.button [ HP.title "previous preset (←)", HE.onClick \_ -> Step (-1) ] [ HH.text "‹" ]
        , HH.div [ cls "pname" ] [ HH.text w.preset.name ]
        , HH.button [ HP.title "next preset (→)", HE.onClick \_ -> Step 1 ] [ HH.text "›" ]
        ]
    , HH.div [ cls "blurb" ] [ HH.text w.preset.about ]
    , HH.div [ cls "edited" ]
        if edited st then [ HH.text "edited", HH.button [ HE.onClick \_ -> Revert ] [ HH.text "revert" ] ] else []
    ]

chooser :: forall m. State -> Working -> H.ComponentHTML Action Slots m
chooser st w = chooserView { corpora: st.corpora, line: w.preset.line, held: st.held }

-- | The strip, the waveform and the loupe: what the sound is, and where in it
-- | the grains read.
soundView :: forall m. State -> Working -> Array (H.ComponentHTML Action Slots m)
soundView st w =
  let
    spec = w.preset.line.spec
  in
    [ HH.div [ cls "striplab" ]
        [ HH.span_ [ HH.text (materialName w.material) ]
        , HH.b_ [ HH.text w.preset.line.set ]
        , HH.span [ cls "sp" ] []
        , HH.span_ [ HH.text (materialNote w) ]
        ]
    , strip
        { material: w.material, plan: st.plan, phase: st.phase, playing: st.playing, lanes: st.lanes
        , backdrop: stripBackdropView { set: w.set, line: w.preset.line, segment: 0 }
        }
    , HH.div [ cls "striplab loupelab" ]
        [ HH.span_ [ HH.text "the loupe" ]
        , HH.b_ [ HH.text (loupeNote st w) ]
        , HH.span [ cls "sp" ] []
        , HH.span_ [ HH.text (if spec.cloud.follow == 0.0 then "shaded: where a grain may start" else "") ]
        ]
    , loupe
        { material: w.material, set: w.set, plan: st.plan, phase: st.phase, spec, segment: st.loupeAt
        , backdrop: loupeBackdropView { set: w.set, line: w.preset.line, segment: st.loupeAt }
        }
    ]

knobView :: forall m. State -> Spec -> Int -> { parameter :: Parameter, label :: String } -> H.ComponentHTML Action Slots m
knobView st spec i k =
  HH.div
    [ cls ("knob" <> if st.highlighted == Just k.parameter then " hl" else "")
    , HP.title "drag up or down; double-click for the preset's value"
    , HE.handler (Event.EventType "pointerdown") (Grab k.parameter Up)
    , HE.onDoubleClick \_ -> Reset k.parameter
    , HE.onMouseEnter \_ -> Hover (Just k.parameter)
    , HE.onMouseLeave \_ -> Hover Nothing
    ]
    [ knob (normalise k.parameter (read k.parameter spec))
    , HH.div [ cls "klab" ] [ HH.text k.label, HH.sup_ [ HH.text (show (i + 1)) ] ]
    , HH.div [ cls "kval" ] [ HH.text (format k.parameter (read k.parameter spec)) ]
    ]

jack :: forall m. String -> Maybe Number -> H.ComponentHTML Action Slots m
jack name level =
  HH.div [ cls "jack" ]
    [ HH.i (maybe [] (\l -> [ HP.style ("box-shadow:0 0 0 " <> fixed 1 (l * 3.0) <> "px var(--accent-soft)") ]) level) []
    , HH.text name
    ]

-- ── the laboratory ───────────────────────────────────────────────────────────

laboratoryPanel :: forall m. State -> Working -> H.ComponentHTML Action Slots m
laboratoryPanel st w =
  HH.div_
    [ HH.section [ cls "panel labtop" ]
        ( [ HH.div [ cls "labtitle" ]
              [ HH.div [ cls "grow" ] [ presetHead st w ]
              , HH.div [ cls "labtransport" ] [ HH.span [ cls "tempo" ] [ HH.text (cycleNote st w.material) ] ]
              ]
          , chooser st w
          ]
            <> soundView st w
        )
    , laboratoryView { preset: w.preset, set: w.set, tempo: st.tempo }
    ]

-- ── the library ────────────────────────────────────────────────────────────

libraryPanel :: State -> H.ComponentHTML Action Slots Aff
libraryPanel st =
  HH.slot (Proxy :: _ "library") unit LibraryView.component
    { rows: entries st <#> \(Tuple o p) ->
        { ref: reference o p
        , name: p.name
        , about: p.about
        , bank: bankName p.bank
        , set: p.line.set
        , kept: case o of
            Shelved _ -> true
            Factory _ -> false
        , playable: isJust (setOf st p)
        }
    , library: st.library
    , current: Tuple <$> st.origin <*> st.current <#> \(Tuple o p) -> reference o p
    }
    FromLibrary

-- ── readouts ─────────────────────────────────────────────────────────────────

cycleNote :: State -> Material -> String
cycleNote st m =
  let
    bars = case m of
      WholeTape t | t.bars > 1 -> t.bars
      HitsAsBars keys -> length keys
      _ -> 0
    c = st.plan.cycle
  in
    "cycle " <> show c
      <> (if bars > 0 then " · bar " <> show (((c `mod` bars) + bars) `mod` bars + 1) <> "/" <> show bars else "")
      <> " · " <> fixed 1 st.tempo <> " bpm" <> (if st.locked then "" else " (free)")

materialNote :: Working -> String
materialNote w = case w.material of
  HitsAsBars keys -> show (length keys) <> " bars"
  WholeTape t ->
    (if t.bars > 1 then show t.bars <> " bars" else "one bar")
      <> maybe "" (\b -> " at " <> fixed 0 b <> " bpm") t.bpm
      <> " · " <> fixed 2 t.sample.seconds <> " s"
  OneSample s -> fixed 2 s.seconds <> " s"
  ManySamples ss -> show (length ss) <> " samples"

loupeNote :: State -> Working -> String
loupeNote st w =
  let
    input = { material: w.material, set: w.set, spec: w.preset.line.spec, segment: st.loupeAt }
    { source, seconds } = loupeSource input
    view = loupeView input
    shown = seconds * (view.to - view.from)
    what = case w.material of
      WholeTape t
        | t.bars > 1 -> "bar " <> show (st.loupeAt + 1) <> " of " <> show t.bars
        | otherwise -> "the bar"
      HitsAsBars _ -> "hit " <> show source.sample <> ", bar " <> show (st.loupeAt + 1) <> " of " <> show (length (segments w.material))
      _ -> "sample " <> show source.sample
  in
    what <> " · " <> (if view.to - view.from < 0.999 then duration shown <> " of " else "") <> duration seconds
  where
  duration s = if s < 1.0 then fixed 0 (s * 1000.0) <> " ms" else fixed 2 s <> " s"

-- ── the shell's queries ──────────────────────────────────────────────────────

type M = H.HalogenM State Action Slots Output Aff

handleQuery :: forall a. SQ.Query a -> M (Maybe a)
handleQuery = case _ of
  -- The shell's transport: Play and Stop, the dashboard's, Panic.
  SQ.SetSounding s next -> do
    play (s /= Silent)
    pure (Just next)
  SQ.AskSounding reply -> H.gets \st -> Just (reply (if st.playing then Rig else Silent))
  SQ.AskSource reply -> H.gets \st -> Just (reply (fromMaybe "" (lineOf st)))
  SQ.AskMarkText reply -> H.gets \st -> reply <$> lineOf st
  SQ.AskBrowser reply -> H.gets \st -> Just (reply (browserOf st))
  SQ.AskBar reply -> H.gets \st -> Just (reply (barOf st))
  SQ.BrowserRecall slot _ next -> do
    st <- H.get
    for_ (index (entries st) slot) \(Tuple o _) -> handleAction (Recall o)
    pure (Just next)
  SQ.BrowserRename slot name next -> do
    st <- H.get
    for_ (index (entries st) slot) case _ of
      Tuple (Shelved h) p -> rename h p (String.trim name)
      _ -> pure unit
    pure (Just next)
  SQ.BrowserAction slot "delete" next -> do
    st <- H.get
    for_ (index (entries st) slot) case _ of
      Tuple (Shelved h) _ -> unshelve h
      _ -> pure unit
    pure (Just next)
  SQ.BrowserKeep next -> keepNow $> Just next
  SQ.Capture next -> keepNow $> Just next
  SQ.BarAction act next -> do
    case act of
      "stage:module" -> chooseTab ModuleTab
      "stage:laboratory" -> chooseTab LaboratoryTab
      "stage:library" -> chooseTab LibraryTab
      "chip:limulus" -> toLimulus
      "help" -> H.modify_ _ { welcome = true }
      _ -> pure unit
    pure (Just next)
  _ -> pure Nothing

chooseTab :: Tab -> M Unit
chooseTab t = do
  H.modify_ _ { tab = t }
  H.raise Changed

-- | Play or stop, as the shell says. A scene goes to the rig as it starts; a
-- | stop goes as it stops.
play :: Boolean -> M Unit
play on = do
  st <- H.get
  unless (on == st.playing) do
    H.modify_ _ { playing = on }
    if on then sendNow else sendText "conspicillum-stop"

-- | The page started or stopped by itself: the shell's transport follows.
playOfItsOwn :: Boolean -> M Unit
playOfItsOwn on = do
  st <- H.get
  unless (on == st.playing) do
    play on
    H.raise (Armed on)

-- | Publish the line now and ask Limulus to show it, and the shell to open it.
toLimulus :: M Unit
toLimulus = do
  st <- H.get
  for_ st.current \c -> do
    let text = print c.line
    sendText (Stage.writeLine text)
    sendText Stage.openLine
    H.modify_ _ { stagedLine = Just text, linePublishedAt = st.now }
  H.raise WantsLimulus
  -- once more when a Limulus opened just now has loaded and subscribed;
  -- one that already shows the line only reveals it again
  void $ H.fork do
    H.liftAff (delay (Milliseconds 2500.0))
    sendText Stage.openLine

-- | The drawer's keep, and `c`: the scene as a preset of its own at once,
-- | named by its name and the time ("Single hit · 21:14") until renamed in
-- | the drawer, as every machine's capture is.
keepNow :: M Unit
keepNow = do
  st <- H.get
  for_ st.current \c -> do
    time <- H.liftEffect Browser.stamp
    let preset = c { name = unstamped c.name <> " · " <> time }
    result <- H.liftAff (Shelf.keep st.host preset)
    case result of
      Left problem -> H.modify_ _ { shelfProblem = Just problem }
      Right kept -> do
        reloaded <- H.liftAff (Shelf.shelf st.host)
        handleAction (ShelfLoaded reloaded)
        H.modify_ _ { origin = Just (Shelved kept.hash), original = Just preset, current = Just preset }
        remember (Shelved kept.hash)
        H.raise Changed

-- | A name without the time a keep gave it ("Single hit · 21:14" is "Single
-- | hit"), so keeping a kept preset again does not stack times.
unstamped :: String -> String
unstamped name =
  let
    n = String.length name
    time = String.drop (n - 5) name
  in
    if n > 8 && String.take 3 (String.drop (n - 8) name) == " · " && String.take 1 (String.drop 2 time) == ":" then String.take (n - 8) name
    else name

-- | A kept preset renamed in the drawer: kept again under the new name, in
-- | its place on the shelf; the library follows it to its new hash.
rename :: String -> Preset -> String -> M Unit
rename h p name = unless (name == "" || name == p.name) do
  st <- H.get
  let renamed = p { name = name }
  result <- H.liftAff (Shelf.replace st.host h renamed)
  case result of
    Left problem -> H.modify_ _ { shelfProblem = Just problem }
    Right kept -> do
      when (kept.hash /= h) $ keepLibrary (Library.renameReference ("kept:" <> h) ("kept:" <> kept.hash))
      reloaded <- H.liftAff (Shelf.shelf st.host)
      handleAction (ShelfLoaded reloaded)
      when (st.origin == Just (Shelved h)) do
        H.modify_ \s -> s
          { origin = Just (Shelved kept.hash)
          , original = s.original <#> _ { name = name }
          , current = s.current <#> _ { name = name }
          }
        remember (Shelved kept.hash)
      H.raise Changed

-- | Off the shelf. The content stays in the store, so nothing kept is lost;
-- | the scene stays in hand, with no place on the shelf.
unshelve :: String -> M Unit
unshelve h = do
  st <- H.get
  result <- H.liftAff (Shelf.remove st.host h)
  case result of
    Left problem -> H.modify_ _ { shelfProblem = Just problem }
    Right _ -> do
      reloaded <- H.liftAff (Shelf.shelf st.host)
      handleAction (ShelfLoaded reloaded)
      when (st.origin == Just (Shelved h)) $ H.modify_ _ { origin = Nothing }
      H.raise Changed

-- | Where the page is, kept for a reload and shown in the address.
remember :: Origin -> M Unit
remember o = H.liftEffect do
  w <- window
  Window.localStorage w >>= Storage.setItem storageKey (token o)
  Window.location w >>= Location.setHash (token o)

-- ── handling ─────────────────────────────────────────────────────────────────

handleAction :: Action -> M Unit
handleAction = case _ of
  Initialise -> do
    w <- H.liftEffect window
    reached <- H.liftEffect (Window.location w >>= Location.hostname)
    search <- H.liftEffect (Window.location w >>= Location.search)
    welcomed <- H.liftEffect (Window.localStorage w >>= Storage.getItem welcomeKey)
    let host = if reached == "" then "localhost" else reached
    rig <- H.liftEffect (Binnacle.connect { url: "ws://" <> host <> ":3012/ws", tempo: 120.0 })
    { emitter, listener } <- H.liftEffect HS.create
    void $ H.subscribe emitter
    H.liftEffect (Binnacle.onAppMessage rig (HS.notify listener <<< FromRig))
    -- `?laboratory` and `?library` open on those views.
    H.modify_ _
      { rig = Just rig
      , host = host
      , welcome = isNothing welcomed
      , tab =
          if String.contains (String.Pattern "laboratory") search then LaboratoryTab
          else if String.contains (String.Pattern "library") search then LibraryTab
          else ModuleTab
      }
    H.raise Changed
    document <- H.liftEffect (Window.document w)
    let target = HTMLDocument.toEventTarget document
    void $ H.subscribe (Frame <$> frames)
    void $ H.subscribe $ HQE.eventListener (Event.EventType "keydown") target (map Key <<< KeyboardEvent.fromEvent)
    void $ H.subscribe $ HQE.eventListener (Event.EventType "pointermove") target (map Move <<< MouseEvent.fromEvent)
    void $ H.subscribe $ HQE.eventListener (Event.EventType "pointerup") target (\_ -> Just Release)
    void $ H.subscribe $ HQE.eventListener (Event.EventType "pointercancel") target (\_ -> Just Release)
    -- The material and the store, in the background: the shell waits for
    -- this page to start before it starts itself, and its first word (the
    -- transport, stopped) must come before the stage says what is playing.
    void $ H.fork do
      corpora <- H.liftAff (load "/conspicillum-corpora.json")
      handleAction (Loaded corpora)
      kept <- H.liftAff (Shelf.shelf host)
      handleAction (ShelfLoaded kept)
      library <- H.liftAff (Library.load host)
      handleAction (LibraryLoaded library)

  Loaded (Left problem) -> H.modify_ _ { problem = Just problem }

  Loaded (Right corpora) -> do
    H.modify_ _ { corpora = corpora }
    st <- H.get
    -- A link names its preset (`#12`, `#k…`); otherwise the one last played
    -- here. A kept one waits for the shelf.
    { hash, stored } <- H.liftEffect do
      w <- window
      hash <- Window.location w >>= Location.hash
      stored <- Window.localStorage w >>= Storage.getItem storageKey
      pure { hash, stored }
    let
      wanted = fromToken (String.drop 1 hash) <|> (stored >>= fromToken)
      playableAt i = maybe false (isJust <<< setOf st) (index st.presets i)
      first = Factory (fromMaybe 0 (findIndex (\p -> isJust (setOf st p)) st.presets))
    -- recalled without a send: the page has only just opened, and the stage
    -- will say what is playing, if anything
    case wanted of
      Just (Factory i) | playableAt i -> recall (Factory i)
      Just (Shelved h) -> do
        H.modify_ _ { wantedKept = Just h }
        recall first
      _ -> recall first

  ShelfLoaded (Left problem) -> H.modify_ _ { shelfProblem = Just problem }

  ShelfLoaded (Right kept) -> do
    H.modify_ _ { shelf = kept, shelfProblem = Nothing }
    st <- H.get
    for_ st.wantedKept \h -> do
      H.modify_ _ { wantedKept = Nothing }
      when (isJust (presetAt st (Shelved h))) $ recall (Shelved h)
    H.raise Changed

  LibraryLoaded (Left problem) -> H.modify_ _ { shelfProblem = Just problem }

  LibraryLoaded (Right { library, hash }) -> do
    H.modify_ _ { library = library, libraryHash = hash }
    H.raise Changed

  FromLibrary (LibraryView.Audition ref) -> do
    st <- H.get
    for_ (find (\(Tuple o p) -> reference o p == ref) (entries st)) \(Tuple o _) -> handleAction (Recall o)

  FromLibrary (LibraryView.Change edit) -> do
    keepLibrary edit
    H.raise Changed

  Frame now -> do
    st <- H.get
    reading <- H.liftEffect (traverseRig st Clock.read)
    connected <- H.liftEffect (fromMaybe (pure false) (st.rig <#> Transport.isConnected <<< Binnacle.socket))
    let
      beat = maybe (now / 500.0) _.beat reading
      tempo = maybe 120.0 _.tempo reading
      bars = beat / 4.0
      cycle = floor bars
      phase = bars - toNumber cycle
      retempo = abs (tempo - st.tempo) > 0.01
      fresh = st { now = now, tempo = tempo, locked = maybe false _.locked reading, connected = connected, phase = phase }
      planned =
        if cycle /= st.plan.cycle || retempo then
          replan (fresh { plan = fresh.plan { cycle = cycle }, dots = [] })
        else fresh
      -- The grains that began since the last frame: all up to here in a new
      -- cycle, the ones crossed in this one.
      since = if planned.plan.cycle /= st.plan.cycle then -1.0 else st.phase
      begun = filter (\g -> g.from > since && g.from <= phase) planned.plan.grains
      dots = live now (spawnAll now planned.dots begun)
      loupeAt = maybe planned.loupeAt (\g -> g.reads.from.segment) (lastOf begun)
    H.put planned { dots = drop (length dots - 160) dots, loupeAt = loupeAt, unsent = planned.unsent || (retempo && planned.playing) }
    -- A new connection, including a re-dial after the rig restarted, asks
    -- the stage what is loaded.
    if not connected then H.modify_ _ { staged = false }
    else when (not planned.staged && not (null planned.corpora)) do
      H.modify_ _ { staged = true }
      sendText "stage-subscribe"
      sendText Stage.textSubscribe
    sendIfDue
    publishLineIfDue

  -- Another page changed what the rig has loaded, or this page just asked:
  -- show it. Not while a number is being dragged here, since the drag's next
  -- send will win anyway. Nothing is sent: this is what is already playing.
  FromRig text | Just frame <- Stage.readLine text -> case frame of
    Stage.LineHeld held -> H.modify_ _ { stagedLine = held }
    Stage.LineWritten Nothing -> H.modify_ _ { stagedLine = Nothing }
    -- Written elsewhere (Limulus's `conspicillum $ …`): play it as if typed
    -- here, or refuse it, which leaves the writer's block as it was typed.
    Stage.LineWritten (Just text') -> do
      st <- H.get
      H.modify_ _ { stagedLine = Just text' }
      case readTyped st text' of
        Left problem -> sendText (Stage.rejectLine problem)
        -- the line in hand, written back (another page of this one, say):
        -- nothing changes, and the preset keeps its name
        Right line | Just (print line) == (print <<< _.line <$> st.current) -> pure unit
        Right line -> do
          playTyped line
          if st.playing then sendNow else playOfItsOwn true

  FromRig text -> for_ (Stage.readEntry text) \entry -> when (entry.slot == "conspicillum") do
    st <- H.get
    when (isNothing st.drag) do
      case entry.page >>= \page -> Tuple page <$> Shelf.fromStored page.current of
        Nothing -> H.modify_ _ { unsent = false }
        Just (Tuple page current) -> do
          let origin = page.origin >>= fromToken
          H.modify_ \s -> replan s
            { origin = origin, original = page.original >>= Shelf.fromStored, current = Just current
            , held = page.held, unsent = false, wantedKept = Nothing
            , lanes = 1, dots = [], shape = Nothing
            }
          for_ origin remember
          firstGrainInLoupe
          H.raise Changed
      -- what the rig is doing, as it is: the shell's transport follows
      unless (entry.playing == st.playing) do
        H.modify_ _ { playing = entry.playing }
        H.raise (Armed entry.playing)

  Recall origin -> do
    recall origin
    sendNow

  Step d -> do
    st <- H.get
    let
      all = onModule st
      count = length all
      from = fromMaybe (-1) (st.origin >>= \o -> findIndex (\(Tuple o' _) -> o' == o) all)
      candidates = (1 .. count) <#> \k -> ((from + d * k) `mod` count + count) `mod` count
      next = index (filter (\i -> maybe false (\(Tuple _ p) -> isJust (setOf st p)) (index all i)) candidates) 0
    for_ (next >>= index all) \(Tuple o _) -> handleAction (Recall o)

  Revert -> do
    st <- H.get
    for_ st.original \o -> do
      H.modify_ \s -> replan s { current = Just o, lanes = 1 }
      changed

  ChooseSet name -> do
    st <- H.get
    for_ (setNamed st.corpora name) \set ->
      editLine \line -> withMaterial set (sampleChoice set line.n) line

  ChooseSample n -> do
    st <- H.get
    for_ (st.current >>= setOf st) \set -> editLine (withMaterial set n)

  Hold h -> H.modify_ _ { held = h }

  Grab parameter axis event -> for_ (MouseEvent.fromEvent event) \m ->
    when (MouseEvent.button m == 0) do
      H.liftEffect (Event.preventDefault event)
      st <- H.get
      for_ st.current \p ->
        H.modify_ _
          { drag = Just
              { parameter, axis
              , x: toNumber (MouseEvent.clientX m), y: toNumber (MouseEvent.clientY m)
              , from: normalise parameter (read parameter p.line.spec)
              }
          , shape = Just p.line
          , highlighted = Just parameter
          }

  Move m -> do
    st <- H.get
    for_ st.drag \d -> do
      let
        travel = case d.axis of
          Across -> toNumber (MouseEvent.clientX m) - d.x
          Up -> d.y - toNumber (MouseEvent.clientY m)
        fine = MouseEvent.shiftKey m
      setParameter d.parameter (denormalise d.parameter (d.from + travel / (if fine then 900.0 else 180.0)))

  Release -> do
    st <- H.get
    when (isJust st.drag) $ H.modify_ _ { drag = Nothing, shape = Nothing, highlighted = Nothing }

  Hover p -> do
    st <- H.get
    unless (isJust st.drag) $ H.modify_ _ { highlighted = p }

  Nudge parameter key -> do
    let
      direction = case KeyboardEvent.key key of
        "ArrowUp" -> 1.0
        "ArrowDown" -> -1.0
        _ -> 0.0
    when (direction /= 0.0) do
      H.liftEffect do
        Event.preventDefault (KeyboardEvent.toEvent key)
        Event.stopPropagation (KeyboardEvent.toEvent key)
      st <- H.get
      for_ st.current \p ->
        setParameter parameter
          (denormalise parameter (normalise parameter (read parameter p.line.spec) + direction * (if KeyboardEvent.shiftKey key then 0.1 else 0.01)))

  Reset parameter -> do
    st <- H.get
    for_ st.original \o -> setParameter parameter (read parameter o.line.spec)

  SetParameter parameter value -> setParameter parameter value

  EditSpec f -> editLine \line -> line { spec = f line.spec }

  Pin parameter -> do
    H.modify_ \st -> st
      { current = st.current <#> \c ->
          if elem parameter (c.knobs <#> _.parameter) then c { knobs = filter (\k -> k.parameter /= parameter) c.knobs }
          else if length c.knobs >= 4 then c
          else c { knobs = snoc c.knobs { parameter, label: String.toLower (describe parameter).label } }
      }

  -- The arrows step through the presets; the rest of the keys (space, b,
  -- Ctrl+`, c) are the shell's.
  Key key -> do
    st <- H.get
    unless (typingIn key || KeyboardEvent.metaKey key || KeyboardEvent.ctrlKey key || KeyboardEvent.altKey key) do
      case KeyboardEvent.key key of
        "ArrowLeft" | st.tab /= LibraryTab -> prevent key *> handleAction (Step (-1))
        "ArrowRight" | st.tab /= LibraryTab -> prevent key *> handleAction (Step 1)
        "Escape" | st.welcome -> handleAction CloseWelcome
        _ -> pure unit

  CloseWelcome -> do
    H.modify_ _ { welcome = false }
    H.liftEffect (window >>= Window.localStorage >>= Storage.setItem welcomeKey "1")

  -- a click on the card stays on the card, rather than closing it
  Swallow e -> H.liftEffect (Event.stopPropagation e)

  NoOp -> pure unit
  where
  prevent key = H.liftEffect (Event.preventDefault (KeyboardEvent.toEvent key))

welcomeKey :: String
welcomeKey = "conspicillum.welcomed"

-- | Whether a key went to a field: the laboratory's inputs and selects keep
-- | their arrows and spaces.
foreign import typingIn :: KeyboardEvent -> Boolean

-- ── the pieces of handling ───────────────────────────────────────────────────

-- | Make a preset the one in hand, without sending it.
recall :: Origin -> M Unit
recall origin = do
  st <- H.get
  for_ (presetAt st origin) \p -> do
    let
      -- Held material stays; the preset brings everything else.
      held = case st.current of
        Just c | st.held -> maybe p (\set -> p { line = withMaterial set c.line.n p.line }) (setOf st c)
        _ -> p
    H.modify_ \s -> replan s
      { origin = Just origin, original = Just held, current = Just held, lanes = 1, dots = [], loupeAt = 0
      , shape = Nothing
      }
    remember origin
    firstGrainInLoupe
    H.raise Changed

editLine :: (Line -> Line) -> M Unit
editLine f = do
  H.modify_ \st -> replan st { current = st.current <#> \p -> p { line = f p.line } }
  changed

setParameter :: Parameter -> Number -> M Unit
setParameter parameter value = editLine \line -> line { spec = set parameter value line.spec }

-- | The library was changed here: show it at once, and keep it. What comes
-- | back is the library as kept, which also has any change another page made
-- | meanwhile. A failed save says so where the shelf's problems are said.
keepLibrary :: Library.Edit -> M Unit
keepLibrary edit = do
  before <- H.get
  H.modify_ _ { library = edit before.library }
  kept <- H.liftAff (Library.keep before.host { hash: before.libraryHash, library: before.library } edit)
  case kept of
    Left problem -> H.modify_ _ { shelfProblem = Just ("the library was not kept: " <> problem) }
    Right { library, hash } -> H.modify_ _ { library = library, libraryHash = hash }

-- | A line written elsewhere, if it reads and names a set this page has.
readTyped :: State -> String -> Either String Line
readTyped st text = case parse text of
  Left problem -> Left problem
  Right line
    | isNothing (setNamed st.corpora line.set) -> Left ("no set called \"" <> line.set <> "\"")
    | otherwise -> Right line

-- | Make a line the scene in hand, as a preset of its own ("From the line").
playTyped :: Line -> M Unit
playTyped line = do
  st <- H.get
  for_ st.current \p -> do
    let typed = p { name = "From the line", about = "", line = line }
    H.modify_ \s -> replan s
      { origin = Nothing, original = Just typed, current = Just typed, lanes = 1 }
    firstGrainInLoupe
    H.raise Changed

-- | Publish the line when it differs from the stage's: not mid-drag, and
-- | not more often than every 300 ms, so turning a knob is not a stream of
-- | writes.
publishLineIfDue :: M Unit
publishLineIfDue = do
  st <- H.get
  for_ st.current \c -> do
    let text = print c.line
    when (st.staged && isNothing st.drag && st.stagedLine /= Just text && st.now - st.linePublishedAt >= 300.0) do
      sendText (Stage.writeLine text)
      H.modify_ _ { stagedLine = Just text, linePublishedAt = st.now }

-- | The scene changed: send it, soon, if it is playing.
changed :: M Unit
changed = H.modify_ \st -> st { unsent = st.playing }

sendIfDue :: M Unit
sendIfDue = do
  st <- H.get
  when (st.unsent && st.now - st.sentAt >= 120.0) sendNow

-- | A recall or a Play lands now, not on the debounce.
sendNow :: M Unit
sendNow = do
  st <- H.get
  when st.playing $ for_ (Tuple <$> sceneText st <*> heldOf st) \(Tuple scene page) -> do
    sendText (Stage.push { base: baseOf st.origin, edited: edited st, page, scene })
    H.modify_ _ { unsent = false, sentAt = st.now }

sendText :: String -> M Unit
sendText text = do
  st <- H.get
  for_ st.rig \rig -> H.liftEffect (Transport.send (Binnacle.socket rig) text)

firstGrainInLoupe :: M Unit
firstGrainInLoupe = H.modify_ \st -> st { loupeAt = maybe 0 _.reads.from.segment (index st.plan.grains 0) }

spawnAll :: Number -> Array Dot -> Array Grain -> Array Dot
spawnAll now = foldl \dots g -> snoc dots (spawn now dots g)

lastOf :: forall a. Array a -> Maybe a
lastOf xs = index xs (length xs - 1)

traverseRig :: forall a. State -> (Clock.Clock -> Effect a) -> Effect (Maybe a)
traverseRig st f = case st.rig of
  Just rig -> Just <$> f (Binnacle.clock rig)
  Nothing -> pure Nothing
