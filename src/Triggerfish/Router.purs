-- | **The router's harmony matrix** (docs/kb/plans/matrix-router.md): which
-- | source feeds each of Odonus's two harmony inputs, as a grid of dots.
-- |
-- |                    odonus.grid   odonus.out
-- |     Vetula key          ●
-- |     Vetula voice 3                   ●
-- |     Scale  dorian  d
-- |     Harmony "<c'maj7 …>"
-- |
-- | The table itself lives on the rig's stage as `routing/harmony`
-- | (`Reef.Route`'s text, one route a line). The rig is its owner: it parses
-- | a write, refuses one it cannot read, applies the change to Odonus, and
-- | announces the canonical text to every page, this one too. So the matrix
-- | sends a whole table and shows whatever the stage says back, never its own
-- | guess. An input takes one source: a dot in a column moves the column's dot.
-- |
-- | The scale and harmony rows carry their own text; editing one that is
-- | routed sends the table again (on Enter or leaving the field).
module Triggerfish.Router
  ( Router
  , Line(..)
  , initial
  , readFrame
  , toggle
  , commit
  , setScalePattern
  , setScaleRoot
  , setHarmony
  , subscribeLine
  , Handlers
  , view
  ) where

import Prelude

import Data.Array (filter, mapMaybe, nub, sort)
import Data.Array as Array
import Data.Either (hush)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Nullable (Nullable, toMaybe)
import Data.String (Pattern(..), split, stripPrefix, trim)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Reef.Move (pitchClass)
import Reef.Route (Input(..), Routes, Source(..), inputName, print, sourceOf)
import Reef.Route as Route
import Simple.JSON (readJSON)
import Web.UIEvent.KeyboardEvent as KeyboardEvent

key :: String
key = "routing/harmony"

keyKey :: String
keyKey = "vetula/key"

subscribeLine :: String
subscribeLine = "stage-text-subscribe"

type Router =
  { routes :: Routes
  -- Vetula's cards as the stage holds them (key → line), for its voices
  , cards :: Map String String
  -- Vetula's key as the stage holds it (`vetula/key`), what `vetula key` feeds
  , vetulaKey :: Maybe String
  , scalePattern :: String
  , scaleRoot :: String
  , harmony :: String
  -- the rig's last refusal, until the next table it accepts
  , problem :: Maybe String
  }

initial :: Router
initial =
  { routes: [], cards: Map.empty, vetulaKey: Nothing
  , scalePattern: "major", scaleRoot: "c", harmony: "<c'maj7 a'min7>/2", problem: Nothing }

data Line = RKey | RVoice Int | RScale | RHarmony

derive instance Eq Line

-- | Vetula's voices: the channels its cards play on (`ch3 …`).
voices :: Router -> Array Int
voices r = sort (nub (mapMaybe channel (Array.fromFoldable (Map.values r.cards))))
  where
  channel line = Array.head (split (Pattern " ") (trim line)) >>= stripPrefix (Pattern "ch") >>= Int.fromString

rows :: Router -> Array Line
rows r = [ RKey ] <> map RVoice (voices r) <> [ RScale, RHarmony ]

-- | A rig frame: the stage's table (on subscribing) or one object written,
-- | and the rig's answer to this page's own write.
readFrame :: String -> Router -> Maybe Router
readFrame msg r = case stripPrefix (Pattern "stage-texts ") msg of
  Just json -> do
    table :: Object { text :: String } <- hush (readJSON json)
    let
      cards = Map.fromFoldable (mapMaybe card (Object.toUnfoldable table))
      routes = fromMaybe [] (Object.lookup key table >>= \t -> hush (Route.parse t.text))
    pure (withRoutes routes r { cards = cards, vetulaKey = _.text <$> Object.lookup keyKey table })
  Nothing -> case stripPrefix (Pattern "stage-text ") msg of
    Just json -> do
      w :: { key :: String, text :: Nullable String } <- hush (readJSON json)
      case toMaybe w.text of
        _ | w.key == key ->
          pure (withRoutes (fromMaybe [] (toMaybe w.text >>= \t -> hush (Route.parse t))) r { problem = Nothing })
        t | w.key == keyKey -> pure r { vetulaKey = t }
        Just t | isCard w.key -> pure r { cards = Map.insert w.key t r.cards }
        Nothing | isCard w.key -> pure r { cards = Map.delete w.key r.cards }
        _ -> Nothing
    Nothing -> case stripPrefix (Pattern "ERR: routing: ") msg of
      Just why -> pure r { problem = Just why }
      Nothing -> Nothing
  where
  isCard k = isJust (stripPrefix (Pattern "vetula/v") k)
  card (Tuple k v) = if isCard k then Just (Tuple k v.text) else Nothing

-- | The stage's routes, and the scale and harmony rows showing what they hold.
withRoutes :: Routes -> Router -> Router
withRoutes routes r = foldSources r { routes = routes }
  where
  foldSources r0 = Array.foldl draft r0 (map _.source routes)
  draft acc = case _ of
    Scale s -> acc { scalePattern = s.pattern, scaleRoot = rootName s.root }
    Harmony h -> acc { harmony = h }
    _ -> acc

rootName :: Int -> String
rootName pc = fromMaybe (show pc) (Array.index [ "c", "cs", "d", "ds", "e", "f", "fs", "g", "gs", "a", "as", "b" ] pc)

sourceOfRow :: Router -> Line -> Source
sourceOfRow r = case _ of
  RKey -> VetulaKey
  RVoice n -> VetulaVoice n
  RScale -> Scale { pattern: r.scalePattern, root: fromMaybe 0 (hush (pitchClass (trim r.scaleRoot))) }
  RHarmony -> Harmony r.harmony

-- | Whether a row may feed an input: the grid takes only a scale.
allowed :: Line -> Input -> Boolean
allowed row input = case input, row of
  OdonusGrid, RVoice _ -> false
  OdonusGrid, RHarmony -> false
  _, _ -> true

rowFeeds :: Router -> Line -> Input -> Boolean
rowFeeds r row input = case sourceOf input r.routes, row of
  Just VetulaKey, RKey -> true
  Just (VetulaVoice n), RVoice m -> n == m
  Just (Scale _), RScale -> true
  Just (Harmony _), RHarmony -> true
  _, _ -> false

-- | The line that writes a table to the stage.
writeLine :: Routes -> String
writeLine routes
  | Array.null routes = "stage-text-del " <> key
  | otherwise = "stage-text " <> key <> " " <> print routes

-- | A dot pressed: a lit one is cleared, an unlit one takes its column.
toggle :: Line -> Input -> Router -> Maybe String
toggle row input r
  | not (allowed row input) = Nothing
  | rowFeeds r row input = Just (writeLine (filter (\rt -> rt.input /= input) r.routes))
  | otherwise = Just (writeLine (setRoute input (sourceOfRow r row) r.routes))

setRoute :: Input -> Source -> Routes -> Routes
setRoute input source routes =
  filter (\rt -> rt.input == OdonusGrid) others' <> filter (\rt -> rt.input == OdonusOut) others'
  where
  others' = filter (\rt -> rt.input /= input) routes <> [ { input, source } ]

-- | A scale or harmony row edited: if it feeds an input, send the table with
-- | the new text.
commit :: Line -> Router -> Maybe String
commit row r =
  let ins = filter (rowFeeds r row) [ OdonusGrid, OdonusOut ]
  in if Array.null ins then Nothing
     else Just (writeLine (Array.foldl (\rs i -> setRoute i (sourceOfRow r row) rs) r.routes ins))

setScalePattern :: String -> Router -> Router
setScalePattern t r = r { scalePattern = t }

setScaleRoot :: String -> Router -> Router
setScaleRoot t r = r { scaleRoot = t }

setHarmony :: String -> Router -> Router
setHarmony t r = r { harmony = t }

type Handlers i =
  { toggle :: Line -> Input -> i
  , scalePattern :: String -> i
  , scaleRoot :: String -> i
  , harmony :: String -> i
  , commit :: Line -> i
  , none :: i
  }

view :: forall w i. Handlers i -> Boolean -> Router -> HH.HTML w i
view on rigUp r =
  HH.section [ cls "router" ]
    [ HH.div [ cls "sectionhead" ]
        [ HH.h2_ [ HH.text "Harmony" ]
        , HH.span [ cls "note" ]
            [ HH.text "What Odonus quantises to. The grid is the scale its cells read; the output is what each note snaps to after its offset. One source each." ]
        ]
    , HH.table [ cls "matrix" ]
        [ HH.thead_
            [ HH.tr_
                [ HH.th_ []
                , colHead OdonusGrid "the scale its cells read"
                , colHead OdonusOut "what each note snaps to"
                ]
            ]
        , HH.tbody_ (map row (rows r))
        ]
    , case r.problem of
        Just why -> HH.p [ cls "router-problem" ] [ HH.text ("The rig refused that: " <> why) ]
        Nothing -> HH.text ""
    , if rigUp then HH.text ""
      else HH.p [ cls "note" ] [ HH.text "The rig is not connected: routes are kept and applied there." ]
    ]
  where
  colHead input what =
    HH.th [ HP.title what ] [ HH.text (inputName input) ]
  row rw =
    HH.tr [ cls (if Array.any (rowFeeds r rw) [ OdonusGrid, OdonusOut ] then "live" else "") ]
      [ HH.th [ cls "src" ] (label rw)
      , cell rw OdonusGrid
      , cell rw OdonusOut
      ]
  cell rw input
    | not (allowed rw input) = HH.td [ cls "dot none" ] []
    | otherwise =
        HH.td [ cls "dot" ]
          [ HH.button
              [ cls (if rowFeeds r rw input then "on" else "off")
              , HE.onClick \_ -> on.toggle rw input
              , HP.title (if rowFeeds r rw input then "stop feeding " <> inputName input else "feed " <> inputName input)
              ]
              []
          ]
  label = case _ of
    RKey ->
      [ HH.text "Vetula key "
      , HH.small_ [ HH.text (fromMaybe "(Vetula has not said)" r.vetulaKey) ]
      ]
    RVoice n -> [ HH.text ("Vetula voice " <> show n) ]
    RScale ->
      [ HH.text "Scale "
      , field "pattern" r.scalePattern on.scalePattern RScale "a scale name, or a pattern of them: <dorian lydian>/4"
      , field "root" r.scaleRoot on.scaleRoot RScale "the root: c, fs, bf, or 0-11"
      ]
    RHarmony ->
      [ HH.text "Harmony "
      , field "pattern wide" r.harmony on.harmony RHarmony "a Tidal note pattern of chords: <c'maj7 a'min7>/2"
      ]
  field c value set rw hint =
    HH.input
      [ cls c, HP.value value, HP.title hint, HP.spellcheck false
      , HE.onValueInput set
      , HE.onBlur \_ -> on.commit rw
      , HE.onKeyDown \k -> if KeyboardEvent.key k == "Enter" then on.commit rw else on.none
      ]

cls :: forall r i. String -> HP.IProp (class :: String | r) i
cls = HP.class_ <<< HH.ClassName
