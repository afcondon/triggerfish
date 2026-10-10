-- | **Vetula's voices, in its drawer** (docs/kb/plans/harmony-routes-coherent.md,
-- | AC 2026-10-05): what each voice plays and what it does to Odonus. Vetula
-- | owns that; the Dashboard only shows it.
-- |
-- | A voice is a line in Limulus, `Q $ …` (voices are lettered P..W; inside,
-- | numbered from 1, the stage's `vetula/vN`).
-- | Only the rig reads a card with Tidal, so the rig publishes each one's
-- | harmony (`vetula/harmonies`: `VOICE TERM HARMONY`, a line a voice), and
-- | the chords are named here from the rig's samples of that pattern
-- | (`odonus-sample`, as Odonus names its own progression). A voice can:
-- |
-- | - **sound**: where its card sends it (MIDI on its channel; `odo`, Odonus
-- |   only; `rig`);
-- | - **shape** Odonus (`odonus.grid <- vetula Q`): the melody becomes the
-- |   chords' arpeggios;
-- | - **colour** it (`odonus.out <- vetula Q`): the melody pulled onto the
-- |   chords' notes at the end.
-- |
-- | Shaping and colouring are routes, written as Limulus writes them
-- | (`route $ …`), so there is still one table.
module Triggerfish.Vetula.Voices
  ( Voice
  , harmoniesKey
  , routesKey
  , stageText
  , parseVoices
  , sampleRequest
  , sampleKeyPrefix
  , chordsOf
  , slotBase
  , rows
  , toggleLine
  ) where

import Prelude

import Data.Array (all, catMaybes, drop, find, foldl, last, length, mapMaybe, range, snoc, take, (!!))
import Data.Either (hush)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Nullable (Nullable, toMaybe)
import Data.String (Pattern(..), joinWith, split, stripPrefix, trim)
import Foreign.Object (Object)
import Foreign.Object as Object
import Reef.Input (Input(..))
import Reef.Route as Route
import Reef.Vetula.VoiceName (voiceLetter)
import Simple.JSON (readJSON, writeJSON)
import Triggerfish.Browser (Item)
import Triggerfish.Odonus.View.Progression (chordName)

-- | `voice`: its number, P = 1 (shown by letter).
type Voice = { voice :: Int, term :: String, harmony :: String }

harmoniesKey :: String
harmoniesKey = "vetula/harmonies"

routesKey :: String
routesKey = "routing/harmony"

-- | A stage object's text in a rig frame: from the whole table (on
-- | subscribing) or a write of `key` (`Just Nothing`: deleted). `Nothing`:
-- | the frame says nothing of it.
stageText :: String -> String -> Maybe (Maybe String)
stageText key msg = case stripPrefix (Pattern "stage-texts ") msg of
  Just json -> do
    table :: Object { text :: String } <- hush (readJSON json)
    pure (_.text <$> Object.lookup key table)
  Nothing -> do
    json <- stripPrefix (Pattern "stage-text ") msg
    w :: { key :: String, text :: Nullable String } <- hush (readJSON json)
    if w.key == key then Just (toMaybe w.text) else Nothing

-- | `vetula/harmonies`, one voice a line.
parseVoices :: String -> Array Voice
parseVoices = mapMaybe one <<< split (Pattern "\n")
  where
  one l = do
    let ws = split (Pattern " ") (trim l)
    n <- ws !! 0 >>= Int.fromString
    term <- ws !! 1
    pure { voice: n, term, harmony: joinWith " " (drop 2 ws) }

sampleKeyPrefix :: String
sampleKeyPrefix = "vetula-voice-"

-- | Ask the rig for a voice's chords over its next 16 cycles, a step a beat.
sampleRequest :: Voice -> String
sampleRequest v = "odonus-sample " <> writeJSON
  { key: sampleKeyPrefix <> show v.voice, from: 0, count: 64, quarters: 4
  , harmony: v.harmony, scale: (Nothing :: Maybe String), outScale: (Nothing :: Maybe { pattern :: String, root :: Int })
  , gridHarmony: (Nothing :: Maybe String) }

-- | The chord names in a run of samples, repeats collapsed, one turn of the
-- | progression (the shortest stretch the rest repeats), at most eight.
chordsOf :: Array Input -> Array String
chordsOf inputs = take 8 (oneTurn (map _.name runs))
  where
  runs = foldl (\acc ns -> if map _.notes (last acc) == Just ns then acc else snoc acc { name: chordName ns, notes: ns }) [] notes
  oneTurn xs = fromMaybe xs (find (\k -> all (\i -> xs !! i == xs !! (i `mod` k)) (range 0 (length xs - 1))) (range 1 (length xs)) <#> \k -> take k xs)
  notes = catMaybes (map (\i -> case i of
    SetSampled (Just c) _ _ _ _ -> Just c
    _ -> Nothing) inputs)

-- | Voice rows' slots start here, past any scene's.
slotBase :: Int
slotBase = 1000

-- | The drawer's rows for the voices: the name and its chords, where it sounds
-- | as the tag, and shape / colour as actions, ticked when routed.
rows :: Array Voice -> Map Int (Array String) -> Maybe String -> Array Item
rows voices names routesText = map row voices
  where
  routes = fromMaybe [] (routesText >>= hush <<< Route.parse)
  feeds input ch = Route.sourceOf input routes == Just (Route.VetulaVoice ch)
  row v =
    let
      chords = fromMaybe [] (Map.lookup v.voice names)
      shapes = feeds Route.OdonusGrid v.voice
      colours = feeds Route.OdonusOut v.voice
    in
      { slot: slotBase + v.voice
      , name: "voice " <> voiceLetter v.voice <> (if chords == [] then "" else ": " <> joinWith " \x00b7 " chords)
      , icons: []
      , tag: joinWith " \x00b7 " ([ sounds v.term ] <> (if shapes then [ "shapes Odonus" ] else []) <> (if colours then [ "colours Odonus" ] else []))
      , current: shapes || colours
      , section: "Voices"
      , builtin: true
      , drag: ""
      , actions: [ (if shapes then "\x2713 " else "") <> "shape Odonus", (if colours then "\x2713 " else "") <> "colour Odonus" ]
      }
  sounds = case _ of
    "odo" -> "Odonus only"
    "rig" -> "the rig"
    "mute" -> "muted"
    _ -> "MIDI"

-- | The route line an action on a voice's row sends: route it, or, when it is
-- | already routed there, take the route away.
toggleLine :: Int -> String -> Maybe String -> Maybe String
toggleLine n action routesText =
  input <#> \i ->
    let
      routes = fromMaybe [] (routesText >>= hush <<< Route.parse)
      on = Route.sourceOf i routes == Just (Route.VetulaVoice n)
    in
      "tidal route $ " <> Route.inputName i <> " <- " <> (if on then "none" else "vetula " <> voiceLetter n)
  where
  input
    | isJust (stripPrefix (Pattern "shape") (dropTick action)) = Just Route.OdonusGrid
    | isJust (stripPrefix (Pattern "colour") (dropTick action)) = Just Route.OdonusOut
    | otherwise = Nothing
  dropTick a = fromMaybe a (stripPrefix (Pattern "\x2713 ") a)
