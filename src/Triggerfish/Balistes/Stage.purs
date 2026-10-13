-- | Triggerfish.Balistes.Stage — the stage's copy of Balistes's named
-- | beats, which `balistes $ "<a b>"` on the rig plays by name
-- | (`balistes/rhythm/<word>`, the word the name's slug). A Rytm rhythm's text
-- | is its printed form, its name as typed; a Grids beat's is `grids ` and the
-- | whole engine state it plays from (the saved point on this page's notes
-- | and ratchets, at the top of its pattern). Internal, as Vetula's
-- | progressions are: the user sees beats and words, never this copy.
module Triggerfish.Balistes.Stage
  ( subscribeLine
  , bankRhythms
  , rhythmTable
  , rhythmLines
  , rhythmDrag
  , dragPrefix
  , gridsPrefix
  , uniqueNames
  , freeName
  , takenWords
  ) where

import Prelude

import Data.Array (catMaybes, filter, foldl, mapMaybe)
import Data.Array as Array
import Data.Either (hush)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), stripPrefix)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Reef.Balistes.Lepidoptera (printPattern, rhythmKey, rhythmOfKey, rhythmSlug)
import Reef.Balistes.Protocol (encodeBalSim)
import Reef.Balistes.Sim (fromTop)
import Simple.JSON (readJSON)
import Triggerfish.Balistes.Model as M
import Triggerfish.Balistes.TriSnapshot (TriSnapshot(..), parseTri, rhythmOfContent)
import Triggerfish.Preset (Preset)

subscribeLine :: String
subscribeLine = "stage-text-subscribe"

-- | The bank's named beats as the stage keeps them, by word, a Grids beat
-- | played on `bal`'s notes and ratchets. Captures (no name) are not.
bankRhythms :: M.Balistes -> Array Preset -> Map String String
bankRhythms bal presets = Map.fromFoldable (mapMaybe one presets)
  where
  one p = do
    name <- p.name
    text <- stageText name p.content
    let word = rhythmSlug name
    if word == "" then Nothing else Just (Tuple word text)
  stageText name content = case parseTri content of
    Just (TSGrids point) -> Just (gridsPrefix <> encodeBalSim (fromTop (M.applySnapshot point bal)))
    Just (TSFixed _) -> printPattern <$> rhythmOfContent name content
    Nothing -> Nothing

-- | What begins a Grids beat's stage text (the rig reads the rest as the
-- | engine state, `Reef.Balistes.Protocol.decodeBalSim`).
gridsPrefix :: String
gridsPrefix = "grids "

-- | The rhythms the stage holds, by word, from the whole table (`Nothing`
-- | for any other frame).
rhythmTable :: String -> Maybe (Map String String)
rhythmTable msg = do
  json <- stripPrefix (Pattern "stage-texts ") msg
  table :: Object { text :: String } <- hush (readJSON json)
  pure (Map.fromFoldable (mapMaybe entry (Object.toUnfoldable table :: Array (Tuple String { text :: String }))))
  where
  entry (Tuple key v) = (\word -> Tuple word v.text) <$> rhythmOfKey key

-- | The writes that bring the stage's rhythms from `seen` to `now`: one per
-- | changed or new rhythm, a delete per one gone.
rhythmLines :: Map String String -> Map String String -> Array String
rhythmLines seen now =
  catMaybes (map write (Map.toUnfoldable now :: Array (Tuple String String)))
    <> map (\word -> "stage-text-del " <> rhythmKey word)
         (filter (\word -> not (Map.member word now)) (Array.fromFoldable (Map.keys seen)))
  where
  -- the key is the word's, and a word is its own slug
  write (Tuple word text) =
    if Map.lookup word seen == Just text then Nothing
    else Just ("stage-text " <> rhythmKey word <> " " <> text)

-- | What a drag of a bank row carries: `balistes-rhythm <word>` for a named
-- | beat of either brain (Limulus places the word), "" for anything else.
rhythmDrag :: Preset -> String
rhythmDrag p = case p.name of
  Just name | Just _ <- parseTri p.content, rhythmSlug name /= "" -> dragPrefix <> rhythmSlug name
  _ -> ""

dragPrefix :: String
dragPrefix = "balistes-rhythm "

-- | The bank with no two names sharing a word, so a word in Limulus means one
-- | beat: a later name whose word is taken gets the first free ` 2`, ` 3`, …
uniqueNames :: Array Preset -> Array Preset
uniqueNames = _.out <<< foldl step { taken: Set.empty, out: [] }
  where
  step acc p = case p.name of
    Just name | rhythmSlug name /= "" ->
      let named = freeName acc.taken name
      in { taken: Set.insert (rhythmSlug named) acc.taken, out: Array.snoc acc.out (p { name = Just named }) }
    _ -> acc { out = Array.snoc acc.out p }

-- | `name`, or the first of `name 2`, `name 3`, … whose word is not in `taken`.
freeName :: Set String -> String -> String
freeName taken name = go 1
  where
  go n =
    let candidate = if n == 1 then name else name <> " " <> show n
    in if Set.member (rhythmSlug candidate) taken then go (n + 1) else candidate

-- | The words the bank's names take.
takenWords :: Array Preset -> Set String
takenWords = Set.fromFoldable <<< mapMaybe (map rhythmSlug <<< _.name)
