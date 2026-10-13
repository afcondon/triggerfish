-- | Triggerfish.Balistes.Stage — the stage's copy of Balistes's named
-- | beats, which `balistes $ "<a b>"` on the rig plays by name
-- | (`balistes/rhythm/<word>`, the word the name's slug). A Rytm rhythm's text
-- | is its printed form, its name as typed; a Grids beat's is `grids ` and the
-- | whole engine state it plays from (the saved point on this page's notes
-- | and ratchets, at the top of its pattern). Internal, as Vetula's
-- | progressions are: the user sees beats and words, never this copy.
module Triggerfish.Balistes.Stage
  ( stagePrefix
  , bankRhythms
  , rhythmDrag
  , dragPrefix
  , rhythmHint
  , gridsPrefix
  ) where

import Prelude

import Data.Array (concatMap)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Reef.Balistes.Lepidoptera (printPattern)
import Reef.Balistes.Protocol (encodeBalSim)
import Reef.Balistes.Sim (fromTop)
import Triggerfish.Balistes.Model as M
import Triggerfish.Balistes.TriSnapshot (TriSnapshot(..), parseTri, rhythmOfContent)
import Triggerfish.Preset (Preset, wordsOf)

-- | Where the stage keeps Balistes's beats (`Triggerfish.StageCopy`).
stagePrefix :: String
stagePrefix = "balistes/rhythm/"

-- | The bank's named beats as the stage keeps them, by every word each
-- | plays by, a Grids beat
-- | played on `bal`'s notes and ratchets. Captures (no name) are not.
bankRhythms :: M.Balistes -> Array Preset -> Map String String
bankRhythms bal presets = Map.fromFoldable (concatMap one presets)
  where
  one p = fromMaybe [] do
    name <- p.name
    text <- stageText name p.content
    pure (map (\word -> Tuple word text) (wordsOf p))
  stageText name content = case parseTri content of
    Just (TSGrids point) -> Just (gridsPrefix <> encodeBalSim (fromTop (M.applySnapshot point bal)))
    Just (TSFixed _) -> printPattern <$> rhythmOfContent name content
    Nothing -> Nothing

-- | What begins a Grids beat's stage text (the rig reads the rest as the
-- | engine state, `Reef.Balistes.Protocol.decodeBalSim`).
gridsPrefix :: String
gridsPrefix = "grids "

-- | What a drag of a bank row carries: `balistes-beat <word>` for a named
-- | beat of either brain (Limulus places the word), "" for anything else.
rhythmDrag :: Preset -> String
rhythmDrag p = case p.name, p.word of
  Just _, Just word | Just _ <- parseTri p.content -> dragPrefix <> word
  _, _ -> ""

-- | What hovering a row says.
rhythmHint :: Preset -> String
rhythmHint p
  | rhythmDrag p == "" = "Click: recall it. Double-click: name it (named, it can be dragged into Limulus)."
  | otherwise = "Click: recall it. Double-click: rename it. Drag it into Limulus, where it plays as " <> fromMaybe "" p.word <> ": onto a balistes line it joins the alternation; anywhere else it starts a new line."

dragPrefix :: String
dragPrefix = "balistes-beat "
