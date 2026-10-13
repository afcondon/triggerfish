-- | Triggerfish.Odonus.Stage — Odonus's named presets as a line plays them:
-- | `odonus $ "<verse chorus>"` recalls each word's patch on the rig
-- | (`odonus/patch/<word>`, the text the preset's patch as saved), and a
-- | drawer row drags into Limulus as its word. The copy itself is
-- | `Triggerfish.StageCopy`'s, as Balistes's is.
module Triggerfish.Odonus.Stage
  ( stagePrefix
  , bankPatches
  , patchDrag
  , patchHint
  ) where

import Prelude

import Data.Array (concatMap)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Triggerfish.Drag (dragOf)
import Triggerfish.Preset (Preset, wordsOf)
import Triggerfish.StageCopy (Copy)

-- | Where the stage keeps Odonus's patches.
stagePrefix :: String
stagePrefix = "odonus/patch/"

-- | The bank's named presets as the stage keeps them, by every word each
-- | plays by.
bankPatches :: Array Preset -> Copy
bankPatches = Map.fromFoldable <<< concatMap \p -> case p.name of
  Just _ -> map (\word -> Tuple word p.content) (wordsOf p)
  Nothing -> []

-- | What a drag of a row carries: its word, for a named preset; "" for a
-- | capture not yet named.
patchDrag :: Preset -> String
patchDrag p = case p.name, p.word of
  Just _, Just word -> dragOf "odonus-patch" word
  _, _ -> ""

-- | What hovering a row says.
patchHint :: Preset -> String
patchHint p
  | patchDrag p == "" = "Click: recall it, the way you last did. Double-click: name it (named, it can be dragged into Limulus)."
  | otherwise = "Click: recall it, the way you last did. Double-click: rename it. Drag it into Limulus, where it plays as " <> fromMaybe "" p.word <> ": onto an odonus line of names it joins the alternation; anywhere else it starts a new line."
