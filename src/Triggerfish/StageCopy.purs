-- | Triggerfish.StageCopy — a machine's named things, copied to the rig's
-- | stage so a line plays them by word with no page open
-- | (`balistes $ "<a b>"` reads `balistes/rhythm/<word>`, `odonus $ "<a b>"`
-- | reads `odonus/patch/<word>`). Each machine says what a thing's text is;
-- | keeping the copy up to date is the same for all of them.
-- |
-- | The stage is shared by every browser, so a page counts as its own only
-- | the words its bank has: connecting adds and updates those and never
-- | deletes another browser's, and a delete is only ever a thing this page
-- | had and lost. Internal: the user sees things and words, never the copy.
module Triggerfish.StageCopy
  ( Copy
  , subscribeLine
  , table
  , ours
  , lines
  ) where

import Prelude

import Data.Array (catMaybes, filter, mapMaybe)
import Data.Array as Array
import Data.Either (hush)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), stripPrefix)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Simple.JSON (readJSON)

-- | Words to texts.
type Copy = Map String String

-- | Ask the stage for its whole table (`stage-texts <json>`), then each change.
subscribeLine :: String
subscribeLine = "stage-text-subscribe"

-- | The things under `prefix` the stage holds, by word, from the whole table
-- | (`Nothing` for any other frame).
table :: String -> String -> Maybe Copy
table prefix msg = do
  json <- stripPrefix (Pattern "stage-texts ") msg
  held :: Object { text :: String } <- hush (readJSON json)
  pure (Map.fromFoldable (mapMaybe entry (Object.toUnfoldable held :: Array (Tuple String { text :: String }))))
  where
  entry (Tuple key v) = (\word -> Tuple word v.text) <$> stripPrefix (Pattern prefix) key

-- | Of what the stage holds, what this bank counts as its own: the words it has.
ours :: Copy -> Copy -> Copy
ours held now = Map.filterKeys (\word -> Map.member word now) held

-- | The writes that bring the stage from `seen` to `now`: one per new or
-- | changed thing, a delete per one gone.
lines :: String -> Copy -> Copy -> Array String
lines prefix seen now =
  catMaybes (map write (Map.toUnfoldable now :: Array (Tuple String String)))
    <> map (\word -> "stage-text-del " <> prefix <> word)
         (filter (\word -> not (Map.member word now)) (Array.fromFoldable (Map.keys seen)))
  where
  write (Tuple word text) =
    if Map.lookup word seen == Just text then Nothing
    else Just ("stage-text " <> prefix <> word <> " " <> text)
