-- | **Lane shapes from the rig.** A lane's mini-notation (a POLYTRIG jack's
-- | source, a route) is read by the rig, never the page
-- | (docs/kb/plans/gpl-boundary-review.md): the page asks
-- | `lane-shapes ["x(3,8)", …]` and the rig answers each source's meter, cell
-- | mask, onsets and named onsets (Littorina's `Tidal.Lane`). The page keeps the
-- | answers by source text and draws and builds lanes from them. A source not
-- | yet answered reads as one empty cell with no onsets.
module Triggerfish.LaneShapes
  ( LaneShape
  , LaneShapes
  , requestLine
  , missing
  , readShapes
  , meterOf
  , cellMaskOf
  , onsetsOf
  , namedOnsetsOf
  ) where

import Prelude

import Data.Array (filter, nub)
import Data.Either (hush)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, maybe)
import Data.String (Pattern(..), stripPrefix, trim)
import Data.Tuple (Tuple)
import Foreign.Object (Object)
import Foreign.Object as Object
import Simple.JSON (readJSON, writeJSON)

type LaneShape =
  { meter :: Int
  , mask :: Array Boolean
  , onsets :: Array Number
  , named :: Array { name :: String, at :: Number }
  }

type LaneShapes = Map String LaneShape

requestLine :: Array String -> String
requestLine srcs = "lane-shapes " <> writeJSON srcs

-- | The sources not yet answered (blank ones need no answer).
missing :: LaneShapes -> Array String -> Array String
missing shapes srcs = nub (filter (\s -> trim s /= "" && not (Map.member s shapes)) srcs)

readShapes :: String -> Maybe LaneShapes
readShapes msg = do
  json <- stripPrefix (Pattern "lane-shapes ") msg
  obj :: Object LaneShape <- hush (readJSON json)
  pure (Map.fromFoldable (Object.toUnfoldable obj :: Array (Tuple String LaneShape)))

meterOf :: LaneShapes -> String -> Int
meterOf shapes src = maybe 1 (max 1 <<< _.meter) (Map.lookup src shapes)

cellMaskOf :: LaneShapes -> String -> Array Boolean
cellMaskOf shapes src = maybe [] _.mask (Map.lookup src shapes)

onsetsOf :: LaneShapes -> String -> Array Number
onsetsOf shapes src = maybe [] _.onsets (Map.lookup src shapes)

namedOnsetsOf :: LaneShapes -> String -> Array { name :: String, at :: Number }
namedOnsetsOf shapes src = maybe [] _.named (Map.lookup src shapes)
