-- | **The Link session's health**, as the rig reports it for the Dashboard:
-- | how many other peers share the session (Diaphus's count), and how many
-- | anchors each receiver has refused. Every count should read zero
-- | (binnacle/conformance/LINK-ANCHOR.md); one that does not is a clock that
-- | will not lock, said before it is heard.
module Triggerfish.LinkHealth
  ( LinkHealth
  , read
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), stripPrefix)
import Data.String as String
import Data.Traversable (traverse)

-- | A receiver's count is `Nothing` when it did not answer.
type LinkHealth = { peers :: Int, refused :: Array { receiver :: String, count :: Maybe Int } }

-- | A `link <peers> <receiver>=<refused> ...` line (Architeuthis's
-- | `tidal_link_anchor:format_link/2`), `-` for a receiver that did not answer.
read :: String -> Maybe LinkHealth
read line = do
  rest <- stripPrefix (Pattern "link ") line
  { head, tail } <- Array.uncons (String.split (Pattern " ") rest)
  peers <- Int.fromString head
  refused <- traverse receiverCount tail
  pure { peers, refused }
  where
  receiverCount word = case String.split (Pattern "=") word of
    [ receiver, n ] -> Just { receiver, count: Int.fromString n }
    _ -> Nothing
