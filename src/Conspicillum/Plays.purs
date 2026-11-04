-- | **What a sampler's pattern holds, as the rig says.** A line's `n "…"` is
-- | Tidal, and no page evaluates Tidal (docs/kb/plans/gpl-boundary-review.md),
-- | so the page asks the rig for a run of cycles and draws what comes back:
-- | the same plays the voice sounds.
module Conspicillum.Plays
  ( Answer
  , readAnswer
  , ask
  , inCycle
  , covers
  ) where

import Prelude

import Data.Array (index)
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Nullable (Nullable, toMaybe)
import Data.String as String
import Reef.Conspicillum.Cloud (Play)
import Simple.JSON (readJSON)

-- | `from` is the cycle `cycles` starts at.
type Answer = { pattern :: String, from :: Int, cycles :: Array (Array Play), problem :: Maybe String }

-- | A `conspicillum-plays …` line from the rig, if that is what this is.
readAnswer :: String -> Maybe Answer
readAnswer line = do
  json <- String.stripPrefix (String.Pattern "conspicillum-plays ") line
  a :: { pattern :: String, from :: Int, cycles :: Array (Array Play), problem :: Nullable String } <- hush (readJSON json)
  pure a { problem = toMaybe a.problem }

ask :: { from :: Int, count :: Int, pattern :: String } -> String
ask r = "conspicillum-plays " <> show r.from <> " " <> show r.count <> " " <> String.trim r.pattern

mine :: String -> Answer -> Boolean
mine pattern a = a.pattern == String.trim pattern

-- | This cycle's plays, or none while the rig has not said.
inCycle :: String -> Int -> Maybe Answer -> Array Play
inCycle pattern cycle answer = fromMaybe [] do
  a <- answer
  if mine pattern a then index a.cycles (cycle - a.from) else Nothing

-- | Whether there is nothing more to ask about this cycle: the answer reaches
-- | it, or says the pattern can't be read.
covers :: String -> Int -> Maybe Answer -> Boolean
covers pattern cycle = case _ of
  Nothing -> false
  Just a -> mine pattern a && (isJust a.problem || isJust (index a.cycles (cycle - a.from)))
