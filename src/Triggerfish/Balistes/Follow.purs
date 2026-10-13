-- | Triggerfish.Balistes.Follow — what the rig says as it plays a
-- | `balistes $ "<a b>"` line, so the page can show the beat that is playing.
-- |
-- | The rig's Balistes voice announces each event of the line ahead of the
-- | step it begins on (`reef_balistes_voice`):
-- |
-- |   balistes-now <step> <word>           a Rytm rhythm, from that step
-- |   balistes-now <step> <word> <state>   a Grids beat, its engine from there
-- |   balistes-now <step> ~                a rest
-- |
-- | Evaluating the line is announced too (`played balistes line <pattern>`),
-- | and a hush ends it.
module Triggerfish.Balistes.Follow
  ( Now
  , parseNow
  , endsLine
  , lineOf
  ) where

import Prelude

import Data.Either (hush)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), indexOf, splitAt, stripPrefix)
import Reef.Balistes.Protocol (decodeBalSim)
import Reef.Balistes.Sim (BalSim)

-- | From sixteenth `step`, the line plays `word` ("~" for a rest); `grids` is
-- | the engine state a Grids beat plays on from.
type Now = { step :: Int, word :: String, grids :: Maybe BalSim }

parseNow :: String -> Maybe Now
parseNow msg = do
  rest <- stripPrefix (Pattern "balistes-now ") msg
  { before: stepText, after: afterStep } <- cut rest
  step <- Int.fromString stepText
  case cut afterStep of
    Just { before: word, after: state } -> do
      grids <- hush (decodeBalSim state)
      pure { step, word, grids: Just grids }
    Nothing -> pure { step, word: afterStep, grids: Nothing }
  where
  cut s = indexOf (Pattern " ") s <#> \i ->
    let parts = splitAt i s in { before: parts.before, after: (splitAt 1 parts.after).after }

-- | The pattern of a line the rig has started playing (`played balistes
-- | line <pattern>`, broadcast when a `balistes $` line is evaluated).
lineOf :: String -> Maybe String
lineOf = stripPrefix (Pattern "played balistes line ")

-- | Whether a frame from the rig ends the line: the rig hushed, all of it or
-- | Balistes, or a page stopped the Balistes voice.
endsLine :: String -> Boolean
endsLine msg = msg == "hushed" || msg == "hushed balistes" || msg == "stopped balistes"
