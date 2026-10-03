-- | **Review cues from the live-coding station** (docs/kb/plans/text-on-the-stage.md,
-- | slice 2). Limulus evaluates `odonus $ mark`, `vetula $ loop 2`, … ; the rig
-- | relays each as `cue {"slot":…,"cue":…,"n":…}` to every page, and the page of
-- | that machine does what its own Review controls do: drop a mark, loop a
-- | mark's region, stop the loop.
module Triggerfish.Cue
  ( Cue(..)
  , module Reexport
  , readCue
  ) where

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, toMaybe)
import Data.String (Pattern(..), stripPrefix)
import Simple.JSON (readJSON)
import Triggerfish.Capture.Logbook (Reshape(..))
import Triggerfish.Capture.Logbook (Reshape(..)) as Reexport

-- | `LoopCue n` counts marks from 1; 0 is the latest. `WindowCue` moves or
-- | stretches the loop window (`slide`, `widen`, `narrow`, in bars).
data Cue = MarkCue | LoopCue Int | StopCue | WindowCue Reshape

-- | A cue for the machine whose slot is given (`odonus`, `vetula`), or Nothing.
readCue :: String -> String -> Maybe Cue
readCue slot msg = do
  json <- stripPrefix (Pattern "cue ") msg
  c :: { slot :: String, cue :: String, n :: Nullable Int, by :: Nullable Number } <- hush (readJSON json)
  if c.slot /= slot then Nothing
  else case c.cue of
    "mark" -> Just MarkCue
    "loop" -> Just (LoopCue (fromMaybe 0 (toMaybe c.n)))
    "stop" -> Just StopCue
    "slide" -> WindowCue <<< Slide <$> toMaybe c.by
    "widen" -> WindowCue <<< Widen <$> toMaybe c.by
    "narrow" -> WindowCue <<< Widen <<< negate <$> toMaybe c.by
    _ -> Nothing
