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
  -- Maybe, not Nullable: simple-json reads a missing key as Nothing only for
  -- Maybe, and each cue carries only its own fields (`mark` has none)
  c :: { slot :: String, cue :: String, n :: Maybe Int, by :: Maybe Number
       , slide :: Maybe Number, widen :: Maybe Number } <- hush (readJSON json)
  if c.slot /= slot then Nothing
  else case c.cue of
    "mark" -> Just MarkCue
    "loop" -> Just (LoopCue (fromMaybe 0 c.n))
    "stop" -> Just StopCue
    "slide" -> WindowCue <<< Slide <$> c.by
    "widen" -> WindowCue <<< Widen <$> c.by
    "narrow" -> WindowCue <<< Widen <<< negate <$> c.by
    -- a pattern's value, from the rig (window_patterns)
    "place" -> Just (WindowCue (Place { slide: c.slide, widen: c.widen }))
    _ -> Nothing
