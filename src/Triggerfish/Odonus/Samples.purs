-- | **Odonus's patterns, sampled by the rig for a page that plays them
-- | itself.** The harmony and scale patterns are Tidal, and the browser never
-- | reads Tidal (docs/kb/plans/gpl-boundary-review.md). In Atlantis the rig's
-- | voice samples them and its `SetSampled` inputs arrive as reef-inputs. In
-- | Solo the page plays, so it asks the rig for the same samples ahead of
-- | time (`odonus-sample`) and applies each on its step, as the rig would.
-- |
-- | The answer is cached by `key`: the patterns and the step length it was
-- | sampled for. Any change to either is a new key, and samples for the old
-- | one are never applied. With no patterns at all nothing needs Tidal, so
-- | `localSample` gives that sample here.
module Triggerfish.Odonus.Samples
  ( Samples
  , noSamples
  , keyOf
  , hasPatterns
  , localSample
  , requestLine
  , readSamples
  , sampleAt
  , window
  , reaches
  ) where

import Prelude

import Data.Array (index, length)
import Data.Either (hush)
import Data.Maybe (Maybe(..), isJust)
import Data.Nullable (Nullable, toNullable)
import Data.String (Pattern(..), stripPrefix)
import Data.Traversable (traverse)
import Reef.Engine (Patterns, samplePatterns)
import Reef.Input (Input, WireInput, fromWire)
import Simple.JSON (readJSON, writeJSON)

-- | Samples for `count` steps from `from`, taken for the patterns `key`.
type Samples = { key :: String, from :: Int, inputs :: Array Input }

noSamples :: Samples
noSamples = { key: "", from: 0, inputs: [] }

-- | How many steps one request asks for, and how many before the end of
-- | what is held the next is asked.
window :: { count :: Int, margin :: Int }
window = { count: 64, margin: 16 }

keyOf :: Patterns -> Int -> String
keyOf p quarters = writeJSON (wire p quarters)

hasPatterns :: Patterns -> Boolean
hasPatterns p = isJust p.harmony || isJust p.scale || isJust p.outScale || isJust p.gridHarmony

-- | The sample with no pattern to read: what the rig would send then.
localSample :: Patterns -> Input
localSample = samplePatterns (const []) (const [])

wire
  :: Patterns
  -> Int
  -> { harmony :: Nullable String, scale :: Nullable String
     , outScale :: Nullable { pattern :: String, root :: Int }, gridHarmony :: Nullable String, quarters :: Int }
wire p quarters =
  { harmony: toNullable p.harmony, scale: toNullable p.scale
  , outScale: toNullable p.outScale, gridHarmony: toNullable p.gridHarmony, quarters }

-- | Ask for `window.count` steps from `from`; `quarters` is the step length in
-- | sixteenths of a cycle (Odonus's stepDiv).
requestLine :: Patterns -> Int -> Int -> String
requestLine p quarters from =
  let w = wire p quarters
  in "odonus-sample " <> writeJSON
       { key: keyOf p quarters, from, count: window.count, quarters
       , harmony: w.harmony, scale: w.scale, outScale: w.outScale, gridHarmony: w.gridHarmony }

-- | The rig's answer, `odonus-samples {key, from, inputs}`, or Nothing for
-- | any other frame.
readSamples :: String -> Maybe Samples
readSamples msg = do
  json <- stripPrefix (Pattern "odonus-samples ") msg
  r :: { key :: String, from :: Int, inputs :: Array WireInput } <- hush (readJSON json)
  inputs <- traverse fromWire r.inputs
  pure { key: r.key, from: r.from, inputs }

-- | The sample held for `step`, if these samples are for `key` and reach it.
sampleAt :: String -> Int -> Samples -> Maybe Input
sampleAt key step s
  | s.key /= key = Nothing
  | otherwise = index s.inputs (step - s.from)

-- | Whether these samples, for `key`, still reach `window.margin` steps past
-- | `step`; when not, it is time to ask for more.
reaches :: String -> Int -> Samples -> Boolean
reaches key step s = s.key == key && s.from <= step && step + window.margin < s.from + length s.inputs
