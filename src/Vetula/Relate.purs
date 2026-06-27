-- | `Vetula.Relate` — pluggable chord-relation metrics.
-- |
-- | A `Metric` rates how a chord relates to a reference chord (the one currently
-- | sounding). The contract is uniform: **higher affinity = more related** — a
-- | smoother neighbour — so a single diverging colour ramp reads the same way for
-- | every metric (cool = closely related, warm = a striking move). That lets the
-- | collection light up by relatedness to whatever you're auditioning, and a warm
-- | bubble in another scale's pack is literally a key-change bridge.
-- |
-- | The metrics are values in a list, selected by key — add a new way of hearing
-- | "what goes with this?" by appending one row to `metrics`.
module Vetula.Relate
  ( Metric
  , metrics
  , noneKey
  , lookupMetric
  , rateAll
  , intervalClass
  , icWeight
  , roughness
  , allPairs
  ) where

import Prelude

import Data.Array (concat, concatMap, drop, filter, find, length, mapWithIndex, nub)
import Data.Foldable (elem, minimum, sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple(..))
import Vetula.Harmony (ChordNode)

type Metric =
  { key :: String
  , label :: String
  -- reference chord -> candidate -> affinity (higher = more related)
  , affinity :: ChordNode -> ChordNode -> Number
  }

metrics :: Array Metric
metrics =
  [ { key: "common", label: "notes in common", affinity: commonTones }
  , { key: "voicelead", label: "voice-leading move", affinity: voiceLeadAffinity }
  , { key: "succession", label: "smooth succession", affinity: successionAffinity }
  , { key: "fifths", label: "root by fifths", affinity: fifthsAffinity }
  ]

noneKey :: String
noneKey = "none"

lookupMetric :: String -> Maybe Metric
lookupMetric k = find (\m -> m.key == k) metrics

-- | The abstraction in one line: a metric, a reference chord, and a set of
-- | others → each other paired with its affinity to the reference.
rateAll :: Metric -> ChordNode -> Array ChordNode -> Array { chord :: ChordNode, score :: Number }
rateAll m ref = map \c -> { chord: c, score: m.affinity ref c }

-- ---------------------------------------------------------------------------
-- Metrics (all oriented so higher = more related / smoother)
-- ---------------------------------------------------------------------------

pcSet :: ChordNode -> Array Int
pcSet c = nub (map (\p -> mod p 12) c.pcs)

-- | How many pitch classes the two chords share.
commonTones :: ChordNode -> ChordNode -> Number
commonTones ref other =
  toNumber (length (filter (\p -> elem p (pcSet ref)) (pcSet other)))

-- | Smoothness of moving between the two note-sets: less total semitone motion
-- | is more related. (Symmetric nearest-tone distance — a cheap voice-leading
-- | proxy that doesn't need an optimal assignment.)
voiceLeadAffinity :: ChordNode -> ChordNode -> Number
voiceLeadAffinity ref other = negate (vlDistance (pcSet ref) (pcSet other))

vlDistance :: Array Int -> Array Int -> Number
vlDistance a b =
  let toNearest xs ys = sum (map (\x -> toNumber (nearest x ys)) xs)
  in (toNearest a b + toNearest b a) / 2.0

nearest :: Int -> Array Int -> Int
nearest x ys = fromMaybe 6 (minimum (map (pcDist x) ys))

pcDist :: Int -> Int -> Int
pcDist a b = let d = mod (absI (a - b)) 12 in min d (12 - d)

-- | How consonant the two chords' tones are against each other — a proxy for how
-- | smooth one sounds after the other. Less cross-roughness = more related.
successionAffinity :: ChordNode -> ChordNode -> Number
successionAffinity ref other = negate (crossRoughness (pcSet ref) (pcSet other))

crossRoughness :: Array Int -> Array Int -> Number
crossRoughness a b =
  let pairs = concatMap (\x -> map (Tuple x) b) a
  in if length pairs == 0 then 0.0
     else sum (map (\(Tuple x y) -> icWeight (intervalClass x y)) pairs) / toNumber (length pairs)

-- | Root motion around the circle of fifths — a fifth/fourth apart is closest.
fifthsAffinity :: ChordNode -> ChordNode -> Number
fifthsAffinity ref other = negate (toNumber (fifthsBetween ref.root other.root))

fifthsBetween :: Int -> Int -> Int
fifthsBetween a b = let s = mod ((b - a) * 7) 12 in min s (12 - s)

-- ---------------------------------------------------------------------------
-- Interval-class primitives (shared with the App's internal-dissonance fill)
-- ---------------------------------------------------------------------------

intervalClass :: Int -> Int -> Int
intervalClass a b = let d = mod (absI (a - b)) 12 in min d (12 - d)

icWeight :: Int -> Number
icWeight = case _ of
  1 -> 1.0
  2 -> 0.4
  3 -> 0.1
  4 -> 0.1
  6 -> 0.6
  _ -> 0.0

allPairs :: Array Int -> Array (Tuple Int Int)
allPairs xs = concat (mapWithIndex (\i a -> map (Tuple a) (drop (i + 1) xs)) xs)

roughness :: Array Int -> Number
roughness pcs =
  let pairs = allPairs pcs
  in if length pairs == 0 then 0.0
     else sum (map (\(Tuple a b) -> icWeight (intervalClass a b)) pairs) / toNumber (length pairs)

absI :: Int -> Int
absI n = if n < 0 then -n else n
