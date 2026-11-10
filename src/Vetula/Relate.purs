-- | `Vetula.Relate` — interval-class primitives: how rough a set of pitch
-- | classes sounds, which `Vetula.Generate` scores its candidates by.
module Vetula.Relate
  ( intervalClass
  , icWeight
  , roughness
  , allPairs
  ) where

import Prelude

import Data.Array (concat, drop, length, mapWithIndex)
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))

-- ---------------------------------------------------------------------------
-- Interval-class primitives (`roughness` also scores Generate's candidates)
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
