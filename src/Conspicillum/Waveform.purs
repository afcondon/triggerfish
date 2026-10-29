-- | **What each box of the strip sounds like.** A segment of the material is a
-- | stretch of one sample: a whole sample in a corpus, a bar of a tape, a hit
-- | played as a bar. So its waveform is that stretch of that sample's peak
-- | envelope, and the same is true of the loupe, which is one segment at full
-- | width.
module Conspicillum.Waveform
  ( Source
  , sources
  , stretch
  , loudest
  , outline
  ) where

import Prelude

import Data.Array (drop, length, mapWithIndex, reverse, take, (..))
import Data.Foldable (foldl, intercalate, maximum)
import Data.Int (ceil, floor, toNumber)
import Data.Maybe (fromMaybe)
import Conspicillum.Corpora (SampleSet, envelopeOf)
import Reef.Conspicillum.Decimal (fixed)
import Reef.Conspicillum.Display (Material(..))

-- | A segment's audio: which sample, and which part of it, as fractions.
type Source = { sample :: Int, from :: Number, to :: Number }

sources :: Material -> Array Source
sources = case _ of
  HitsAsBars keys -> keys <#> \k -> { sample: k, from: 0.0, to: 1.0 }
  WholeTape t ->
    let bars = max 1 t.bars
    in (0 .. (bars - 1)) <#> \b ->
      { sample: t.sample.index, from: toNumber b / toNumber bars, to: toNumber (b + 1) / toNumber bars }
  OneSample s -> [ { sample: s.index, from: 0.0, to: 1.0 } ]
  ManySamples ss -> ss <#> \s -> { sample: s.index, from: 0.0, to: 1.0 }

-- | The envelope's bins for one source, 0..255.
stretch :: SampleSet -> Source -> Array Int
stretch set source =
  let
    whole = envelopeOf set source.sample
    n = toNumber (length whole)
    first = floor (source.from * n)
    past = max (first + 1) (ceil (source.to * n))
  in
    take (past - first) (drop first whole)

-- | The loudest bin across some stretches, so a set is drawn against its own
-- | peak: quiet sets stay legible, and loud and quiet samples within a set
-- | stay loud and quiet.
loudest :: Array (Array Int) -> Int
loudest stretches = max 1 (foldl max 0 (stretches <#> \s -> fromMaybe 0 (maximum s)))

-- | A closed outline, mirrored about a centre line, of an envelope laid over
-- | [x0, x1]. Each bin is a flat step, so a short hit's few bins still read as
-- | a shape rather than a spike.
outline :: { x0 :: Number, x1 :: Number, centre :: Number, half :: Number, peak :: Int } -> Array Int -> String
outline box bins
  | length bins == 0 = ""
  | otherwise =
      let
        count = toNumber (length bins)
        width = (box.x1 - box.x0) / count
        height v = box.half * toNumber v / toNumber box.peak
        top = bins # mapWithIndex \i v ->
          let x = box.x0 + toNumber i * width
          in [ point x (box.centre - height v), point (x + width) (box.centre - height v) ]
        bottom = reverse bins # mapWithIndex \j v ->
          let x = box.x0 + (count - toNumber j) * width
          in [ point x (box.centre + height v), point (x - width) (box.centre + height v) ]
      in
        "M" <> intercalate " L" (join top <> join bottom) <> " Z"
  where
  point x y = fixed 1 x <> "," <> fixed 1 y
