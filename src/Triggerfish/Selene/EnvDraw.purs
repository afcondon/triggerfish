-- | `Triggerfish.Selene.EnvDraw` — the geometry of a drawn envelope.
-- |
-- | Pure: takes a slot and a box, returns points and a colour. No Halogen, so
-- | the library wall and the in-rack slot cell render from ONE figure rather
-- | than two drawings that agree until they don't.
-- |
-- | The encoding is deliberately mostly **depiction** — the mark shows the thing
-- | rather than standing for it (see `docs/DESIGN-envelope-machine.md`):
-- |
-- |   * the SHAPE is the curve
-- |   * DEPTH is the curve's height; a depth of 64 is flat, because a depth of
-- |     64 genuinely does nothing
-- |   * INVERSION (depth < 64) draws BELOW the baseline, which is what it does
-- |   * VELOCITY RESPONSE is a band: the same shape at velocity 1 and at 127,
-- |     with the area between them filled. A static envelope is a single line; a
-- |     fully velocity-scaled one is a wide band. So you read not just "does
-- |     velocity do something" but how much
-- |   * DURATION is the curve's horizontal extent, log-scaled — a click occupies
-- |     a fifth of its cell, a drone fills it
-- |
-- | Only the time-bucket COLOUR is conventional, and it is redundant with the
-- | extent rather than carrying anything on its own.
module Triggerfish.Selene.EnvDraw
  ( Pt
  , Figure
  , figure
  , durationMs
  , bucketMs
  , timeInk
  ) where

import Prelude

import Data.Array ((!!), reverse)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Data.Number (log)
import Data.Ord (abs)

import Triggerfish.Selene.Model (EnvSlot)

type Pt = { x :: Number, y :: Number }

type Figure =
  { hi :: Array Pt        -- the shape at velocity 127
  , lo :: Array Pt        -- the shape at velocity 1
  , band :: Array Pt      -- closed polygon between them (empty when velDepth is 64)
  , baseline :: Number    -- y of zero volts, in box coordinates
  , ink :: String         -- time-bucket colour
  , durationMs :: Number
  , hasBand :: Boolean
  }

-- | The firmware's eight time buckets, in ms of full-scale.
bucketMs :: Int -> Number
bucketMs n = fromMaybe 1000.0 ([ 200.0, 500.0, 1000.0, 2000.0, 5000.0, 10000.0, 20000.0, 50000.0 ] !! n)

-- | Roughly how long this envelope lasts. Attack, decay and release are times
-- | scaled by the bucket; SUSTAIN is not a time — it is a level held for as long
-- | as the gate — so it is excluded. Floored so a pure gate (a=d=r=0) still has a
-- | duration to place on the log axis.
durationMs :: EnvSlot -> Number
durationMs sl =
  let stages = toNumber (sl.attack + sl.decay + sl.release) / 127.0
  in max 8.0 (stages * bucketMs sl.timeRange)

-- | An ORDERED single-hue ramp: bright and light for the 200 ms bucket, deep and
-- | dark for 50 s. Ordered data wants an ordered scale — a categorical palette
-- | here would imply the buckets are unrelated kinds when they are a scale.
-- | Redundant with the horizontal extent, deliberately: two channels carrying one
-- | fact is legible, one channel carrying two is not.
timeInk :: Int -> String
timeInk n = fromMaybe "#7d4f20"
  ([ "#e0a83c", "#cf8f2e", "#ba7726", "#a26221", "#874e1e", "#6b3c1a", "#4e2c15", "#331d10" ] !! n)

-- | Build the figure inside a `w × h` box.
figure :: { w :: Number, h :: Number } -> EnvSlot -> Figure
figure box sl =
  { hi: curve hiScale
  , lo: curve loScale
  , band: if hasBand then curve hiScale <> reverse (curve loScale) else []
  , baseline
  , ink: timeInk sl.timeRange
  , durationMs: dur
  , hasBand
  }
  where
  pad = 3.0
  usableW = max 1.0 (box.w - pad * 2.0)

  -- Zero volts sits at 75% of the box, not at the middle: positive envelopes are
  -- the common case and get three quarters of the height, inverted ones the
  -- remaining quarter. Splitting evenly would halve the resolution of the shape
  -- you draw most often to flatter the one you draw least.
  baseline = box.h * 0.75
  upH = baseline - pad
  downH = box.h - pad - baseline

  -- `depth` is an ATTENUVERTER: 64 is zero, above is positive, below inverts.
  amp = (toNumber sl.depth - 64.0) / 63.0

  -- `velDepth` likewise: 64 means velocity does nothing, so the two curves
  -- coincide and there is no band to draw. Above 64, low velocity shrinks the
  -- envelope; below 64 the response inverts and low velocity makes it LARGER,
  -- which is why `lo` can legitimately sit outside `hi`.
  velK = (toNumber sl.velDepth - 64.0) / 63.0
  hiScale = 1.0
  loScale = 1.0 - velK
  hasBand = abs velK > 0.01

  dur = durationMs sl

  -- Horizontal extent is log-scaled duration: the span reaches ~150 s from ~8 ms,
  -- so a linear axis would collapse every percussive shape to a hairline.
  -- Floored so the shortest is still a shape rather than a tick.
  extent =
    let lo' = log 8.0
        -- Top of the scale is 30s, not the theoretical 150s maximum. Mapping the
        -- full possible range spent more than half the visual width on shapes
        -- longer than two seconds, which are rare — so everything percussive,
        -- which is most of what gets used, was crushed into the first fifth.
        -- Anything past 30s simply pins at full width; there is nothing to
        -- distinguish up there anyway.
        hi' = log 30000.0
        t = (log dur - lo') / (hi' - lo')
    in usableW * min 1.0 (max 0.2 t)

  -- Stage widths in proportion to their times, plus a hold segment so a
  -- sustaining envelope reads as sustaining. A pure gate (a=d=r=0, s=127)
  -- degenerates to hold alone, i.e. a rectangle — which is exactly what it is.
  aT = toNumber sl.attack
  dT = toNumber sl.decay
  rT = toNumber sl.release
  hold = if sl.sustain > 0 then max 18.0 ((aT + dT + rT) * 0.25) else 0.0
  total = max 1.0 (aT + dT + rT + hold)

  x0 = pad
  x1 = x0 + extent * (aT / total)
  x2 = x1 + extent * (dT / total)
  x3 = x2 + extent * (hold / total)
  x4 = x3 + extent * (rT / total)

  -- Height above (or below) the baseline for a normalised level 0..1.
  yFor lvl =
    let v = amp * lvl
    in if v >= 0.0 then baseline - v * upH else baseline - v * downH

  curve k =
    let peak = yFor (1.0 * k)
        sus = yFor ((toNumber sl.sustain / 127.0) * k)
    in [ { x: x0, y: baseline }
       , { x: x1, y: peak }
       , { x: x2, y: sus }
       , { x: x3, y: sus }
       , { x: x4, y: baseline }
       ]
