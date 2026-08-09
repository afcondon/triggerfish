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
-- |   * VELOCITY RESPONSE is a spike at the peak: a vertical mark spanning the
-- |     peak's range, from where a velocity-1 note lands to where a velocity-127
-- |     one does. A static envelope has no spike at all; a fully velocity-scaled
-- |     one has a tall one. So you read not just "does velocity do something" but
-- |     how much
-- |
-- |     (This replaced a filled BAND between the velocity-1 and velocity-127
-- |     outlines. The band was honest in the channel it intended — its height at
-- |     the peak was exactly the velocity range — but you perceive a filled
-- |     shape by its AREA, and area is width × height, so a band's apparent
-- |     strength was confounded with the envelope's duration. `ramp` and `knock`
-- |     carry the same `vel 96` and had the same 29px separation at the peak,
-- |     but ramp's band was 94px wide and knock's 13.6px, so one read as
-- |     velocity-sensitive and the other as static. Worst exactly where it
-- |     mattered most: percussive shapes are the most velocity-sensitive
-- |     musically and the narrowest on screen. A one-dimensional mark for a
-- |     one-dimensional quantity has no width to be confounded by.)
-- |   * DURATION is the curve's horizontal extent, log-scaled — a click occupies
-- |     a fifth of its cell, a drone fills it
-- |
-- | Only the time-bucket COLOUR is conventional, and it is redundant with the
-- | extent rather than carrying anything on its own.
module Triggerfish.Selene.EnvDraw
  ( Pt
  , Spike
  , Figure
  , figure
  , durationMs
  , bucketMs
  , timeInk
  ) where

import Prelude

import Data.Array ((!!))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (log)
import Data.Ord (abs)

import Triggerfish.Selene.Model (EnvSlot)

type Pt = { x :: Number, y :: Number }

-- | The velocity mark: a bar HANGING FROM THE PEAK, whose length is how far
-- | velocity moves that peak — the gap between where a velocity-1 note lands and
-- | where a velocity-127 one does. `from` is the peak itself, `to` the far end.
-- |
-- | For the ordinary response (`velDepth > 64`) this is geometrically exact: a
-- | quiet note really does peak that far below a loud one, so the bar covers the
-- | interval the peak actually occupies.
-- |
-- | For an INVERSE response (`velDepth < 64`) it is a magnitude rather than a
-- | picture — the bar still hangs down, though the quiet peak is really *above*
-- | the loud one. There is nowhere to draw it truthfully: a full-depth curve
-- | already reaches the top of the box, and an inverse response asks for up to
-- | twice full depth, so `soft` would need to reach 12 px above a 44 px cell and
-- | `velDepth 0` would need 30. Buying that headroom means drawing every normal
-- | envelope at half height to flatter the rarest case. So direction is the
-- | thing this mark does not carry; the numbers under the cell do.
type Spike = { x :: Number, from :: Number, to :: Number }

type Figure =
  { curve :: Array Pt     -- the shape at velocity 127 — what the numbers describe
  , vel :: Maybe Spike    -- Nothing when velDepth is 64, i.e. velocity does nothing
  , baseline :: Number    -- y of zero volts, in box coordinates
  , ink :: String         -- time-bucket colour
  , durationMs :: Number
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
  { curve: shapeAt hiScale
  , vel: if hasVel then Just spike else Nothing
  , baseline
  , ink: timeInk sl.timeRange
  , durationMs: dur
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

  -- `velDepth` likewise: 64 means velocity does nothing, so a quiet note and a
  -- loud one land in the same place and there is no spike to draw. Above 64, low
  -- velocity shrinks the envelope; below 64 the response inverts and low
  -- velocity makes it LARGER, which is why `quiet` can legitimately sit outside
  -- the curve rather than inside it.
  velK = (toNumber sl.velDepth - 64.0) / 63.0
  hiScale = 1.0
  loScale = 1.0 - velK
  hasVel = abs velK > 0.01

  -- The velocity bar runs from the peak to where a velocity-1 note puts it. That
  -- is a real position, so draw it there whenever it fits — which covers every
  -- ordinary response, and also the INVERTED envelopes, whose quiet peak is less
  -- negative and therefore sits back toward the baseline rather than further
  -- from it. ("Hang it downward" is wrong for those: it points away from where
  -- the peak actually moves, and runs off the bottom of the box.)
  --
  -- It fails to fit in exactly one situation: an inverse response on a
  -- deep positive envelope, where the quiet peak exceeds full depth and there is
  -- no headroom above a curve already touching the top. Then, and only then, the
  -- bar is mirrored about the peak — same length, opposite side, guaranteed to
  -- fit because the mirror points back toward the baseline. See `Spike`.
  spike =
    let peakY = yFor hiScale
        quietY = yFor loScale
        fits = quietY >= pad && quietY <= box.h - pad
    in { x: x1
       , from: peakY
       , to: if fits then quietY else peakY + abs (quietY - peakY)
       }

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

  shapeAt k =
    let peak = yFor (1.0 * k)
        sus = yFor ((toNumber sl.sustain / 127.0) * k)
    in [ { x: x0, y: baseline }
       , { x: x1, y: peak }
       , { x: x2, y: sus }
       , { x: x3, y: sus }
       , { x: x4, y: baseline }
       ]
