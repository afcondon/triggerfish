-- | A MONO PITCH LINE on the ES-9: one head of notes driving one VCO through a
-- | pitch jack and a gate jack, the way a TB-303 drives its own oscillator.
-- |
-- | What makes it more than "set a voltage per note" is the slide. A glide cell
-- | holds its note until the next one arrives (Odonus already keeps it in
-- | `headNote`), and when that next note comes:
-- |
-- |   * the pitch SLEWS to it over `slideMs` instead of stepping, and
-- |   * the gate is already high, so it stays high: no new edge, so the
-- |     envelope is not retriggered, and the two notes run together.
-- |
-- | That pair is the acid slide. A plain note steps its pitch and fires a gate
-- | of its own length, so consecutive plain notes retrigger.
-- |
-- | Mono, unlike `Triggerfish.Poly`: there is one oscillator and no allocator,
-- | so a later note always takes it. Pitch goes through the jack's measured
-- | calibration table when there is one (`RM.es9JackVco`), since an analogue
-- | VCO does not track 1 V/oct and a bass line drifts out of tune across two
-- | octaves without it.
module Triggerfish.Es9Line
  ( Line
  , lineFor
  , emit
  , release
  , slideMs
  ) where

import Prelude

import Data.Foldable (for_)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Timer (setTimeout)
import Binnacle.Output (cvOut, cvSlew, fireAt)
import Binnacle.Transport (Socket)
import Reef.Calibration (Table, realiseNote)
import Triggerfish.Amphora (LibItem)
import Triggerfish.Poly as Poly
import Triggerfish.Routing.Model as RM

-- | Where one line lands: es9-daemon buses for its pitch and gate, and the
-- | pitch jack's calibration table if one was found.
type Line =
  { pitchBus :: Int
  , gateBus :: Maybe Int
  , table :: Maybe Table
  }

-- | A destination's line, with its table picked out of the `vco-calibrations`
-- | fetch by the VCO its pitch jack reaches.
lineFor :: Array LibItem -> { jack :: Int, gate :: Int } -> Line
lineFor items d =
  { pitchBus: RM.es9JackBus d.jack
  , gateBus: if d.gate > 0 then Just (RM.es9JackBus d.gate) else Nothing
  , table: case RM.es9JackVco d.jack of
      Just label -> case Poly.tablesFor [ label ] items of
        [ t ] -> t
        _ -> Nothing
      Nothing -> Nothing
  }

-- | The slide's length. A 303 slides in a fixed time, about 60 ms, whatever
-- | the tempo, and that constancy is part of its sound.
slideMs :: Number
slideMs = 60.0

-- | es9-daemon's slew is a first-order smoother, so its `lag` is a TIME
-- | CONSTANT: a lag of 60 ms would cover only 63% of the interval in 60 ms.
-- | A third of the slide arrives within 5% of the note in `slideMs`.
slideLagSec :: Number
slideLagSec = slideMs / 3.0 / 1000.0

-- | And the lag is STICKY per bus: `/cv` sets a value without touching it, so
-- | after one slide every later note would glide too. A plain note therefore
-- | goes out as a slew at es9-daemon's own default (`DEFAULT_LAG_SEC`), which
-- | restores the bus as it was.
stepLagSec :: Number
stepLagSec = 0.005

-- | How long the pitch is given to settle before the gate rises, so the
-- | envelope never opens on the previous note's pitch. Sent in the same tick
-- | as the pitch and held by es9-daemon, so browser timer jitter is common to
-- | the pair (as `Poly`'s Rings strum does).
settleMs :: Number
settleMs = 2.0

-- | Play one note on the line.
-- |
-- | `prev` is the note this head is still holding from a glide cell, if any:
-- | its presence is what makes this note a slide. `glide` says whether THIS
-- | note holds on into the next.
emit
  :: Socket
  -> Line
  -> Number
  -> { atMs :: Number, pitch :: Int, prev :: Maybe Int, glide :: Boolean, gateMs :: Number }
  -> Effect Unit
emit sock line nowMs n = deferBy (n.atMs - nowMs) do
  let value = Poly.normalise (volts line n.pitch)
  case n.prev of
    -- slid into: glide the pitch (a tie when it is the same note)
    Just p
      | p /= n.pitch -> cvSlew sock { bus: line.pitchBus, value, lagSec: slideLagSec }
      | otherwise -> pure unit
    Nothing -> cvSlew sock { bus: line.pitchBus, value, lagSec: stepLagSec }
  for_ line.gateBus \bus ->
    if n.glide
      -- held into the next note; that note (or `release`) ends it
      then cvOut sock { bus, value: high }
      else case n.prev of
        -- already high from the held note: extend to this note's end, no edge
        Just _ -> fireAt sock { bus, value: high, durMs: n.gateMs, delayMs: 0.0 }
        -- a fresh note: pitch first, then the gate
        Nothing -> fireAt sock { bus, value: high, durMs: n.gateMs, delayMs: settleMs }
  where
  high = Poly.normalise Poly.gateVolts

-- | Close the line's gate: on stop, or when its head is muted, a held slide
-- | would otherwise leave the envelope open for ever.
release :: Socket -> Line -> Effect Unit
release sock line = for_ line.gateBus \bus -> cvOut sock { bus, value: 0.0 }

volts :: Line -> Int -> Number
volts line note = case line.table of
  Just t -> realiseNote t (toNumber note)
  Nothing -> Poly.nominalVolts note

deferBy :: Number -> Effect Unit -> Effect Unit
deferBy ms act
  | ms <= 1.0 = act
  | otherwise = void (setTimeout (round ms) act)
