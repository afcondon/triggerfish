-- | The Odonus grid on a Midifighter Twister: what each encoder reads, and
-- | what its ring should show.
-- |
-- | The device is Helm's (`Helm.Twister`); this is the binding. Layer 0 is the
-- | sixteen cells, read through whichever field the top two rows' pushes have
-- | selected; layer 1 is the four voices, a row each.
-- |
-- | ## The rings are painted from the model
-- |
-- | Never from the turn that caused them, so a value changed by the trackpad,
-- | by the rig, or by switching field shows on the device too, and the next
-- | turn continues from where the value really is rather than jumping.
-- |
-- | **But not under a moving hand.** The encoder has 128 positions and LEN has
-- | eight values, so a ring repainted to "where LEN 3 sits" lands a few clicks
-- | away from where the hand left it — and a slow turn, one click at a time,
-- | is then snapped back on every click and can never leave. So an encoder
-- | whose last reported position already *means* the model's value is left
-- | alone; only a value that has moved for some other reason repaints.
-- |
-- | Writing a ring also sets the device's stored position, on whichever layer
-- | it addresses whether or not it is showing — the Twister keeps a value per
-- | encoder per layer (`Helm.Layers`) — so both layers are painted always.
module Triggerfish.Odonus.Twister
  ( Reading(..)
  , reading
  , twisterScale
  , means
  , ringOf
  , paint
  ) where

import Prelude

import Data.Array (catMaybes, find, length, range, (!!))
import Data.Int (round, toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Helm.Twister as HT
import Reef.Gen (GenKind(..), rateMax)
import Triggerfish.Odonus.Grid.Types (State, TwisterField(..))
import Triggerfish.Odonus.Model as M

-- | What an encoder stands for right now.
data Reading
  -- | An integer in `lo..hi`, spread over the whole turn.
  = Range Int Int Int
  -- | A switch: right of centre is on.
  | Flag Boolean

-- | Map an absolute 0..127 encoder onto an integer range [lo,hi].
twisterScale :: Int -> Int -> Int -> Int
twisterScale lo hi d2 = lo + round (toNumber d2 / 127.0 * toNumber (hi - lo))

-- | Whether a position on the device already says this value.
means :: Int -> Reading -> Boolean
means pos = case _ of
  Range lo hi v -> twisterScale lo hi pos == v
  Flag b -> (pos >= 64) == b

-- | Where the ring sits for a value.
ringOf :: Reading -> Int
ringOf = case _ of
  Range lo hi v
    | hi <= lo -> 0
    | otherwise -> clamp 0 127 (round (toNumber (v - lo) / toNumber (hi - lo) * 127.0))
  Flag b -> if b then 127 else 0

-- | The encoder at `knob`, read from the state. The ranges are the ones the
-- | turn handlers in `Triggerfish.Odonus.Grid` scale into; `Nothing` is an
-- | encoder with no value to show (a one-shot, or unbound).
reading :: State -> HT.Knob -> Maybe Reading
reading st k = case k.layer of
  0 -> case st.twisterField of
    FNote -> cell \c -> Range 0 M.knobMax c.note
    FLen -> cell \c -> Range 1 8 c.dur
    FRatchet -> cell \c -> Range 1 8 c.ratchet
    FVel -> cell \c -> Range 1 127 c.vel
    FGate -> cell \c -> Flag c.gate
    FSkip -> cell \c -> Flag c.skip
    FGlide -> cell \c -> Flag c.glide
    FMacro -> macro
  1 -> do
    h <- st.odo.heads !! (k.index / 4)
    case k.index `mod` 4 of
      0 -> Just (Range 0 (length M.patternLibrary - 1) h.patternIx)
      1 -> Just (Range 0 16 h.pulses)
      2 -> Just (Range 1 16 h.esteps)
      _ -> Just (Range (-24) 24 h.transp)
  _ -> Nothing
  where
  cell f = f <$> st.odo.cells !! k.index
  notes = find (\g -> g.kind == GNotes) st.gen
  macro = case k.index of
    0 -> Just (Range (-2) 2 st.odo.octaveShift)
    1 -> Just (Range 0 6 st.odo.degShift)
    2 -> Just (Range 0 127 (round (st.genBias * 127.0)))
    3 -> Just (Range 0 127 (round (st.genSpread * 127.0)))
    4 -> Flag <<< _.on <$> notes
    5 -> Range 0 100 <<< _.amt <$> notes
    6 -> Range 0 rateMax <<< _.rate <$> notes
    8 -> Just (Range 0 (length M.speedTable - 1) st.odo.clockIx)
    9 -> Just (Range 10 200 st.odo.gatePct)
    10 -> Just (Range 0 60 (round (st.swing * 100.0)))
    11 -> Just (Range 0 40 st.velHumanize)
    _ -> Nothing

-- | The ring writes this frame needs: every encoder on
-- | both layers whose ring should differ from what was last written. `heard`
-- | is the device's last reported position per CC, `lit` the last written.
paint :: State -> { heard :: Map Int Int, lit :: Map Int Int } -> Array { knob :: HT.Knob, cc :: Int, pos :: Int }
paint st { heard, lit } = catMaybes (map one knobs)
  where
  knobs = do
    layer <- [ 0, 1 ]
    index <- range 0 (HT.encodersPerLayer - 1)
    pure { layer, index }
  one k = do
    r <- reading st k
    let cc = HT.knobCC k
        want = case Map.lookup cc heard of
          Just pos | means pos r -> pos
          _ -> ringOf r
    if Map.lookup cc lit == Just want then Nothing else Just { knob: k, cc, pos: want }
