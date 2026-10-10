-- | The calibration tables a polyphonic instrument's voices are corrected by,
-- | picked out of Amphora's `vco-calibrations` collection.
-- |
-- | Playing a polyphonic instrument (the Saïch, Rings, the Rample as one) is
-- | the rig's: the allocator and the voltages are `Reef.Articulation`'s, and
-- | the page hands the rig these tables with the routing, so it needs no
-- | lookup of its own (docs/kb/plans/hardware-through-the-rig.md).
module Triggerfish.Poly
  ( tablesFor
  ) where

import Prelude

import Data.Array (findMap)
import Data.Maybe (Maybe(..))
import Reef.Calibration (Table)
import Triggerfish.Amphora (LibItem)

-- | Pick each voice's calibration table out of an Amphora `vco-calibrations`
-- | fetch, by label.
-- |
-- | A missing label yields `Nothing` rather than a failure: an uncalibrated
-- | voice is a normal state on a rig where modules move, and refusing to play
-- | would be a worse answer than playing slightly out of tune. The caller can
-- | see which are missing and say so.
tablesFor :: Array String -> Array LibItem -> Array (Maybe Table)
tablesFor labels items = map find labels
  where
  find label = findMap (match label) items
  match label it =
    if it.name == label then Just { label, points: parsePoints it.payload }
    else Nothing

-- | Calibration payloads are the canonical JSON DeepStar writes. Parsing lives
-- | in FFI because the payload is a string of arbitrary JSON and reef's codec
-- | would need the whole table schema to read four fields of it.
foreign import parsePoints :: String -> Array { volts :: Number, hz :: Number }
