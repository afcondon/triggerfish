-- | The Odonus grid's Twister rings (`Triggerfish.Odonus.Twister`).
module Test.TwisterSpec (runTwisterTests) where

import Prelude

import Data.Array (all, filter, (..))
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert')
import Triggerfish.Odonus.Twister (Reading(..), means, ringOf, twisterScale)

-- | Every range the grid binds, as `{ lo, hi }`.
ranges :: Array { lo :: Int, hi :: Int }
ranges =
  [ { lo: 0, hi: 255 }, { lo: 1, hi: 8 }, { lo: 1, hi: 127 }, { lo: 0, hi: 4 }
  , { lo: 0, hi: 16 }, { lo: 1, hi: 16 }, { lo: -24, hi: 24 }, { lo: -2, hi: 2 }
  , { lo: 0, hi: 6 }, { lo: 0, hi: 100 }, { lo: 10, hi: 200 }, { lo: 0, hi: 60 }
  , { lo: 0, hi: 40 }
  ]

runTwisterTests :: Effect Unit
runTwisterTests = do
  -- A ring painted for a value reads back as that value, wherever the range
  -- fits in the encoder's 128 positions. NOTE's 0..255 does not, so a note
  -- set elsewhere lands on the nearest position; `lit` is what keeps that
  -- from being rewritten every frame.
  assert' "a painted ring means its value" $
    all (\r -> all (\v -> means (ringOf (Range r.lo r.hi v)) (Range r.lo r.hi v)) (r.lo .. r.hi))
      (filter (\r -> r.hi - r.lo <= 127) ranges)
  -- Every position the hand can leave the knob at means *something*, so a
  -- turn the model has taken in is never repainted under the hand.
  assert' "the hand's position means the value it set" $
    all (\r -> all (\p -> means p (Range r.lo r.hi (twisterScale r.lo r.hi p))) (0 .. 127)) ranges
  assert' "switches: right of centre is on" $
    means 64 (Flag true) && means 63 (Flag false) && ringOf (Flag true) == 127
  log "PASS - Twister rings: painted positions read back, hand positions are kept"
