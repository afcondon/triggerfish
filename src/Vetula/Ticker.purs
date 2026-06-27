-- | A bare `setInterval` ticker for the Performance transport — a self-contained
-- | clock for the standalone prototype (the rig-locked Binnacle clock arrives
-- | when this folds into Triggerfish). `startTicker ms cb` calls `cb` every `ms`
-- | and returns a canceller.
module Vetula.Ticker (startTicker) where

import Data.Unit (Unit)
import Effect (Effect)

foreign import startTicker :: Int -> Effect Unit -> Effect (Effect Unit)
