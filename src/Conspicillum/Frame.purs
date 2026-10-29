-- | The animation clock: every frame the browser draws, with its timestamp in
-- | milliseconds (`performance.now`'s clock), until cancelled.
module Conspicillum.Frame (frames) where

import Prelude

import Effect (Effect)
import Halogen.Subscription as HS

foreign import onFrames :: (Number -> Effect Unit) -> Effect (Effect Unit)

frames :: HS.Emitter Number
frames = HS.makeEmitter onFrames
