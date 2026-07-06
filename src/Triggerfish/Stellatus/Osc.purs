-- | The Stellatus → SuperDirt emit seam. `fire url json` POSTs one event to the
-- | OSC bridge (audio/stellatus-bridge.mjs), which relays it to SuperDirt as a
-- | `/dirt/play` message. Fire-and-forget — a dev-only audition path (the ship
-- | path stays BEAM, per docs/SUFFLAMEN-DESIGN.md decision 3).
module Triggerfish.Stellatus.Osc (fire) where

import Prelude (Unit)
import Effect (Effect)

foreign import fireImpl :: String -> String -> Effect Unit

-- | `fire bridgeUrl jsonBody` — POST one event; never blocks, never throws.
fire :: String -> String -> Effect Unit
fire = fireImpl
