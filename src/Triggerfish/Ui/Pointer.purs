-- | Pointer-position helper for 2-D controls (the Marbles X-Y pad). Reads the
-- | mouse event's position normalised to the element it's bound to, so a drag
-- | maps straight to two parameters.
module Triggerfish.Ui.Pointer (padNorm) where

import Effect (Effect)
import Effect.Uncurried (EffectFn3, runEffectFn3)

foreign import padNormImpl :: EffectFn3 String Int Int { x :: Number, y :: Number }

-- | `{ x, y }` in [0,1], x→right, y→down, of the point (`clientX`, `clientY`)
-- | within the element of the given `id`. The coordinates are read
-- | synchronously at the view; only the element lookup is deferred to here.
padNorm :: String -> Int -> Int -> Effect { x :: Number, y :: Number }
padNorm = runEffectFn3 padNormImpl
