-- | A ghost fish is a link that a plain click leaves alone and a cmd-click
-- | opens behind (see the JS for why).
module Triggerfish.BackgroundOpen (install) where

import Prelude

import Effect (Effect)

foreign import install :: Effect Unit
