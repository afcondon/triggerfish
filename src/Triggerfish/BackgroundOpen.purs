-- | A plain click on anything with `data-bg-open` opens that page in a
-- | background tab (see the JS for why it is a document listener).
module Triggerfish.BackgroundOpen (install) where

import Prelude

import Effect (Effect)

foreign import install :: Effect Unit
