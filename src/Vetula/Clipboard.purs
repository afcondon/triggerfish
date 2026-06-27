-- | Copy a string to the system clipboard (best-effort; a no-op if the browser
-- | withholds clipboard access). Used by the Progression tab's "copy" button.
module Vetula.Clipboard (copyText) where

import Data.Unit (Unit)
import Effect (Effect)

foreign import copyText :: String -> Effect Unit
