-- | Dropping a module from the browser drawer on a bank (docs/kb/plans/
-- | selene-in-tidal.md): the drop target's two halves.
module Triggerfish.Selene.Drop (allowDrop, dropText) where

import Prelude

import Effect (Effect)
import Web.Event.Event (Event)

-- | Let a drag over the bank drop here.
foreign import allowDrop :: Event -> Effect Unit

-- | What the drag carried: a module's line, or a block's name.
foreign import dropText :: Event -> Effect String
