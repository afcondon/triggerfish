-- | Dropping a module from the browser drawer on a bank (docs/kb/plans/
-- | selene-in-tidal.md): the drop target's two halves.
module Triggerfish.Selene.Drop (allowDrop, dropText, currentDrag, altHeld, startDrag) where

import Prelude

import Effect (Effect)
import Web.Event.Event (Event)

-- | Let a drag over the bank drop here.
foreign import allowDrop :: Event -> Effect Unit

-- | What the drag carried: a module's line, or a block's name.
foreign import dropText :: Event -> Effect String

-- | What is being dragged from the drawer now, "" if nothing.
foreign import currentDrag :: Effect String

-- | Option (Alt) held at the drop: the expert's merge.
foreign import altHeld :: Event -> Boolean

-- | Start a drag from the page carrying `text` (the rack's rebus: "keep:rack").
foreign import startDrag :: Event -> String -> Effect Unit
