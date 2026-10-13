-- | Dragging a saved thing out of a machine's drawer, or off its page, and
-- | dropping it: one module for every machine (and Limulus reads the same
-- | types). A drag's text is `<kind> <payload>` for the kinds that have one
-- | (`vetula-progression`, `balistes-beat`, `odonus-patch`, `selene-module`),
-- | carried as `application/x-<kind>`; any other text as it is.
module Triggerfish.Drag
  ( startDrag
  , currentDrag
  , allowDrop
  , altHeld
  , dropText
  , dropOf
  , dragOf
  ) where

import Prelude

import Effect (Effect)
import Web.Event.Event (Event)

-- | Start a drag carrying `text`.
foreign import startDrag :: Event -> String -> Effect Unit

-- | What is being dragged from a drawer or page now, "" if nothing.
foreign import currentDrag :: Effect String

-- | Let a drag over this target drop here.
foreign import allowDrop :: Event -> Effect Unit

-- | Option (Alt) held at the drop: the expert's merge.
foreign import altHeld :: Event -> Boolean

-- | What a drop carried as plain text.
foreign import dropText :: Event -> Effect String

-- | What a drop carried of `kind` (its payload), "" if it carried none.
foreign import dropOf :: String -> Event -> Effect String

-- | The drag text for a thing of `kind`.
dragOf :: String -> String -> String
dragOf kind payload = kind <> " " <> payload
