-- | Convert a pointer event's client Y into the SVG's own user-space Y.
-- |
-- | The handler must be attached to the `<svg>` element itself, so that the
-- | event's `currentTarget` is the SVG and we can read its live `viewBox` and
-- | on-screen rect. This makes the mapping correct under any CSS scaling of the
-- | surface (the surface is `max-width: 880px; width: 100%`), not just at 1:1.
module Vetula.SvgCoord (svgYFromEvent, svgXFromEvent, isFormField, surfaceHidden) where

import Effect (Effect)
import Web.Event.Event (Event)

-- | True if the event's target is a text field (input / textarea / contenteditable)
-- | — used to let the global keyboard shortcuts stand down while the user types.
foreign import isFormField :: Event -> Effect Boolean

-- | True when the Vetula surface is not laid out (its tab is `display:none`
-- | inside the Triggerfish rack). Always false standalone, where the surface
-- | fills the page — so the keyboard shortcuts only fire on the visible Vetula.
foreign import surfaceHidden :: Effect Boolean

foreign import svgYFromEvent :: Event -> Effect Number

-- | The horizontal mirror of `svgYFromEvent` — the pointer's X in the SVG's own
-- | user space. Used by the progression rows, whose pitch axis runs left→right.
foreign import svgXFromEvent :: Event -> Effect Number
