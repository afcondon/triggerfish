-- | `Triggerfish.GlyphView` — the shared Halogen rendering of a glyph, so the
-- | shell's six-machine status board and any machine-local chip draw identically.
-- | Pure presentation: each FontAwesome solid icon carries its OWN colour (FA solid
-- | inherits CSS `color`), so a pair reads as "red cow, blue star"; the pair fades
-- | when the live state has diverged from the parked identity. See
-- | `docs/DESIGN-scene-modal.md` and `Triggerfish.Glyph`.
module Triggerfish.GlyphView
  ( faIcon
  , chipIcons
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Triggerfish.Glyph (ChipView, GlyphIcon)

style :: forall r i. String -> HP.IProp r i
style = HP.attr (H.AttrName "style")

-- | One FontAwesome solid glyph-icon, tinted its own colour.
faIcon :: forall w i. GlyphIcon -> HH.HTML w i
faIcon g =
  HH.i
    [ HP.attr (H.AttrName "class") ("fa-solid fa-" <> g.icon)
    , style ("font-size:17px;line-height:1;color:" <> g.color) ]
    []

-- | A machine's chip icons for the tab-bar status board: the coloured icon-pair,
-- | full-strength when held and faded when diverged; nothing at all when the
-- | machine reports no identity (`Nothing`). The alias + divergence ride in the
-- | tooltip.
chipIcons :: forall w i. Maybe ChipView -> HH.HTML w i
chipIcons Nothing = HH.text ""
chipIcons (Just cv) =
  HH.span
    [ HP.attr (H.AttrName "title") (cv.glyph.alias <> (if cv.diverged then " · modified" else ""))
    , style
        ( "display:inline-flex;align-items:center;gap:3px;margin-left:7px;padding-right:11px;vertical-align:middle;opacity:"
            <> (if cv.diverged then "0.4" else "1") )
    ]
    [ faIcon cv.glyph.first, faIcon cv.glyph.second ]
