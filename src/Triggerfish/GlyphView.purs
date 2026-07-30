-- | `Triggerfish.GlyphView` — the shared Halogen rendering of a glyph, so the
-- | shell's six-machine status board and any machine-local chip draw identically.
-- | Pure presentation: FontAwesome solid icons tinted by the machine hue (FA solid
-- | inherits CSS `color`), faded when the live state has diverged from the parked
-- | identity. See `docs/DESIGN-scene-modal.md` and `Triggerfish.Glyph`.
module Triggerfish.GlyphView
  ( faIcon
  , chipIcons
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Triggerfish.Glyph (ChipView)

style :: forall r i. String -> HP.IProp r i
style = HP.attr (H.AttrName "style")

-- | One FontAwesome solid glyph, tinted the given hue.
faIcon :: forall w i. String -> String -> HH.HTML w i
faIcon hue name =
  HH.i
    [ HP.attr (H.AttrName "class") ("fa-solid fa-" <> name)
    , style ("font-size:17px;line-height:1;color:" <> hue) ]
    []

-- | A machine's chip icons for the tab-bar status board: the icon-pair, tinted
-- | `hue`, full-strength when held and faded when diverged; nothing at all when
-- | the machine reports no identity (`Nothing`). The alias + divergence ride in
-- | the tooltip.
chipIcons :: forall w i. String -> Maybe ChipView -> HH.HTML w i
chipIcons _ Nothing = HH.text ""
chipIcons hue (Just cv) =
  HH.span
    [ HP.attr (H.AttrName "title") (cv.glyph.alias <> (if cv.diverged then " · modified" else ""))
    , style
        ( "display:inline-flex;align-items:center;gap:3px;margin-left:7px;padding-right:11px;vertical-align:middle;opacity:"
            <> (if cv.diverged then "0.4" else "1") )
    ]
    [ faIcon hue cv.glyph.first.icon, faIcon hue cv.glyph.second.icon ]
