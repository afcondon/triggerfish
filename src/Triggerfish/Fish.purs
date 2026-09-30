-- | **The machines' fish**, the small mnemonic from the identity study: each
-- | machine's species, drawn once as an SVG symbol and used wherever the machine
-- | needs naming in little space (a routing row, a card's roundel).
-- |
-- | `install` adds the sprite to the page; call it once at start. `icon` draws
-- | one fish; before `install`, or for an unknown slot, it draws nothing.
module Triggerfish.Fish
  ( install
  , icon
  , ofSource
  ) where

import Prelude

import Effect (Effect)
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..), ElemName(..), Namespace(..))
import Halogen.HTML.Properties as HP
import Triggerfish.Routing.Model as RM

foreign import install :: Effect Unit

-- | A machine's fish, by its stage slot (`odonus`, `vetula`, `balistes`, …). It
-- | is drawn 22 by 13 pixels, the routing-row size, unless CSS for class `cls`
-- | sizes it otherwise (the width and height attributes yield to any CSS).
icon :: forall w i. String -> String -> HH.HTML w i
icon cls slot =
  HH.elementNS svgNS (ElemName "svg")
    [ HP.attr (AttrName "viewBox") "0 0 200 120"
    , HP.attr (AttrName "width") "22"
    , HP.attr (AttrName "height") "13"
    , HP.attr (AttrName "class") cls
    , HP.attr (AttrName "aria-hidden") "true"
    ]
    [ HH.elementNS svgNS (ElemName "use") [ HP.attr (AttrName "href") ("#sp-" <> slot) ] [] ]
  where
  svgNS = Namespace "http://www.w3.org/2000/svg"

-- | The machine a routing source belongs to.
ofSource :: RM.Source -> String
ofSource = case _ of
  RM.SOdonusHead _ -> "odonus"
  RM.SDrumLane _ -> "balistes"
  RM.SVetulaVoice _ -> "vetula"
  RM.SSeleneBank _ -> "selene"
