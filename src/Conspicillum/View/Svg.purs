-- | SVG in Halogen, with attributes as the strings SVG takes.
module Conspicillum.View.Svg
  ( element
  , attribute
  , number
  , classes
  , styled
  , polar
  ) where

import Prelude

import Data.Number (cos, pi, sin)
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..), ElemName(..), Namespace(..))
import Halogen.HTML.Properties as HP
import Reef.Conspicillum.Decimal (fixed)

element :: forall r w i. String -> Array (HP.IProp r i) -> Array (HH.HTML w i) -> HH.HTML w i
element name = HH.elementNS (Namespace "http://www.w3.org/2000/svg") (ElemName name)

attribute :: forall r i. String -> String -> HP.IProp r i
attribute name = HP.attr (AttrName name)

-- | Two places are plenty for a drawing, and keep the DOM diff short.
number :: forall r i. String -> Number -> HP.IProp r i
number name = attribute name <<< fixed 2

classes :: forall r i. String -> HP.IProp r i
classes = attribute "class"

-- | Inline style, which beats a stylesheet rule where a presentation attribute
-- | would lose to it (the reversed grains once came out white that way).
styled :: forall r i. String -> HP.IProp r i
styled = attribute "style"

-- | A point on a circle about (cx, cy), with `turn` 0 at twelve o'clock and
-- | going clockwise, 1 a whole turn.
polar :: { x :: Number, y :: Number } -> Number -> Number -> { x :: Number, y :: Number }
polar centre radius turn =
  let angle = turn * 2.0 * pi - pi / 2.0
  in { x: centre.x + radius * cos angle, y: centre.y + radius * sin angle }
