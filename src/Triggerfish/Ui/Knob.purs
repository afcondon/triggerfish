-- | A test-gear knob, geometry ported from producing-with-your-feet's `Donut`
-- | (300° sweep, 7 o'clock → 5 o'clock). Value-driven arc + pointer; the
-- | mousedown emits a caller-supplied action and the parent tracks the drag at
-- | the document level (see Grid). The shared instrument primitive.
module Triggerfish.Ui.Knob
  ( KnobView
  , knob
  ) where

import Prelude

import Data.Array (range)
import Data.Int (toNumber)
import Data.Number (cos, sin, pi)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (EventType(..))
import Web.UIEvent.MouseEvent (MouseEvent)

svgNS :: String
svgNS = "http://www.w3.org/2000/svg"

svgEl :: forall w i. String -> Array (HH.IProp () i) -> Array (HH.HTML w i) -> HH.HTML w i
svgEl name = HH.elementNS (HH.Namespace svgNS) (HH.ElemName name)

svgAttr :: forall r i. String -> String -> HH.IProp r i
svgAttr n v = HP.attr (HH.AttrName n) v

svgOnDown :: forall r i. (MouseEvent -> i) -> HH.IProp r i
svgOnDown f = HE.handler (EventType "mousedown") (unsafeCoerce f)

minAngle :: Number
minAngle = -5.0 * pi / 6.0

maxAngle :: Number
maxAngle = 5.0 * pi / 6.0

sweep :: Number
sweep = maxAngle - minAngle

type KnobView =
  { cx :: Number
  , cy :: Number
  , rOuter :: Number
  , rInner :: Number
  , color :: String
  , lo :: Int
  , hi :: Int
  , value :: Int
  , ticks :: Int    -- 0 = smooth; N > 1 draws N detent marks around the arc
  }

valToAngle :: KnobView -> Number
valToAngle k =
  let
    frac = if k.hi == k.lo then 0.0 else toNumber (k.value - k.lo) / toNumber (k.hi - k.lo)
  in
    minAngle + frac * sweep

-- | Filled donut wedge from a0 to a1 (radians; 0 = up).
arcPath :: Number -> Number -> Number -> Number -> Number -> Number -> String
arcPath cx cy outerR innerR a0 a1 =
  let
    toSvg a = a - pi / 2.0
    sa = toSvg a0
    ea = toSvg a1
    ox0 = cx + outerR * cos sa
    oy0 = cy + outerR * sin sa
    ox1 = cx + outerR * cos ea
    oy1 = cy + outerR * sin ea
    ix0 = cx + innerR * cos sa
    iy0 = cy + innerR * sin sa
    ix1 = cx + innerR * cos ea
    iy1 = cy + innerR * sin ea
    largeArc = if (a1 - a0) > pi then "1" else "0"
    s = show
  in
    "M" <> s ox0 <> "," <> s oy0
      <> " A" <> s outerR <> "," <> s outerR <> " 0 " <> largeArc <> ",1 " <> s ox1 <> "," <> s oy1
      <> " L" <> s ix1 <> "," <> s iy1
      <> " A" <> s innerR <> "," <> s innerR <> " 0 " <> largeArc <> ",0 " <> s ix0 <> "," <> s iy0
      <> " Z"

-- | A complete `<svg>` knob that fills its container. `onDown` is raised on
-- | mousedown over the knob (the parent then tracks the drag).
knob :: forall w i. KnobView -> i -> HH.HTML w i
knob k onDown =
  let
    a = valToAngle k
    px = k.cx + (k.rOuter + 1.0) * cos (a - pi / 2.0)
    py = k.cy + (k.rOuter + 1.0) * sin (a - pi / 2.0)
    s = show
  in
    svgEl "svg"
      [ svgAttr "viewBox" "0 0 48 48"
      , svgAttr "width" "100%"
      , svgAttr "height" "100%"
      , svgAttr "style" "display:block"
      ]
      ( [ svgEl "path"
            [ svgAttr "d" (arcPath k.cx k.cy k.rOuter k.rInner minAngle maxAngle)
            , svgAttr "fill" "#bdb8a6"
            ] []
        , svgEl "path"
            [ svgAttr "d" (arcPath k.cx k.cy k.rOuter k.rInner minAngle a)
            , svgAttr "fill" k.color
            ] []
        ] <> detents k <>
        [ svgEl "circle"
            [ svgAttr "cx" (s k.cx), svgAttr "cy" (s k.cy)
            , svgAttr "r" (s (k.rInner - 1.0)), svgAttr "fill" "#34322c"
            ] []
        , svgEl "line"
            [ svgAttr "x1" (s k.cx), svgAttr "y1" (s k.cy)
            , svgAttr "x2" (s px), svgAttr "y2" (s py)
            , svgAttr "stroke" "#e7e2d2", svgAttr "stroke-width" "2"
            , svgAttr "stroke-linecap" "round"
            ] []
        , svgEl "circle"
            [ svgAttr "cx" (s k.cx), svgAttr "cy" (s k.cy)
            , svgAttr "r" (s k.rOuter), svgAttr "fill" "transparent"
            , svgAttr "style" "cursor:ns-resize"
            , svgOnDown (const onDown)
            ] []
        ] )

-- | Detent ticks: short radial marks just outside the arc at each of the
-- | `ticks` discrete positions, so a small-range knob reads as a rotary
-- | selector. Empty when `ticks <= 1`.
detents :: forall w i. KnobView -> Array (HH.HTML w i)
detents k
  | k.ticks <= 1 = []
  | otherwise =
      let s = show
          tick i =
            let frac = toNumber i / toNumber (k.ticks - 1)
                ang = (minAngle + frac * sweep) - pi / 2.0
                r0 = k.rOuter + 1.2
                r1 = k.rOuter + 3.4
            in svgEl "line"
                 [ svgAttr "x1" (s (k.cx + r0 * cos ang)), svgAttr "y1" (s (k.cy + r0 * sin ang))
                 , svgAttr "x2" (s (k.cx + r1 * cos ang)), svgAttr "y2" (s (k.cy + r1 * sin ang))
                 , svgAttr "stroke" "#6b6657", svgAttr "stroke-width" "1"
                 ] []
      in map tick (range 0 (k.ticks - 1))
