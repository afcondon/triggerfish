-- | A test-gear knob: value-driven arc + pointer, `Int`-valued, in Triggerfish's
-- | pale Hainbach colours. The shared instrument primitive.
-- |
-- | **Why this is not `Halogen.Widgets.Knob`.** The library's knob is a
-- | component that owns its own drag; Triggerfish's parents own theirs, because
-- | a knob turn has to travel to the BEAM and come back in lockstep with
-- | everything else the machine is doing. So `knob` is a pure view: mousedown
-- | raises a caller-supplied action and the parent tracks the rest at the
-- | document level (see `Odonus.Grid`).
-- |
-- | The two do share `Halogen.Widgets.Knob.Geometry` — the 300° sweep, the
-- | value→angle map and the donut-wedge path. Those had been duplicated
-- | verbatim in both repos, which is the kind of pair that silently drifts:
-- | nothing would fail to compile if one of them changed, the same value would
-- | just start reading as two different positions.
module Triggerfish.Ui.Knob
  ( KnobView
  , knob
  ) where

import Prelude

import Data.Array (range)
import Data.Int (toNumber)
import Halogen.HTML as HH
import Halogen.Widgets.Knob.Geometry (arcPath, maxAngle, minAngle, pointerAt, tickAngle, valToAngle)
import Halogen.Widgets.Svg (svgAttr, svgEl, svgOnDown)

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

angleOf :: KnobView -> Number
angleOf k = valToAngle (toNumber k.lo) (toNumber k.hi) (toNumber k.value)

-- | A complete `<svg>` knob that fills its container. `onDown` is raised on
-- | mousedown over the knob (the parent then tracks the drag).
knob :: forall w i. KnobView -> i -> HH.HTML w i
knob k onDown =
  let
    a = angleOf k
    ptr = pointerAt k.cx k.cy (k.rOuter + 1.0) a
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
            , svgAttr "x2" (s ptr.x), svgAttr "y2" (s ptr.y)
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
            let ang = tickAngle k.ticks i
                p0 = pointerAt k.cx k.cy (k.rOuter + 1.2) ang
                p1 = pointerAt k.cx k.cy (k.rOuter + 3.4) ang
            in svgEl "line"
                 [ svgAttr "x1" (s p0.x), svgAttr "y1" (s p0.y)
                 , svgAttr "x2" (s p1.x), svgAttr "y2" (s p1.y)
                 , svgAttr "stroke" "#6b6657", svgAttr "stroke-width" "1"
                 ] []
      in map tick (range 0 (k.ticks - 1))
