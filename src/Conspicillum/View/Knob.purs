-- | A panel knob: 270 degrees of travel, the value as an arc on the track.
module Conspicillum.View.Knob (knob) where

import Prelude

import Conspicillum.View.Svg (attribute, classes, element, number, polar)
import Halogen.HTML as HH
import Hylograph.Shape.Arc (arcPathWithCenter, degreesToRadians)

knob :: forall w i. Number -> HH.HTML w i
knob position =
  let
    u = max 0.0 (min 1.0 position)
    degrees = -135.0 + 270.0 * u
    centre = { x: 32.0, y: 32.0 }
    band from to = arcPathWithCenter centre.x centre.y { innerRadius: 26.5, outerRadius: 29.5 }
      { startAngle: degreesToRadians from, endAngle: degreesToRadians to }
    inner = polar centre 7.0 (degrees / 360.0)
    outer = polar centre 18.0 (degrees / 360.0)
  in
    element "svg" [ attribute "viewBox" "0 0 64 64", attribute "width" "60", attribute "height" "60" ]
      ( [ element "path" [ classes "kt", attribute "d" (band (-135.0) 135.0) ] [] ]
          <> (if u > 0.002 then [ element "path" [ classes "kv", attribute "d" (band (-135.0) degrees) ] [] ] else [])
          <>
            [ element "circle" [ classes "kb", number "cx" 32.0, number "cy" 32.0, number "r" 21.0 ] []
            , element "line" [ classes "ki", number "x1" inner.x, number "y1" inner.y, number "x2" outer.x, number "y2" outer.y ] []
            ]
      )
