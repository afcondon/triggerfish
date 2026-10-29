-- | **The ring: time.** Clockwise from the top, one bar. Each grain is a
-- | wedge from its onset for as long as it sounds (to the next onset at most,
-- | so a dense cloud tiles rather than piles up), in the colour of where it
-- | reads. Silence is a gap. Inside it, **the cloud**: each grain as it fires,
-- | by pan across and speed up.
module Conspicillum.View.Circle
  ( Dot
  , spawn
  , live
  , circle
  , centre
  ) where

import Prelude

import Data.Array (filter, length, mapWithIndex, (..))
import Data.Int (toNumber)
import Data.Number (log, sqrt, abs, cos, sin)
import Conspicillum.Plan (Grain, Plan)
import Conspicillum.View.Svg (attribute, classes, element, number, polar, styled)
import Halogen.HTML as HH
import Hylograph.Shape.Arc (arcPathWithCenter, tau)

-- The ring's geometry, in a 400 × 400 viewBox.
centre :: { x :: Number, y :: Number }
centre = { x: 200.0, y: 200.0 }

wedgeInner = 136.0 :: Number
wedgeOuter = 174.0 :: Number
tickRadius = 184.0 :: Number
cloudRadius = 124.0 :: Number

-- | A grain in the cloud: where it landed, as big as it is loud, filled with
-- | where it reads (a ring, if it plays backwards), fading over its own length.
type Dot =
  { target :: { x :: Number, y :: Number }
  , x :: Number
  , y :: Number
  , radius :: Number
  , colour :: String
  , backwards :: Boolean
  , born :: Number
  , life :: Number
  }

-- | A grain that has just begun. Grains landing on one spot fan out on a
-- | sunflower spiral rather than stacking: the i-th live dot near a spot goes
-- | out by √i, turned by the golden angle, so a pile-up reads as a cluster
-- | whose size is its count.
spawn :: Number -> Array Dot -> Grain -> Dot
spawn now dots grain =
  let
    e = grain.emit
    speed = max 0.25 (min 4.0 (if e.speed == 0.0 then 1.0 else abs e.speed))
    tx = centre.x + (max 0.0 (min 1.0 e.pan) - 0.5) * 2.0 * (cloudRadius - 26.0)
    ty = centre.y - log speed / log 2.0 * 40.0
    radius = 3.0 + 6.0 * min 1.2 e.gain
    near = toNumber (length (filter (\d -> hypot (d.target.x - tx) (d.target.y - ty) < 4.0) dots))
    out = if near == 0.0 then 0.0 else 2.0 * radius * 0.62 * sqrt near
    turn = near * 2.39996
  in
    { target: { x: tx, y: ty }
    , x: tx + out * cos turn
    , y: ty + out * sin turn
    , radius
    , colour: grain.colour
    , backwards: e.speed < 0.0
    , born: now
    , life: max 140.0 (e.sustain * 1000.0)
    }
  where
  hypot a b = sqrt (a * a + b * b)

-- | The dots still visible at `now`.
live :: Number -> Array Dot -> Array Dot
live now = filter \d -> now - d.born < d.life

circle
  :: forall w i
   . { plan :: Plan, phase :: Number, playing :: Boolean, dots :: Array Dot, now :: Number }
  -> HH.HTML w i
circle { plan, phase, playing, dots, now } =
  element "svg" [ attribute "id" "circle", attribute "viewBox" "0 0 400 400", classes (if playing then "" else "idle") ]
    ( [ element "defs" []
          [ element "pattern"
              [ attribute "id" "hatch", attribute "width" "5", attribute "height" "5"
              , attribute "patternUnits" "userSpaceOnUse", attribute "patternTransform" "rotate(45)"
              ]
              [ element "line" [ attribute "x1" "0", attribute "y1" "0", attribute "x2" "0", attribute "y2" "5", attribute "stroke" "#fff", attribute "stroke-width" "1.6" ] [] ]
          ]
      , element "circle"
          [ classes "slotband", number "cx" centre.x, number "cy" centre.y
          , number "r" ((wedgeInner + wedgeOuter) / 2.0), number "stroke-width" (wedgeOuter - wedgeInner)
          ] []
      ]
        <> ticks
        <> beatLabels
        <> axes
        <> mapWithIndex wedge plan.grains
        <> [ element "g" [] (dots <#> dot) ]
        <> [ hand ]
    )
  where
  ticks = (0 .. 15) <#> \i ->
    let
      beat = i `mod` 4 == 0
      a = polar centre (tickRadius - (if beat then 5.0 else 2.0)) (toNumber i / 16.0)
      b = polar centre (tickRadius + 4.0) (toNumber i / 16.0)
    in
      element "line" [ classes (if beat then "tick beat" else "tick"), number "x1" a.x, number "y1" a.y, number "x2" b.x, number "y2" b.y ] []

  beatLabels = (0 .. 3) <#> \b ->
    let p = polar centre (tickRadius + 11.0) (toNumber b / 4.0 + 0.02)
    in element "text" [ classes "blabel", number "x" p.x, number "y" p.y ] [ HH.text (show (b + 1)) ]

  -- The cloud's axes: pan across, speed up (a doubling per 40 units).
  axes =
    let
      reach = cloudRadius - 16.0
    in
      [ element "line" [ classes "cloudaxis", number "x1" (centre.x - reach), number "y1" centre.y, number "x2" (centre.x + reach), number "y2" centre.y ] []
      , element "line" [ classes "cloudaxis", number "x1" centre.x, number "y1" (centre.y - reach), number "x2" centre.x, number "y2" (centre.y + reach) ] []
      , label (centre.x - cloudRadius + 18.0) (centre.y - 4.0) "L"
      , label (centre.x + cloudRadius - 24.0) (centre.y - 4.0) "R"
      , label (centre.x + 4.0) (centre.y - cloudRadius + 26.0) "faster"
      , label (centre.x + 4.0) (centre.y + cloudRadius - 20.0) "slower"
      ]
  label x y t = element "text" [ classes "cloudlab", number "x" x, number "y" y ] [ HH.text t ]

  wedge _ g =
    let
      path = arcPathWithCenter centre.x centre.y { innerRadius: wedgeInner, outerRadius: wedgeOuter }
        { startAngle: g.from * tau, endAngle: (g.from + max 0.002 (min (g.to - g.from) 0.9995)) * tau }
      on = phase >= g.from && phase < g.to
    in
      element "g" [ classes (if on then "gr on" else "gr") ]
        ( [ element "path" [ classes "wd", attribute "d" path, attribute "fill" g.colour ] [] ]
            <> (if g.emit.speed < 0.0 then [ element "path" [ attribute "d" path, attribute "fill" "url(#hatch)" ] [] ] else [])
        )

  dot d =
    let
      age = (now - d.born) / d.life
    in
      element "circle"
        [ classes "pt", number "cx" d.x, number "cy" d.y, number "r" (d.radius * (1.0 + 0.5 * age))
        , number "opacity" (1.0 - age)
        , styled
            if d.backwards then "fill:" <> d.colour <> ";fill-opacity:.22;stroke:" <> d.colour <> ";stroke-width:2.6"
            else "fill:" <> d.colour <> ";stroke:#fff"
        ] []

  hand =
    let
      a = polar centre (wedgeInner - 4.0) phase
      b = polar centre (tickRadius + 2.0) phase
    in
      element "line" [ classes "hand", number "x1" a.x, number "y1" a.y, number "x2" b.x, number "y2" b.y ] []
