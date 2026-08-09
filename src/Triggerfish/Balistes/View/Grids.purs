-- | Triggerfish.Balistes.View.Grids — the GRIDS (MI-Grids morph engine) tab:
-- | the CONTROL column (the X/Y STYLE pad + density/push/groove knobs) and the
-- | live 3×32 interpolation heatmap, plus the snapshot-bank + sequence pane below
-- | it. Pure renderers over `State`.
module Triggerfish.Balistes.View.Grids
  ( padSvg
  , heatSvg
  , knobStack
  ) where

import Prelude

import Data.Array (concatMap, null, range)
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Int.Bits (shr)
import Halogen as H
import Halogen.HTML as HH
import Web.UIEvent.MouseEvent as ME
import Triggerfish.Odonus.Grid.Widgets (style)
import Halogen.Widgets.Svg (svgAttr, svgEl, svgOn)
import Reef.Balistes.Tables as T
import Triggerfish.Balistes.Model as M
import Triggerfish.Transport (Sounding(..))
import Triggerfish.Balistes.Types (Action(..), DragKind(..), KnobTarget(..), NoteRef(..), State, padId)
import Triggerfish.Balistes.Widgets
  ( knobRow, bigKnob, flatBtn, noteTag, instColor, ohColor, concatMap' )

-- The knob block for the GRIDS band: density / push / groove, stacked. Was the
-- body of the old CONTROL panel, minus the pad (now a sibling in the band) and
-- minus the help text (the band has no room for prose, and the ⓘ has it).
knobStack :: forall m. State -> H.ComponentHTML Action () m
knobStack s =
  HH.div [ style "display:flex;flex-direction:column;gap:2px;flex:0 0 250px" ]
    [ knobRow "DENSITY"
        [ bigKnob (KDens 0) (instColor 0) "BD" s.bal
        , bigKnob (KDens 1) (instColor 1) "SD" s.bal
        , bigKnob (KDens 2) (instColor 2) "HH" s.bal
        ]
    , knobRow "PUSH ms"
        [ bigKnob (KPush 0) (instColor 0) "BD" s.bal
        , bigKnob (KPush 1) (instColor 1) "SD" s.bal
        , bigKnob (KPush 2) (instColor 2) "HH" s.bal
        ]
    , knobRow "GROOVE"
        [ bigKnob KRand "#6a6657" "RAND" s.bal
        , bigKnob KOpen ohColor "OPEN" s.bal
        , HH.div [ style "display:flex;flex-direction:column;gap:5px;width:58px;align-self:center" ]
            [ flatBtn "DILLA" DillaPreset, flatBtn "FLAT" FlatGroove ]
        ]
    ]

padSvg :: forall m. State -> H.ComponentHTML Action () m
padSvg s =
  let
    b = s.bal
    -- node value 0..255 for grid index 0..4
    nodeVal k = toNumber k * 255.0 / 4.0
    sx v = v
    sy v = 255.0 - v
    i0 = b.x `shr` 6
    j0 = b.y `shr` 6
    bracket i j = (i == i0 || i == i0 + 1) && (j == j0 || j == j0 + 1)
    -- a node dot, radius by its overall energy
    dot i j =
      let
        nv = T.node (T.drumMapIx i j)
        energy = if null nv then 0.0 else toNumber (sum nv) / (96.0 * 255.0)
        r = 3.0 + energy * 7.0
        hot = bracket i j
        dcx = sx (nodeVal i)
        dcy = sy (nodeVal j)
      in
        svgEl "circle"
          [ svgAttr "cx" (show dcx), svgAttr "cy" (show dcy), svgAttr "r" (show r)
          , svgAttr "fill" (if hot then "#7a7460" else "#9a9583")
          , svgAttr "fill-opacity" (if hot then "0.85" else "0.4")
          ] []
    cx = sx (toNumber b.x)
    cy = sy (toNumber b.y)
  in
    svgEl "svg"
      [ svgAttr "viewBox" "-8 -8 272 272", svgAttr "width" "100%", svgAttr "height" "100%"
      , svgAttr "id" padId
      , svgOn "mousedown" \e -> PadAt (ME.clientX e) (ME.clientY e) (ME.buttons e)
      , svgOn "mousemove" \e -> PadAt (ME.clientX e) (ME.clientY e) (ME.buttons e)
      , svgOn "mouseup" \_ -> PadRelease
      , svgAttr "style" "display:block;cursor:crosshair;touch-action:none"
      ]
      ( [ svgEl "rect"
            [ svgAttr "x" "-8", svgAttr "y" "-8", svgAttr "width" "272", svgAttr "height" "272"
            , svgAttr "rx" "8", svgAttr "fill" "#c4bfae", svgAttr "stroke" "#a8a392" ] []
        ]
        <> (range 0 4 `concatMap'` \i -> range 0 4 `concatMap'` \j -> [ dot i j ])
        <> crosshair cx cy
      )

-- crosshair lines + a filled cursor dot
crosshair :: forall w i. Number -> Number -> Array (HH.HTML w i)
crosshair cx cy =
  [ svgEl "line"
      [ svgAttr "x1" "0", svgAttr "y1" (show cy), svgAttr "x2" "255", svgAttr "y2" (show cy)
      , svgAttr "stroke" "#3f3c33", svgAttr "stroke-opacity" "0.25", svgAttr "stroke-width" "0.8" ] []
  , svgEl "line"
      [ svgAttr "x1" (show cx), svgAttr "y1" "0", svgAttr "x2" (show cx), svgAttr "y2" "255"
      , svgAttr "stroke" "#3f3c33", svgAttr "stroke-opacity" "0.25", svgAttr "stroke-width" "0.8" ] []
  , svgEl "circle"
      [ svgAttr "cx" (show cx), svgAttr "cy" (show cy), svgAttr "r" "7"
      , svgAttr "fill" "#1c1a12", svgAttr "stroke" "#efece1", svgAttr "stroke-width" "2" ] []
  ]

heatSvg :: forall m. State -> H.ComponentHTML Action () m
heatSvg s =
  let
    b = s.bal
    cols = 32
    colW = 16.0
    rowH = 30.0
    nLanes = 3
    gutter = 34.0                       -- left margin for lane name + MIDI note
    laneY lane = toNumber lane * rowH
    colX step = gutter + toNumber step * colW
    w = gutter + toNumber cols * colW
    h = toNumber nLanes * rowH
    rectBlock x0 y0 wid hgt c op =
      svgEl "rect"
        [ svgAttr "x" (show x0), svgAttr "y" (show y0)
        , svgAttr "width" (show wid), svgAttr "height" (show hgt), svgAttr "rx" "2"
        , svgAttr "fill" c, svgAttr "fill-opacity" (show op) ] []
    accentOutline x0 y0 =
      svgEl "rect"
        [ svgAttr "x" (show (x0 + 1.0)), svgAttr "y" (show (y0 + 1.0))
        , svgAttr "width" (show (colW - 3.0)), svgAttr "height" (show (rowH - 3.0)), svgAttr "rx" "3"
        , svgAttr "fill" "none", svgAttr "stroke" "#1c1a12", svgAttr "stroke-width" "1.2"
        , svgAttr "style" "pointer-events:none" ] []
    -- ratchets as a vertical STACK of n blocks down the tall pill (readable,
    -- since the cell is taller than wide), all at the same (flat) opacity.
    stackBlocks x y c n op =
      if n <= 1 then [ rectBlock (x + 2.0) (y + 2.0) (colW - 5.0) (rowH - 5.0) c op ]
      else
        range 0 (n - 1) `concatMap'` \k ->
          let segH = (rowH - 4.0) / toNumber n
              sy = y + 2.0 + toNumber k * segH
          in [ rectBlock (x + 2.0) sy (colW - 5.0) (segH - 1.0) c op ]
    -- the small ⋮N marker shown on a ratcheted slot that isn't firing.
    ratchetHint x y c n =
      svgEl "text"
        [ svgAttr "x" (show (x + colW / 2.0)), svgAttr "y" (show (y + rowH / 2.0 + 3.0))
        , svgAttr "fill" c, svgAttr "fill-opacity" "0.6", svgAttr "font-size" "8"
        , svgAttr "text-anchor" "middle", svgAttr "font-family" "Georgia,serif"
        , svgAttr "style" "pointer-events:none" ]
        [ HH.text ("⋮" <> show n) ]
    -- a Grids lane cell: faint interpolated landscape + ratcheted hit + accent.
    gridsCell lane step =
      let
        level = M.levelAt b lane step
        fires = M.wouldFire b lane step
        accent = fires && level > 192
        opens = lane == 2 && fires && M.opensAt b step   -- an open hat here
        n = M.ratchetAt b lane step
        x = colX step
        y = laneY lane
        c = if opens then ohColor else instColor lane
        landscape = rectBlock x y (colW - 1.0) (rowH - 1.0) c (toNumber level / 255.0 * 0.32)
        segs = if not fires then [] else stackBlocks x y c n (if accent then 0.95 else 0.7)
        acc = if accent then [ accentOutline x y ] else []
        hint = if n > 1 && not fires then [ ratchetHint x y c n ] else []
      in
        [ landscape ] <> segs <> acc <> hint
    -- Grids cells have no plain action (hits come from X/Y); a vertical drag
    -- sets the beat's ratchet (up = more retriggers).
    gridsTarget lane step =
      let
        x = colX step
        y = laneY lane
      in
        svgEl "rect"
          [ svgAttr "x" (show x), svgAttr "y" (show y)
          , svgAttr "width" (show (colW - 1.0)), svgAttr "height" (show (rowH - 1.0))
          , svgAttr "fill" "rgba(0,0,0,0)", svgAttr "style" "cursor:ns-resize;pointer-events:all"
          , svgOn "mousedown" \_ -> StartDrag (DCell lane step) (M.ratchetAt b lane step) ] []
    playhead =
      svgEl "rect"
        [ svgAttr "x" (show (colX s.playStep)), svgAttr "y" "0"
        , svgAttr "width" (show colW), svgAttr "height" (show h)
        , svgAttr "fill" "#1c1a12", svgAttr "fill-opacity" (if s.sounding /= Silent then "0.10" else "0.0")
        , svgAttr "stroke" "#1c1a12", svgAttr "stroke-opacity" (if s.sounding /= Silent then "0.5" else "0.15")
        , svgAttr "stroke-width" "1", svgAttr "style" "pointer-events:none" ] []
    beatLines =
      range 0 8 `concatMap'` \k ->
        let x = colX (k * 4)
        in [ svgEl "line"
               [ svgAttr "x1" (show x), svgAttr "y1" "0", svgAttr "x2" (show x), svgAttr "y2" (show h)
               , svgAttr "stroke" "#3f3c33", svgAttr "stroke-opacity" "0.18", svgAttr "stroke-width" "0.8"
               , svgAttr "style" "pointer-events:none" ] [] ]
    laneDivider lane =
      svgEl "line"
        [ svgAttr "x1" (show gutter), svgAttr "y1" (show (laneY lane)), svgAttr "x2" (show w)
        , svgAttr "y2" (show (laneY lane))
        , svgAttr "stroke" "#3f3c33", svgAttr "stroke-opacity" "0.12", svgAttr "stroke-width" "0.6"
        , svgAttr "style" "pointer-events:none" ] []
    -- the lane name (bold, coloured) + its editable MIDI note below, pulled into
    -- the gutter like the fixed grid.
    rowLabel lane =
      [ svgEl "text"
          [ svgAttr "x" "3", svgAttr "y" (show (laneY lane + 13.0))
          , svgAttr "fill" (instColor lane), svgAttr "fill-opacity" "0.9", svgAttr "style" "pointer-events:none"
          , svgAttr "font-size" "9", svgAttr "font-weight" "bold", svgAttr "font-family" "Georgia,serif" ]
          [ HH.text (M.instName lane) ]
      , noteTag 3.0 (laneY lane + 25.0) (NGrids lane) (M.noteOf lane b)
      ]
    visuals =
      range 0 2 `concatMap'` \lane -> range 0 (cols - 1) `concatMap'` \step -> gridsCell lane step
    targets =
      range 0 2 `concatMap'` \lane -> range 0 (cols - 1) `concatMap'` \step -> [ gridsTarget lane step ]
  in
    svgEl "svg"
      [ svgAttr "viewBox" ("0 0 " <> show w <> " " <> show h)
      -- Same cap as the RYTM grid (see Fixed.fixedSvg): bound the height, let
      -- `meet` letterbox, so the heatmap can take the band's width without the
      -- rows growing to match.
      , svgAttr "width" "100%"
      , svgAttr "preserveAspectRatio" "xMidYMid meet"
      , svgAttr "style" "display:block;max-height:170px" ]
      ( visuals <> beatLines
          <> map laneDivider (range 1 (nLanes - 1))
          <> [ playhead ] <> concatMap rowLabel (range 0 (nLanes - 1)) <> targets )
