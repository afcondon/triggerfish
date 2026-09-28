-- | Triggerfish.Balistes.Widgets — the Balistes-shared HTML toolkit: the pale
-- | Hainbach panel, the button/readout/stepper primitives, the big knob, the SVG
-- | helpers (`svgRect`/`noteTag`) and the voice colours. Every
-- | `Balistes.View.*` module and `Balistes.Snapshot` render from these, so the
-- | look stays consistent and the view modules stay small.
-- |
-- | The low-level `style`/`svgEl`/`svgAttr`/`engrave` primitives still come from
-- | `Odonus.Grid.Widgets` (the cross-instrument shared kit) — promoting those to a
-- | neutral `Triggerfish.Ui` is a separate, app-wide job. Mirrors the role
-- | `Odonus.Grid.Widgets` plays for Odonus's own view modules.
module Triggerfish.Balistes.Widgets
  ( panel
  , flatBtn
  , readout
  , stepBtn
  , armBtn
  , chip
  , newChip
  , knobRow
  , bigKnob
  , svgRect
  , noteTag
  , concatMap'
  , instColor
  , laneColor
  , ohColor
  ) where

import Prelude

import Data.Array (concatMap)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Triggerfish.Ui.Style (engrave, style)
import Halogen.Widgets.Svg (svgAttr, svgEl, svgOn)
import Triggerfish.Ui.Knob (knob)
import Triggerfish.Balistes.Model as M
import Triggerfish.Balistes.Types (Action(..), DragKind(..), KnobTarget, NoteRef, knobValue, targetRange)

-- A pale Hainbach panel (header + body). Scrolls vertically if its content is
-- taller than the viewport (the consolidated CONTROL panel can be).
panel :: forall m. String -> String -> Array (H.ComponentHTML Action () m) -> H.ComponentHTML Action () m
panel label widthCss body =
  HH.div
    [ style $ widthCss <> ";height:100%;box-sizing:border-box;overflow-y:auto;overflow-x:hidden;"
        <> "background:linear-gradient(#dcd8c9,#cfcabb);border-left:1px solid #b3ae9c;"
        <> "padding:18px 16px;display:flex;flex-direction:column" ]
    ( [ HH.div
          [ style $ engrave <> ";font-size:14px;letter-spacing:0.16em;color:#3f3c33;"
              <> "margin-bottom:14px;border-bottom:1px solid #00000018;padding-bottom:6px" ]
          [ HH.text label ]
      ] <> body )

flatBtn :: forall m. String -> Action -> H.ComponentHTML Action () m
flatBtn label act =
  HH.button
    [ HE.onClick \_ -> act
    , style $ "flex:1;padding:8px 0;border:1px solid #a8a392;border-radius:6px;cursor:pointer;"
        <> "font-family:Georgia,serif;font-size:11px;letter-spacing:0.08em;color:#3f3c33;"
        <> "background:linear-gradient(#efece1,#ddd9cb)" ]
    [ HH.text label ]

readout :: forall m. String -> String -> H.ComponentHTML Action () m
readout label val =
  HH.div [ style "display:flex;justify-content:space-between;align-items:baseline;border-bottom:1px dotted #0000001a;padding-bottom:3px" ]
    [ HH.span [ style $ engrave <> ";font-size:9px" ] [ HH.text label ]
    , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:10px;color:#3f3c33;text-align:right" ] [ HH.text val ]
    ]

-- A small square stepper button (− / +).
stepBtn :: forall m. String -> Action -> H.ComponentHTML Action () m
stepBtn label act =
  HH.button
    [ HE.onClick \_ -> act
    , style $ "width:18px;height:18px;border:1px solid #a8a392;border-radius:4px;cursor:pointer;"
        <> "font-family:'SF Mono',Menlo,monospace;font-size:11px;color:#3f3c33;background:#efece1;"
        <> "display:flex;align-items:center;justify-content:center;padding:0" ]
    [ HH.text label ]

-- A small arm/toggle button (brass when active).
armBtn :: forall m. String -> Boolean -> Action -> H.ComponentHTML Action () m
armBtn label active act =
  HH.button
    [ HE.onClick \_ -> act
    , style $ "padding:4px 11px;border:1px solid #a8a392;border-radius:5px;cursor:pointer;"
        <> "font-family:'SF Mono',Menlo,monospace;font-size:9px;letter-spacing:0.06em;"
        <> (if active then "color:#1c1a12;background:linear-gradient(#c8a86a,#b8975a)"
            else "color:#3f3c33;background:linear-gradient(#efece1,#ddd9cb)") ]
    [ HH.text label ]

chip :: forall m. String -> Boolean -> Action -> H.ComponentHTML Action () m
chip label active act =
  HH.button
    [ HE.onClick \_ -> act
    , style $ "padding:6px 13px;border:1px solid #a8a392;border-radius:6px;cursor:pointer;"
        <> "font-family:Georgia,serif;font-size:12px;letter-spacing:0.04em;"
        <> (if active then "color:#1c1a12;background:linear-gradient(#c8a86a,#b8975a)"
            else "color:#3f3c33;background:linear-gradient(#efece1,#ddd9cb)") ]
    [ HH.text label ]

-- The "+ NEW" tab: appends a fresh empty rhythm (dashed to read as an action).
newChip :: forall m. H.ComponentHTML Action () m
newChip =
  HH.button
    [ HE.onClick \_ -> NewPattern
    , style $ "padding:6px 13px;border:1px dashed #a8a392;border-radius:6px;cursor:pointer;"
        <> "font-family:Georgia,serif;font-size:12px;color:#6a6657;background:#00000006" ]
    [ HH.text "+ NEW" ]

-- One labelled row of knobs (the left label, then the knobs across).
knobRow :: forall m. String -> Array (H.ComponentHTML Action () m) -> H.ComponentHTML Action () m
knobRow label knobs =
  HH.div [ style "display:flex;align-items:flex-start;gap:8px;margin-bottom:4px" ]
    [ HH.div [ style $ engrave <> ";font-size:8px;opacity:0.7;width:42px;flex:0 0 auto;padding-top:6px;text-align:right" ]
        [ HH.text label ]
    , HH.div [ style "display:flex;gap:2px" ] knobs ]

bigKnob :: forall m. KnobTarget -> String -> String -> M.Balistes -> H.ComponentHTML Action () m
bigKnob target color label b =
  let
    v = knobValue target b
    r = targetRange target
  in
    HH.div [ style "display:flex;flex-direction:column;align-items:center;width:64px" ]
      [ HH.span [ style $ engrave <> ";font-size:9px;margin-bottom:2px" ] [ HH.text label ]
      , HH.div [ style "width:50px;height:50px" ]
          [ knob { cx: 24.0, cy: 24.0, rOuter: 20.0, rInner: 8.0, color, lo: r.lo, hi: r.hi, value: v, ticks: 0 } (StartDrag (DKnob target) v) ]
      , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:10px;color:#3f3c33;margin-top:2px" ]
          [ HH.text (show v) ]
      ]

-- A filled, rounded SVG rect — the cell primitive shared by the fixed grid.
svgRect :: forall w i. Number -> Number -> Number -> Number -> String -> Number -> HH.HTML w i
svgRect x0 y0 wid hgt c op =
  svgEl "rect"
    [ svgAttr "x" (show x0), svgAttr "y" (show y0)
    , svgAttr "width" (show wid), svgAttr "height" (show hgt), svgAttr "rx" "2"
    , svgAttr "fill" c, svgAttr "fill-opacity" (show op) ] []

-- An editable MIDI-note tag in a lane gutter: drag up/down to nudge the note.
-- Shared by the Grids heatmap and the fixed grid.
noteTag :: forall m. Number -> Number -> NoteRef -> Int -> H.ComponentHTML Action () m
noteTag x y ref n =
  svgEl "text"
    [ svgAttr "x" (show x), svgAttr "y" (show y)
    , svgAttr "fill" "#3f3c33", svgAttr "fill-opacity" "0.7"
    , svgAttr "font-size" "8.5", svgAttr "font-family" "'SF Mono',Menlo,monospace"
    , svgAttr "style" "cursor:ns-resize"
    , svgOn "mousedown" \_ -> StartDrag (DNote ref) n ]
    [ HH.text ("♪" <> show n) ]

-- Flipped concatMap so the call sites read `range … `concatMap'` \i -> …`.
concatMap' :: forall a b. Array a -> (a -> Array b) -> Array b
concatMap' xs f = concatMap f xs

-- ---------------------------------------------------------------------------
-- Voice colours
-- ---------------------------------------------------------------------------

instColor :: Int -> String
instColor = case _ of
  0 -> "#b04a2f"   -- BD, amber-red
  1 -> "#5f7d3f"   -- SD, green
  _ -> "#3f6f8a"   -- HH, steel-blue

-- | Per-lane colour for a fixed rhythm's 16-lane kit, grouped by voice family
-- | (kick/snare warm, hats cool, toms brown, cymbals gold, perc violet).
laneColor :: Int -> String
laneColor = case _ of
  0 -> "#b04a2f"   -- BD
  1 -> "#5f7d3f"   -- SD
  2 -> "#a86a2f"   -- CP
  3 -> "#8a6a4a"   -- RS
  4 -> "#3f6f8a"   -- CH
  5 -> "#4f7f9a"   -- PH
  6 -> "#2f8a8a"   -- OH
  7 -> "#7a5a3a"   -- LT
  8 -> "#8a6a44"   -- MT
  9 -> "#9a7a4a"   -- HT
  10 -> "#9a7d3a"  -- RD
  11 -> "#aa8d4a"  -- RB
  12 -> "#b58a3a"  -- CR
  13 -> "#6a5f8a"  -- CW
  14 -> "#7a6f9a"  -- TB
  _ -> "#8a7faa"   -- SH

-- | The open hat's teal — distinct from HH steel-blue, so opening cells read as
-- | a different voice in the heatmap.
ohColor :: String
ohColor = "#2f8a8a"
