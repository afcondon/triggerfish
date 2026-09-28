-- | Triggerfish.Balistes.View.Fixed — the RYTM (fixed-rhythm) tab: the library
-- | switcher chips, the literal lane-grid editor (`fixedSvg`), and the NOTE
-- | inspector column that edits the selected cell's velocity / probability /
-- | condition / ratchet. Pure renderers over `State`.
module Triggerfish.Balistes.View.Fixed
  ( fixedSvg
  , cellStrip
  ) where

import Prelude

import Data.Array (concatMap, length, range, (!!))
import Data.Foldable (any)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Web.UIEvent.MouseEvent as ME
import Triggerfish.Odonus.Grid.Widgets (engrave, style)
import Halogen.Widgets.Svg (svgAttr, svgEl, svgOn)
import Triggerfish.Balistes.Pattern as P
import Triggerfish.Transport (Sounding(..))
import Triggerfish.Balistes.Types (Action(..), NoteRef(..), State, activePattern)
import Triggerfish.Balistes.Widgets
  ( stepBtn, svgRect, noteTag, laneColor, concatMap' )

-- The per-cell editor, as a HORIZONTAL strip that rides in the RYTM band header
-- and fills only while a cell is selected. Was a whole 240px NOTE column that
-- stood there permanently showing "CLICK A CELL IN THE GRID TO INSPECT IT" — a
-- fifth of the width spent on a hint. Same edits, no standing cost.
--
-- The row is ALWAYS there, one fixed height, blank when nothing is selected, and
-- never wraps. Appearing on select and vanishing on clear moved the grid under
-- the pointer: select a hit and the grid dropped a row, clear it and it jumped
-- back, so the next click landed on a different cell.
cellStrip :: forall m. State -> H.ComponentHTML Action () m
cellStrip s = HH.div [ style stripRow ] case s.selected >>= \sel -> map { sel, pat: _ } (activePattern s) of
  Nothing -> []
  Just { sel, pat } ->
      let c = P.cellAt pat sel.lane sel.step
      in   [ HH.span [ style $ "font-family:Georgia,serif;font-size:12px;font-weight:bold;color:" <> laneColor sel.lane ]
               [ HH.text (P.laneName sel.lane) ]
           , HH.span [ style $ engrave <> ";font-size:8px;opacity:0.6" ]
               [ HH.text ("STEP " <> show (sel.step + 1) <> " · ♪" <> show (P.noteOf pat sel.lane)) ]
           , paramCell "VEL" (show c.vel) (SetCellVel (-8)) (SetCellVel 8)
           , paramCell "PROB" (show c.prob <> "%") (SetCellProb (-10)) (SetCellProb 10)
           , paramCell "RATCHET" ("×" <> show c.ratchet) (SetCellRatchet (-1)) (SetCellRatchet 1)
           , HH.button
               [ HE.onClick \_ -> CycleCellCond
               , style $ "padding:3px 10px;border:1px solid #a8a392;border-radius:5px;cursor:pointer;"
                   <> "font-family:'SF Mono',Menlo,monospace;font-size:10px;color:#3f3c33;background:linear-gradient(#efece1,#ddd9cb)" ]
               [ HH.text (condDisplay c.cond) ]
           , HH.button
               [ HE.onClick \_ -> ClearSelected
               , style $ "padding:3px 10px;border:1px solid #a8a392;border-radius:5px;cursor:pointer;"
                   <> "font-family:Georgia,serif;font-size:10px;color:#8a3120;background:#efece1" ]
               [ HH.text "× clear" ]
           ]

-- `flex: 0 0 100%`: it sits in the band header, which wraps, and a blank strip
-- has no width, so without it the blank row would fold up onto the header's
-- first line and the bounce would be back.
stripRow :: String
stripRow = "flex:0 0 100%;display:flex;align-items:center;gap:12px;flex-wrap:nowrap;height:32px;overflow-x:auto;overflow-y:hidden;white-space:nowrap"

-- One inline parameter: label, − stepper, value, + stepper.
paramCell :: forall m. String -> String -> Action -> Action -> H.ComponentHTML Action () m
paramCell label val dec inc =
  HH.div [ style "display:flex;align-items:center;gap:5px" ]
    [ HH.span [ style $ engrave <> ";font-size:8px;opacity:0.7" ] [ HH.text label ]
    , stepBtn "−" dec
    , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:11px;color:#3f3c33;min-width:34px;text-align:center" ] [ HH.text val ]
    , stepBtn "+" inc
    ]

-- The condition button's label ("ALWAYS" reads better than the "—" glyph here).
condDisplay :: P.TrigCond -> String
condDisplay P.CAlways = "ALWAYS"
condDisplay c = P.condLabel c

-- The fixed-rhythm step grid: one row per used lane (kit name + GM note),
-- velocity as cell intensity, playhead sweeping the steps.
fixedSvg :: forall m. State -> Int -> P.FixedPattern -> H.ComponentHTML Action () m
fixedSvg s idx pat =
  let
    -- folded to the lanes in use, or the whole 16-lane kit when editing.
    lanes = if s.editing then range 0 (P.kitSize - 1) else P.usedLanes pat
    nLanes = length lanes
    cols = pat.steps
    -- WIDE cells. The band is ~8:1 but the grid's natural aspect was 3.25:1, so
    -- fitting it by height letterboxed it into the middle third with dead space
    -- either side while GRIDS's heatmap spanned the full width. Widening the
    -- cells (rather than scaling the drawing) makes the natural aspect match the
    -- band, so it fills the width at the height we want — and a wider cell is an
    -- easier click target besides. Everything else here derives from colW.
    colW = 38.0
    rowH = 28.0
    gutter = 34.0                       -- left margin for lane name + MIDI note
    w = gutter + toNumber cols * colW
    h = toNumber nLanes * rowH
    here = s.playStep `mod` cols
    colX step = gutter + toNumber step * colW
    laneEmpty lane = not (any (P.firesAt pat lane) (range 0 (cols - 1)))
    -- visuals only (the coloured hit + a faint slot in edit mode + the tweak-dot
    -- + the selection outline).
    rowVisuals row lane =
      range 0 (cols - 1) `concatMap'` \step ->
        let c = P.cellAt pat lane step
            v = c.vel
            x = colX step
            y = toNumber row * rowH
            slot = if s.editing && v <= 0
              then [ svgEl "rect"
                       [ svgAttr "x" (show (x + 2.0)), svgAttr "y" (show (y + 2.0))
                       , svgAttr "width" (show (colW - 4.0)), svgAttr "height" (show (rowH - 5.0)), svgAttr "rx" "2"
                       , svgAttr "fill" "none", svgAttr "stroke" (laneColor lane), svgAttr "stroke-opacity" "0.16"
                       , svgAttr "stroke-width" "0.8", svgAttr "style" "pointer-events:none" ] [] ]
              else []
            hit = if v <= 0 then []
              else [ svgRect (x + 2.0) (y + 2.0) (colW - 4.0) (rowH - 5.0) (laneColor lane)
                       (0.34 + toNumber v / 127.0 * 0.62) ]
            -- a small dot marks a hit whose prob/cond/ratchet overlay was tweaked.
            dot = if v > 0 && P.cellTweaked c
              then [ svgEl "circle"
                       [ svgAttr "cx" (show (x + colW - 3.6)), svgAttr "cy" (show (y + 4.6)), svgAttr "r" "1.9"
                       , svgAttr "fill" "#1c1a12", svgAttr "fill-opacity" "0.85", svgAttr "style" "pointer-events:none" ] [] ]
              else []
            sel = if s.selected == Just { lane, step }
              then [ svgEl "rect"
                       [ svgAttr "x" (show (x + 0.5)), svgAttr "y" (show (y + 0.5))
                       , svgAttr "width" (show (colW - 1.0)), svgAttr "height" (show (rowH - 1.0)), svgAttr "rx" "3"
                       , svgAttr "fill" "none", svgAttr "stroke" "#1c1a12", svgAttr "stroke-width" "1.4"
                       , svgAttr "stroke-opacity" "0.9", svgAttr "style" "pointer-events:none" ] [] ]
              else []
        in slot <> hit <> dot <> sel
    -- a transparent click target per cell, drawn last so it always wins clicks.
    rowTargets row lane =
      range 0 (cols - 1) `concatMap'` \step ->
        let x = colX step
            y = toNumber row * rowH
        in [ svgEl "rect"
               [ svgAttr "x" (show x), svgAttr "y" (show y)
               , svgAttr "width" (show (colW - 1.0)), svgAttr "height" (show (rowH - 1.0))
               , svgAttr "fill" "rgba(0,0,0,0)", svgAttr "style" "cursor:pointer;pointer-events:all"
               , svgOn "click" \e -> CellClick lane step (ME.shiftKey e) ] [] ]
    laneAt row = fromMaybe 0 (lanes !! row)
    cells = concatMap (\row -> rowVisuals row (laneAt row)) (range 0 (nLanes - 1))
    targets = concatMap (\row -> rowTargets row (laneAt row)) (range 0 (nLanes - 1))
    beatLines =
      range 0 (cols / 4) `concatMap'` \k ->
        let x = colX (k * 4)
        in [ svgEl "line"
               [ svgAttr "x1" (show x), svgAttr "y1" "0", svgAttr "x2" (show x), svgAttr "y2" (show h)
               , svgAttr "stroke" "#3f3c33", svgAttr "stroke-opacity" "0.18", svgAttr "stroke-width" "0.8"
               , svgAttr "style" "pointer-events:none" ] [] ]
    laneDivider row =
      svgEl "line"
        [ svgAttr "x1" (show gutter), svgAttr "y1" (show (toNumber row * rowH)), svgAttr "x2" (show w)
        , svgAttr "y2" (show (toNumber row * rowH))
        , svgAttr "stroke" "#3f3c33", svgAttr "stroke-opacity" "0.12", svgAttr "stroke-width" "0.6"
        , svgAttr "style" "pointer-events:none" ] []
    -- empty lanes (only visible while editing) are dimmed; named-and-used ones full.
    rowLabel row =
      let lane = laneAt row
          op = if laneEmpty lane then "0.4" else "0.9"
      in [ svgEl "text"
             [ svgAttr "x" "3", svgAttr "y" (show (toNumber row * rowH + 12.0))
             , svgAttr "fill" (laneColor lane), svgAttr "fill-opacity" op, svgAttr "style" "pointer-events:none"
             , svgAttr "font-size" "9", svgAttr "font-weight" "bold", svgAttr "font-family" "Georgia,serif" ]
             [ HH.text (P.laneName lane) ]
         , noteTag 3.0 (toNumber row * rowH + 23.0) (NFixed idx lane) (P.noteOf pat lane)
         ]
    playhead =
      svgEl "rect"
        [ svgAttr "x" (show (colX here)), svgAttr "y" "0"
        , svgAttr "width" (show colW), svgAttr "height" (show h)
        , svgAttr "fill" "#1c1a12", svgAttr "fill-opacity" (if s.sounding /= Silent then "0.10" else "0.0")
        , svgAttr "stroke" "#1c1a12", svgAttr "stroke-opacity" (if s.sounding /= Silent then "0.5" else "0.15")
        , svgAttr "stroke-width" "1", svgAttr "style" "pointer-events:none" ] []
  in
    svgEl "svg"
      [ svgAttr "viewBox" ("0 0 " <> show w <> " " <> show h)
      -- Aspect-preserving at full band width, which now lands at ~180px for the
      -- usual 6 lanes because `colW` matches the band's shape. EDIT mode reveals
      -- all 16 lanes and legitimately needs more room, so the cap is generous and
      -- the surface scrolls rather than squashing it.
      , svgAttr "width" "100%"
      , svgAttr "preserveAspectRatio" "xMidYMid meet"
      , svgAttr "style" "display:block;max-height:60vh" ]
      ( cells <> beatLines
          <> map laneDivider (range 1 (nLanes - 1))
          <> [ playhead ] <> concatMap rowLabel (range 0 (nLanes - 1)) <> targets )
