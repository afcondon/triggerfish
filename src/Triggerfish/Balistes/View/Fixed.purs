-- | Triggerfish.Balistes.View.Fixed — the RYTM (fixed-rhythm) tab: the library
-- | switcher chips, the literal lane-grid editor (`fixedSvg`), and the NOTE
-- | inspector column that edits the selected cell's velocity / probability /
-- | condition / ratchet. Pure renderers over `State`.
module Triggerfish.Balistes.View.Fixed
  ( fixedSvg
  , cellStrip
  , lengthControl
  ) where

import Prelude

import Reef.Balistes.Kit as Kit

import Data.Array (concatMap, head, length, range, (!!))
import Data.Foldable (all, any, elem)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Web.UIEvent.MouseEvent as ME
import Triggerfish.Ui.Style (engrave, style)
import Halogen.Widgets.Svg (svgAttr, svgEl, svgOn)
import Triggerfish.Balistes.Pattern as P
import Triggerfish.Transport (Sounding(..))
import Triggerfish.Balistes.Types (Action(..), State, activePattern)
import Triggerfish.Balistes.Widgets
  ( stepBtn, armBtn, svgRect, laneHead, laneColor, concatMap' )

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
cellStrip s = HH.div [ style stripRow ] case activePattern s of
  Just pat | Just first <- head s.selected ->
      let cells = map (\sp -> P.cellAt pat sp.lane sp.step) s.selected
          c = P.cellAt pat first.lane first.step
          -- one value, or "mixed" when the group's hits differ
          shown :: forall a. Eq a => (P.Cell -> a) -> String -> String
          shown f txt = if all (\x -> f x == f c) cells then txt else "mixed"
          one = length s.selected == 1
      in   [ HH.span [ style $ "font-family:Georgia,serif;font-size:12px;font-weight:bold;color:" <> (if one then laneColor first.lane else "#3f3c33") ]
               [ HH.text (if one then P.laneName first.lane else show (length s.selected) <> " hits") ]
           , HH.span [ style $ engrave <> ";font-size:8px;opacity:0.6" ]
               [ HH.text (if one then "LANE " <> show (first.lane + 1) <> " · STEP " <> show (first.step + 1)
                          else "⌫ CLEAR · ←→ MOVE · ⌘C ⌘V") ]
           , paramCell "VEL" (shown _.vel (show c.vel)) (SetCellVel (-8)) (SetCellVel 8)
           , paramCell "PROB" (shown _.prob (show c.prob <> "%")) (SetCellProb (-10)) (SetCellProb 10)
           , paramCell "RATCHET" (shown _.ratchet ("×" <> show c.ratchet)) (SetCellRatchet (-1)) (SetCellRatchet 1)
           , HH.button
               [ HE.onClick \_ -> CycleCellCond
               , style $ "padding:3px 10px;border:1px solid #a8a392;border-radius:5px;cursor:pointer;"
                   <> "font-family:'SF Mono',Menlo,monospace;font-size:10px;color:#3f3c33;background:linear-gradient(#efece1,#ddd9cb)" ]
               [ HH.text (shown _.cond (condDisplay c.cond)) ]
           , HH.button
               [ HE.onClick \_ -> ClearSelected
               , style $ "padding:3px 10px;border:1px solid #a8a392;border-radius:5px;cursor:pointer;"
                   <> "font-family:Georgia,serif;font-size:10px;color:#8a3120;background:#efece1" ]
               [ HH.text "× clear" ]
           ]
  _ -> []

-- `flex: 0 0 100%`: it sits in the band header, which wraps, and a blank strip
-- has no width, so without it the blank row would fold up onto the header's
-- first line and the bounce would be back.
stripRow :: String
stripRow = "flex:0 0 100%;display:flex;align-items:center;gap:12px;flex-wrap:nowrap;height:32px;overflow-x:auto;overflow-y:hidden;white-space:nowrap"

-- The rhythm's length: four picks that repeat it into the new bars, and a
-- step either way.
lengthControl :: forall m. P.FixedPattern -> H.ComponentHTML Action () m
lengthControl pat =
  HH.div [ style "display:flex;align-items:center;gap:5px" ]
    ( [ HH.span [ style $ engrave <> ";font-size:8px;opacity:0.7" ] [ HH.text "LENGTH" ] ]
      <> map pick [ 16, 32, 48, 64 ]
      <> [ stepBtn "−" (NudgeLength (-1))
         , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:11px;color:#3f3c33;min-width:20px;text-align:center" ]
             [ HH.text (show pat.steps) ]
         , stepBtn "+" (NudgeLength 1) ] )
  where
  pick n = armBtn (show n) (pat.steps == n) (SetLength n)

-- One inline parameter: label, − stepper, value, + stepper.
paramCell :: forall m. String -> String -> Action -> Action -> H.ComponentHTML Action () m
paramCell label val dec inc =
  HH.div [ style "display:flex;align-items:center;gap:5px" ]
    [ HH.span [ style $ engrave <> ";font-size:8px;opacity:0.7" ] [ HH.text label ]
    , stepBtn "−" dec
    , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:11px;color:#3f3c33;min-width:34px;text-align:center" ] [ HH.text val ]
    , stepBtn "+" inc
    ]

isEvery :: P.TrigCond -> Boolean
isEvery = case _ of
  P.CEvery _ _ -> true
  _ -> false

-- The condition button's label ("ALWAYS" reads better than the "—" glyph here).
condDisplay :: P.TrigCond -> String
condDisplay P.CAlways = "ALWAYS"
condDisplay c = P.condLabel c

-- The fixed-rhythm step grid: one row per used lane (kit name + GM note),
-- velocity as cell intensity, playhead sweeping the steps.
fixedSvg :: forall m. State -> P.FixedPattern -> H.ComponentHTML Action () m
fixedSvg s pat =
  let
    -- folded to the lanes in use, or the whole 16-lane kit when editing.
    lanes = if s.editing then range 0 (P.kitSize - 1) else P.usedLanes pat
    nLanes = length lanes
    cols = pat.steps
    -- The drawing is always sixteen of the old 38-wide cells across, whatever
    -- the length (AC, 2026-10-14): a bar fills the band as GRIDS's heatmap
    -- does, and a longer rhythm narrows its cells rather than its rows, so 64
    -- steps sit at half a Grids cell. Everything else here derives from colW.
    colW = 608.0 / toNumber (max 1 cols)
    rowH = 28.0
    gutter = 34.0                       -- left margin for the lane's head
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
            sounds = P.soundsIn s.fill c
            v = if sounds then c.vel else 0
            x = colX step
            y = toNumber row * rowH
            -- a hit that sounds only in the other view (fill-only in the
            -- rhythm, NOT FILL in the fill): a dashed ghost, so the delta shows
            ghost = if c.vel > 0 && not sounds
              then [ svgEl "rect"
                       [ svgAttr "x" (show (x + 2.5)), svgAttr "y" (show (y + 2.5))
                       , svgAttr "width" (show (colW - 5.0)), svgAttr "height" (show (rowH - 6.0)), svgAttr "rx" "2"
                       , svgAttr "fill" "none", svgAttr "stroke" (laneColor lane), svgAttr "stroke-opacity" "0.6"
                       , svgAttr "stroke-width" "0.9", svgAttr "stroke-dasharray" "2 1.5", svgAttr "style" "pointer-events:none" ] [] ]
              else []
            -- in the fill, its own hits carry a brass bar along the top
            fillMark = if s.fill && sounds && c.cond == P.CFill
              then [ svgEl "rect"
                       [ svgAttr "x" (show (x + 2.0)), svgAttr "y" (show (y + 2.0))
                       , svgAttr "width" (show (colW - 4.0)), svgAttr "height" "3"
                       , svgAttr "fill" "#b8975a", svgAttr "style" "pointer-events:none" ] [] ]
              else []
            slot = if s.editing && v <= 0 && c.vel <= 0
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
            dot = if v > 0 && (c.prob /= 100 || c.ratchet /= 1 || isEvery c.cond)
              then [ svgEl "circle"
                       [ svgAttr "cx" (show (x + colW - 3.6)), svgAttr "cy" (show (y + 4.6)), svgAttr "r" "1.9"
                       , svgAttr "fill" "#1c1a12", svgAttr "fill-opacity" "0.85", svgAttr "style" "pointer-events:none" ] [] ]
              else []
            sel = if elem { lane, step } s.selected
              then [ svgEl "rect"
                       [ svgAttr "x" (show (x + 0.5)), svgAttr "y" (show (y + 0.5))
                       , svgAttr "width" (show (colW - 1.0)), svgAttr "height" (show (rowH - 1.0)), svgAttr "rx" "3"
                       , svgAttr "fill" "none", svgAttr "stroke" "#1c1a12", svgAttr "stroke-width" "1.4"
                       , svgAttr "stroke-opacity" "0.9", svgAttr "style" "pointer-events:none" ] [] ]
              else []
        in slot <> ghost <> hit <> fillMark <> dot <> sel
    -- a transparent click target per cell, drawn last so it always wins clicks.
    rowTargets row lane =
      range 0 (cols - 1) `concatMap'` \step ->
        let x = colX step
            y = toNumber row * rowH
        in [ svgEl "rect"
               [ svgAttr "x" (show x), svgAttr "y" (show y)
               , svgAttr "width" (show (colW - 1.0)), svgAttr "height" (show (rowH - 1.0))
               , svgAttr "fill" "rgba(0,0,0,0)", svgAttr "style" "cursor:pointer;pointer-events:all"
               , svgOn "mousedown" \e -> CellDown lane step (ME.shiftKey e)
               , svgOn "mouseenter" \_ -> PaintOver lane step ] [] ]
    laneAt row = fromMaybe 0 (lanes !! row)
    cells = concatMap (\row -> rowVisuals row (laneAt row)) (range 0 (nLanes - 1))
    targets = concatMap (\row -> rowTargets row (laneAt row)) (range 0 (nLanes - 1))
    beatLines =
      range 0 (cols / 4) `concatMap'` \k ->
        let x = colX (k * 4)
            bar = k `mod` 4 == 0
        in [ svgEl "line"
               [ svgAttr "x1" (show x), svgAttr "y1" "0", svgAttr "x2" (show x), svgAttr "y2" (show h)
               , svgAttr "stroke" "#3f3c33", svgAttr "stroke-opacity" (if bar then "0.42" else "0.16")
               , svgAttr "stroke-width" (if bar then "1.2" else "0.8")
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
          muted = Kit.muted s.mutes lane
      in laneHead { top: toNumber row * rowH, height: rowH, width: gutter, lane
                  , name: P.laneName lane, number: show (lane + 1), color: laneColor lane
                  , opacity: if muted then "0.3" else op, mutes: [ lane ], muted }
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
      , svgAttr "style" "display:block;max-height:60vh;user-select:none;-webkit-user-select:none" ]
      ( cells <> beatLines
          <> map laneDivider (range 1 (nLanes - 1))
          <> [ playhead ] <> concatMap rowLabel (range 0 (nLanes - 1)) <> targets )
