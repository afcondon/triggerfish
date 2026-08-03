-- | Triggerfish.Balistes.View.Fixed — the GRIDS (fixed-rhythm) tab: the library
-- | switcher chips, the literal lane-grid editor (`fixedSvg`), and the NOTE
-- | inspector column that edits the selected cell's velocity / probability /
-- | condition / ratchet. Pure renderers over `State`.
module Triggerfish.Balistes.View.Fixed
  ( inspectorPanel
  , patternSwitcher
  , fixedBody
  ) where

import Prelude

import Data.Array (concatMap, length, mapWithIndex, range, (!!))
import Data.Foldable (any)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.MouseEvent as ME
import Triggerfish.Odonus.Grid.Widgets (engrave, style, svgAttr, svgEl)
import Triggerfish.Balistes.Pattern as P
import Triggerfish.Transport (Sounding(..))
import Triggerfish.Balistes.Types (Action(..), Active(..), NoteRef(..), State, activePattern)
import Triggerfish.Balistes.Widgets
  ( panel, flatBtn, armBtn, stepBtn, chip, newChip, svgMouse, svgRect, noteTag
  , laneColor, concatMap' )

-- The NOTE inspector — the per-cell editor that fills the column CONTROL
-- vacates for a fixed rhythm. Edits the selected cell's velocity / probability /
-- trig-condition / ratchet (the overlay the grid's tweak-dot flags).
inspectorPanel :: forall m. State -> H.ComponentHTML Action () m
inspectorPanel s =
  panel "NOTE" "flex:0 0 240px"
    [ case s.active, s.selected of
        AFixed _, Just sel -> case activePattern s of
          Just pat -> cellInspector pat sel
          Nothing -> inspectorHint
        _, _ -> inspectorHint
    ]

inspectorHint :: forall m. H.ComponentHTML Action () m
inspectorHint =
  HH.div [ style $ engrave <> ";font-size:9px;opacity:0.55;line-height:1.8;margin-top:8px" ]
    [ HH.text "CLICK A CELL IN THE GRID TO INSPECT IT — VELOCITY · PROBABILITY · CONDITION · RATCHET. SHIFT-CLICK CLEARS A CELL." ]

cellInspector :: forall m. P.FixedPattern -> { lane :: Int, step :: Int } -> H.ComponentHTML Action () m
cellInspector pat sel =
  let c = P.cellAt pat sel.lane sel.step
  in HH.div [ style "display:flex;flex-direction:column;gap:13px;margin-top:6px" ]
       [ HH.div [ style "display:flex;align-items:baseline;justify-content:space-between" ]
           [ HH.span [ style $ "font-family:Georgia,serif;font-size:16px;font-weight:bold;color:" <> laneColor sel.lane ]
               [ HH.text (P.laneName sel.lane) ]
           , HH.span [ style $ engrave <> ";font-size:9px;opacity:0.6" ]
               [ HH.text ("STEP " <> show (sel.step + 1) <> " · ♪" <> show (P.noteOf pat sel.lane)) ]
           ]
       , paramRow "VELOCITY" (show c.vel) (SetCellVel (-8)) (SetCellVel 8)
       , paramRow "PROBABILITY" (show c.prob <> "%") (SetCellProb (-10)) (SetCellProb 10)
       , paramRow "RATCHET" ("×" <> show c.ratchet) (SetCellRatchet (-1)) (SetCellRatchet 1)
       , HH.div [ style "display:flex;align-items:center;justify-content:space-between;border-bottom:1px dotted #0000001a;padding-bottom:9px" ]
           [ HH.span [ style $ engrave <> ";font-size:9px" ] [ HH.text "CONDITION" ]
           , HH.button
               [ HE.onClick \_ -> CycleCellCond
               , style $ "padding:5px 14px;border:1px solid #a8a392;border-radius:5px;cursor:pointer;"
                   <> "font-family:'SF Mono',Menlo,monospace;font-size:12px;color:#3f3c33;background:linear-gradient(#efece1,#ddd9cb)" ]
               [ HH.text (condDisplay c.cond) ]
           ]
       , flatBtn "× CLEAR CELL" ClearSelected
       ]

-- The condition button's label ("ALWAYS" reads better than the "—" glyph here).
condDisplay :: P.TrigCond -> String
condDisplay P.CAlways = "ALWAYS"
condDisplay c = P.condLabel c

-- One inspector parameter row: label, − stepper, value, + stepper.
paramRow :: forall m. String -> String -> Action -> Action -> H.ComponentHTML Action () m
paramRow label val dec inc =
  HH.div [ style "display:flex;align-items:center;justify-content:space-between;border-bottom:1px dotted #0000001a;padding-bottom:9px" ]
    [ HH.span [ style $ engrave <> ";font-size:9px" ] [ HH.text label ]
    , HH.div [ style "display:flex;align-items:center;gap:9px" ]
        [ stepBtn "−" dec
        , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:13px;color:#3f3c33;width:46px;text-align:center" ] [ HH.text val ]
        , stepBtn "+" inc
        ]
    ]

-- Within the GRIDS tab: the library of user rhythms as chips (the drum model is
-- now the tab, so the morph engine is no longer a chip here). Clicking switches
-- which rhythm plays.
patternSwitcher :: forall m. State -> H.ComponentHTML Action () m
patternSwitcher s =
  HH.div [ style "display:flex;gap:6px;flex-wrap:wrap;margin-bottom:16px;max-width:640px" ]
    ( mapWithIndex (\i pat -> chip pat.name (s.active == AFixed i) (SelectPattern (AFixed i))) s.library
        <> [ newChip ] )

-- A fixed rhythm: the literal lane grid (folded to used lanes, or all 16 when
-- editing), click-to-toggle cells, draggable per-lane notes.
fixedBody :: forall m. State -> Int -> P.FixedPattern -> H.ComponentHTML Action () m
fixedBody s idx pat =
  HH.div_
    [ HH.div [ style "display:flex;align-items:center;gap:10px;max-width:640px;margin:0 auto 12px" ]
        [ HH.input
            [ HP.value pat.name
            , HE.onValueInput SetPatternName
            , style $ "padding:5px 9px;border:1px solid #a8a392;border-radius:5px;background:#f3f1e8;"
                <> "font-family:Georgia,serif;font-size:13px;color:#1c1a12;width:150px" ]
        , armBtn (if s.editing then "● EDITING" else "EDIT") s.editing ToggleEdit
        , armBtn "PUBLISH ⚱" false PublishActive
        , case s.publishMsg of
            Just msg -> HH.span [ style $ engrave <> ";font-size:8px;opacity:0.8;color:#2f6a4a" ] [ HH.text msg ]
            Nothing ->
              HH.span [ style $ engrave <> ";font-size:8px;opacity:0.6;line-height:1.5" ]
                [ HH.text (if s.editing
                    then "ALL 16 LANES — CLICK CELLS TO TOGGLE HITS · DRAG A ♪NOTE TO RETUNE A LANE."
                    else "CLICK A CELL TO TOGGLE A HIT · EDIT REVEALS ALL 16 LANES TO ADD VOICES.") ]
        ]
    , HH.div [ style "width:100%;max-width:640px;margin:0 auto" ] [ fixedSvg s idx pat ]
    , HH.div [ style $ engrave <> ";font-size:8px;opacity:0.5;margin-top:10px;line-height:1.6;max-width:640px" ]
        [ HH.text ("STARTER RHYTHM · " <> show pat.steps <> " STEPS · " <> show (length (P.usedLanes pat)) <> " OF 16 LANES IN USE. A FIXED LOOP — RECALL INSTANTLY, EDIT TO TASTE. SAMPLES SWAP DOWNSTREAM.") ]
    ]

-- The fixed-rhythm step grid: one row per used lane (kit name + GM note),
-- velocity as cell intensity, playhead sweeping the steps.
fixedSvg :: forall m. State -> Int -> P.FixedPattern -> H.ComponentHTML Action () m
fixedSvg s idx pat =
  let
    -- folded to the lanes in use, or the whole 16-lane kit when editing.
    lanes = if s.editing then range 0 (P.kitSize - 1) else P.usedLanes pat
    nLanes = length lanes
    cols = pat.steps
    colW = 16.0
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
               , svgMouse "click" \e -> CellClick lane step (ME.shiftKey e) ] [] ]
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
      , svgAttr "width" "100%", svgAttr "style" "display:block;max-height:90vh" ]
      ( cells <> beatLines
          <> map laneDivider (range 1 (nLanes - 1))
          <> [ playhead ] <> concatMap rowLabel (range 0 (nLanes - 1)) <> targets )
