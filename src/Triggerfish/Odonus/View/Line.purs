-- | THE LINE: Melichthys's material, where Odonus has its grids
-- | (`Reef.Melichthys`). One drawn melody of sixteen to thirty-two bars, with
-- | its own rhythm, shown four bars to a row with the heads moving over it.
-- | It is not edited by hand and nothing mutates it (AC, 2026-11-15): the
-- | controls here draw another, and start the heads again from the top.
-- |
-- | WAVES shape it as an LFO shapes a patch (AC, 2026-11-16): three slow
-- | waves over the line's length, each sent by its own amount to the notes'
-- | loudness (DYN) and to their pitch (BEND). The strip shows the line as it
-- | comes out: a louder note is a thicker bar.
-- |
-- | The drawing is a plain strip for now. What it wants to become is a small,
-- | tidy piece of partitura (AC, the same day), which is why it is a view of
-- | its own that takes only the line and the heads.
module Triggerfish.Odonus.View.Line (linePanel) where

import Prelude

import Data.Array (concat, concatMap, filter, foldl, mapWithIndex, range, (!!))
import Data.Foldable (maximum, minimum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Widgets.Svg (svgAttr, svgEl)
import Reef.Melichthys as Melichthys
import Data.String (joinWith)
import Reef.Melody (barSteps)
import Triggerfish.Odonus.Grid.Types (Action(..), GenKind(..), KnobTarget(..), Slots, State)
import Triggerfish.Odonus.Grid.Widgets (engrave, genRow, headColor, labelledRow, miniKnob, octLabel, panelShell, style, tabBtn)
import Triggerfish.Odonus.Model as M
import Triggerfish.Odonus.View.Grid (transportRow)
import Triggerfish.Odonus.View.Key (gridPiece)

linePanel :: forall m. State -> H.ComponentHTML Action Slots m
linePanel s =
  panelShell s.collapsed "LINE" "one melody · four heads" "flex:1 1 560px;min-width:420px"
    [ HH.div [ style "display:flex;gap:22px;align-items:flex-start;flex-wrap:wrap" ]
        [ HH.div [ style "flex:0 0 280px" ]
            [ gridPiece s
            , labelledRow "OCTAVE"
                (map (\n -> tabBtn (octLabel n) (s.odo.octaveShift == n) (SetOctave n)) [ -2, -1, 0, 1, 2 ])
            , labelledRow "DEGREE"
                (map (\d -> tabBtn (show (d + 1)) (s.odo.degShift == d) (SetDegShift d)) [ 0, 1, 2, 3, 4, 5, 6 ])
            ]
        , HH.div [ style "flex:0 0 250px;display:flex;flex-direction:column;gap:8px" ]
            ( [ transportRow s ] <> map (genRow s) [ GHeads, GTransp, GSpeed ] )
        ]
    , drawRow s
    , case s.odo.line of
        Just l -> HH.div_ [ wavesRows l, strip s.odo (Melichthys.sounded l) ]
        Nothing -> HH.text ""
    ]

-- | A new line at a length (the length lit is the one playing; pressing it
-- | again draws another of the same), and the heads from the top.
drawRow :: forall m. State -> H.ComponentHTML Action Slots m
drawRow s =
  let
    bars = maybe 0 _.bars s.odo.line
    seed = maybe 0 _.seed s.odo.line
  in
    HH.div [ style "display:flex;align-items:flex-end;gap:14px;margin:14px 0 10px" ]
      [ HH.div [ HP.title "draw a new line of this many bars; the heads start it from the top, each at its entry" ]
          [ HH.div [ style $ engrave <> ";font-size:9px;margin-bottom:4px" ] [ HH.text "NEW LINE · BARS" ]
          , HH.div [ style "display:flex;gap:3px;width:190px" ]
              (map (\b -> tabBtn (show b) (b == bars) (DrawLine b)) [ 8, 16, 24, 32 ])
          ]
      , HH.button
          [ HE.onClick \_ -> RestartLine
          , HP.title (if s.entriesMoved then "an entry was moved: start the heads again to hear it" else "every head back to the start of the line, each coming in at its entry")
          , style $ "padding:5px 12px;border:1px solid #a8a392;border-radius:5px;cursor:pointer;font-family:Georgia,serif;font-size:10px;letter-spacing:0.08em;"
              <> (if s.entriesMoved then "background:linear-gradient(#c8a86a,#b8975a);color:#1c1a12" else "background:linear-gradient(#efece1,#ddd9cb);color:#3f3c33") ]
          [ HH.text "↺ FROM THE TOP" ]
      , HH.span [ style "margin-left:auto;font-family:'SF Mono',Menlo,monospace;font-size:10px;color:#6a6456" ]
          [ HH.text ("line " <> show seed) ]
      ]

-- | The three waves, one to a row: its shape, how many cycles it makes in the
-- | line, where in its cycle the line starts, its two sends, and the wave as
-- | it lies over the whole line. A turn is heard on the notes still to come.
wavesRows :: forall m. M.Line -> H.ComponentHTML Action Slots m
wavesRows l =
  HH.div [ style "margin:4px 0 12px" ]
    ( [ HH.div [ style $ engrave <> ";font-size:9px;margin-bottom:2px" ] [ HH.text "WAVES · OVER THE WHOLE LINE" ] ]
        <> mapWithIndex waveRow l.waves )
  where
  shapes = [ "∿", "△", "◿", "◺", "⊓", "?" ]
  shapeNames = [ "sine", "triangle", "ramp up", "ramp down", "square", "random levels, one to a cycle" ]
  ink = "#4a463b"
  signed n = if n > 0 then "+" <> show n else show n
  waveRow i w =
    HH.div [ style "display:flex;align-items:center;gap:10px;padding:3px 0;border-top:1px solid #00000012" ]
      [ HH.div [ style "display:flex;gap:2px;width:150px" ]
          ( mapWithIndex
              (\k glyph -> HH.span [ HP.title (fromMaybe "" (shapeNames !! k)), style "flex:1;display:flex" ]
                 [ tabBtn glyph (w.shape == k) (SetLineWave i 0 k) ])
              shapes )
      , HH.div [ HP.title "cycles in the line" ] [ miniKnob (LineWave i 1) w.cycles ink "CYCLES" (show w.cycles) ]
      , HH.div [ HP.title "where in its cycle the line starts, in sixteenths of a cycle" ] [ miniKnob (LineWave i 2) w.phase ink "PHASE" (show w.phase <> "/16") ]
      , HH.div [ HP.title "how much of this wave goes to the notes' loudness: whole is forty either side" ] [ miniKnob (LineWave i 3) w.dyn ink "DYN" (signed w.dyn) ]
      , HH.div [ HP.title "how much of this wave goes to the notes' pitch: whole is an octave of the scale either side" ] [ miniKnob (LineWave i 4) w.bend ink "BEND" (signed w.bend) ]
      , trace i w
      ]
  trace i w =
    let
      points = map
        (\x -> show (toNumber x * 2.0) <> "," <> show (15.0 - toNumber (Melichthys.waveAt l.seed i w x 128) * 12.0 / 127.0))
        (range 0 127)
      idle = w.dyn == 0 && w.bend == 0
    in
      svgEl "svg"
        [ svgAttr "viewBox" "0 0 256 30", svgAttr "preserveAspectRatio" "none", svgAttr "width" "100%", svgAttr "height" "30"
        , svgAttr "style" "flex:1;min-width:80px;background:#e6e2d4;border-radius:4px;box-shadow:inset 0 0 0 1px #00000014" ]
        [ svgEl "polyline"
            [ svgAttr "points" (joinWith " " points), svgAttr "fill" "none"
            , svgAttr "stroke" ink, svgAttr "stroke-width" "1.2", svgAttr "opacity" (if idle then "0.3" else "0.9")
            , svgAttr "vector-effect" "non-scaling-stroke" ] [] ]

-- | Four bars to a row. A note is a bar at its degree's height, as long as it
-- | lasts; a rest is a gap. Each head that has come in is a rule in its
-- | colour on the note it is reading, and that note is filled with it.
strip :: forall m. M.Odonus -> Array Melichthys.Sounded -> H.ComponentHTML Action Slots m
strip odo line =
  let
    placed = (foldl (\acc n -> { at: acc.at + n.dur, out: acc.out <> [ { at: acc.at, dur: n.dur, degree: n.degree, rest: n.rest, vel: n.vel, cell: acc.cell } ], cell: acc.cell + 1 })
      { at: 0, out: [], cell: 0 } line).out
    total = foldl (\t n -> t + n.dur) 0 line
    sounding = map _.degree (filter (not <<< _.rest) placed)
    lo = fromMaybe 0 (minimum sounding)
    hi = fromMaybe 0 (maximum sounding)
    rowSteps = 4 * barSteps
    rows = max 1 ((total + rowSteps - 1) / rowSteps)
    stepW = 10.0
    rowH = 54.0
    padY = 7.0
    w = toNumber rowSteps * stepW
    yOf d = padY + toNumber (hi - d) * (rowH - 2.0 * padY) / toNumber (max 1 (hi - lo))
    -- the heads that have come in and sound, by the cell each is reading
    reading = concat (mapWithIndex (\i h -> if h.mute || Melichthys.waiting odo h then [] else [ { head: i, cell: h.cursor } ]) odo.heads)
    headsOn c = filter (\r -> r.cell == c) reading
    num = show
    -- a louder note is a thicker bar
    thick v = 1.5 + toNumber v * 6.0 / 127.0
    row r =
      let
        from = r * rowSteps
        inRow = filter (\n -> n.at < from + rowSteps && n.at + n.dur > from) placed
        barLine b =
          [ svgEl "line"
              [ svgAttr "x1" (num (toNumber (b * barSteps) * stepW)), svgAttr "y1" "0"
              , svgAttr "x2" (num (toNumber (b * barSteps) * stepW)), svgAttr "y2" (num rowH)
              , svgAttr "stroke" "#00000026", svgAttr "stroke-width" "1" ] []
          , svgEl "text"
              [ svgAttr "x" (num (toNumber (b * barSteps) * stepW + 3.0)), svgAttr "y" "9"
              , svgAttr "font-size" "8", svgAttr "font-family" "Georgia,serif", svgAttr "fill" "#8a8473" ]
              [ HH.text (show (r * 4 + b + 1)) ]
          ]
        note n =
          let
            x0 = toNumber (max from n.at - from) * stepW
            x1 = toNumber (min (from + rowSteps) (n.at + n.dur) - from) * stepW
            on = headsOn n.cell
            fill = maybe "#4a463b" (\o -> headColor o.head) (on !! 0)
          in
            if n.rest then []
            else
              [ svgEl "rect"
                  [ svgAttr "x" (num (x0 + 0.5)), svgAttr "y" (num (yOf n.degree - thick n.vel / 2.0))
                  , svgAttr "width" (num (max 1.0 (x1 - x0 - 1.5))), svgAttr "height" (num (thick n.vel)), svgAttr "rx" "1.5"
                  , svgAttr "fill" fill ] [] ]
                <> concat (mapWithIndex
                     (\k o ->
                        [ svgEl "line"
                            [ svgAttr "x1" (num (x0 + 1.0 + toNumber k * 2.5)), svgAttr "y1" "1"
                            , svgAttr "x2" (num (x0 + 1.0 + toNumber k * 2.5)), svgAttr "y2" (num (rowH - 1.0))
                            , svgAttr "stroke" (headColor o.head), svgAttr "stroke-width" "2", svgAttr "opacity" "0.85" ] [] ])
                     on)
      in
        svgEl "svg"
          [ svgAttr "viewBox" ("0 0 " <> num w <> " " <> num rowH), svgAttr "preserveAspectRatio" "none"
          , svgAttr "width" "100%", svgAttr "height" (num rowH)
          , svgAttr "style" "display:block;background:#e6e2d4;border-radius:5px;margin-bottom:5px;box-shadow:inset 0 0 0 1px #00000014" ]
          (concatMap barLine (range 0 3) <> concatMap note inRow)
  in
    HH.div [ style "flex:1;min-height:0;overflow-y:auto" ] (map row (range 0 (rows - 1)))
