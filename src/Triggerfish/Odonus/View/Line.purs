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

import Data.Array (concat, concatMap, filter, find, foldl, last, mapWithIndex, range, (!!))
import Data.Foldable (maximum, minimum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Widgets.Svg (svgAttr, svgEl)
import Reef.Melichthys as Melichthys
import Data.String (Pattern(..), contains, joinWith)
import Rebus as Rebus
import Reef.Melody (barSteps)
import Reef.Vetula.NoteText (parseProgression)
import Triggerfish.Odonus.View.Progression (chordName)
import Triggerfish.Odonus.Grid.Types (Action(..), GenKind(..), KnobTarget(..), Slots, State)
import Triggerfish.Odonus.Grid.Widgets (engrave, genRow, headColor, labelledRow, miniKnob, octLabel, panelShell, style, tabBtn)
import Triggerfish.Odonus.Model as M
import Triggerfish.Odonus.View.Grid (clockRow, feelBlock)
import Triggerfish.Odonus.View.Key (gridPiece)
import Triggerfish.Glyph (bundleRebus)
import Triggerfish.GlyphView (faIcon)

linePanel :: forall m. State -> H.ComponentHTML Action Slots m
linePanel s =
  panelShell s.collapsed "LINE" "one melody · four heads" "flex:1 1 560px;min-width:420px"
    -- what the notes are held to, together at the top: the scale, then the
    -- ground's chords under it (AC, 2026-11-16)
    [ gridPiece s
    , maybe (HH.text "") (groundRow s) s.odo.line
    -- two columns (AC, 2026-11-16): where the line sits and how fast, and
    -- drawing a new one; then its feel and what changes the heads by itself
    , HH.div [ style "display:flex;gap:26px;align-items:flex-start;flex-wrap:wrap;margin-bottom:12px" ]
        [ HH.div [ style "flex:0 0 340px" ]
            [ labelledRow "OCTAVE"
                (map (\n -> tabBtn (octLabel n) (s.odo.octaveShift == n) (SetOctave n)) [ -2, -1, 0, 1, 2 ])
            , clockRow s
            , labelledRow "DEGREE"
                (map (\d -> tabBtn (show (d + 1)) (s.odo.degShift == d) (SetDegShift d)) [ 0, 1, 2, 3, 4, 5, 6 ])
            , drawRow s
            ]
        , HH.div [ style "flex:0 0 250px;display:flex;flex-direction:column;gap:8px" ]
            ( [ HH.div [ style "display:flex;justify-content:space-around;margin-bottom:6px" ] [ feelBlock s ] ]
                <> map (genRow s) [ GHeads, GTransp, GSpeed ] )
        ]
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
    HH.div [ HP.title "draw a new line of this many bars; the heads start it from the top, each at its entry" ]
      [ HH.div [ style "display:flex;align-items:baseline;justify-content:space-between;margin-bottom:4px" ]
          [ HH.span [ style $ engrave <> ";font-size:9px" ] [ HH.text "NEW LINE · BARS" ]
          , HH.span [ style "font-family:'SF Mono',Menlo,monospace;font-size:10px;color:#6a6456" ] [ HH.text ("line " <> show seed) ]
          ]
      , HH.div [ style "display:flex;align-items:center;gap:10px" ]
          [ HH.div [ style "display:flex;gap:3px;flex:1" ]
              (map (\b -> tabBtn (show b) (b == bars) (DrawLine b)) [ 8, 16, 24, 32 ])
          , HH.button
              [ HE.onClick \_ -> RestartLine
              , HP.title (if s.entriesMoved then "an entry was moved: start the heads again to hear it" else "every head back to the start of the line, each coming in at its entry")
              , style $ "padding:5px 12px;border:1px solid #a8a392;border-radius:5px;cursor:pointer;white-space:nowrap;font-family:Georgia,serif;font-size:10px;letter-spacing:0.08em;"
                  <> (if s.entriesMoved then "background:linear-gradient(#c8a86a,#b8975a);color:#1c1a12" else "background:linear-gradient(#efece1,#ddd9cb);color:#3f3c33") ]
              [ HH.text "↺ FROM THE TOP" ]
          ]
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

-- | The ground: a kept progression of Vetula's the heads play over, a chord
-- | every so many bars of time, the same chord for every head. PULL chooses
-- | how much of the line goes to the chord's nearest tones: the notes that
-- | carry most first (bar lines, long notes, phrase ends), every note at 100.
-- | The rest pass between them in the scale.
groundRow :: forall m. State -> M.Line -> H.ComponentHTML Action Slots m
groundRow s l =
  let
    pcs = map (map (\n -> ((n `mod` 12) + 12) `mod` 12))
    chordsOfGround g = pcs (filter (\c -> c /= []) (parseProgression g.source))
    picked = find (\g -> chordsOfGround g == l.ground) s.grounds
    on = l.ground /= []
    count = max 1 (foldl (\n _ -> n + 1) 0 l.ground)
    now = (l.tick / (16 * max 1 l.every)) `mod` count
    -- named as voiced upward from the bass
    voiced = foldl (\acc pc -> let low = fromMaybe 47 (last acc) in acc <> [ low + 1 + ((pc - low - 1) `mod` 12 + 12) `mod` 12 ]) []
    chip i c = HH.span
      [ style $ "font-family:Georgia,serif;font-size:11px;padding:1px 5px;border-radius:4px;white-space:nowrap;"
          <> (if i == now then "background:#c8a86a;color:#1c1a12" else "color:#4a463b") ]
      [ HH.text (chordName (voiced c)) ]
    -- a progression by its name's rebus ("tractor-drum-cow": three icons);
    -- one named some other way, by the rebus of its chords
    rebusOf g = HH.span [ style "display:inline-flex;gap:5px;align-items:center" ]
      ( map faIcon
          ( if contains (Pattern "-") g.name then (Rebus.rebusFromAnyAlias g.name).icons
            else bundleRebus (parseProgression g.source) ) )
    keyOf g = HH.span [ style "font-family:Georgia,serif;font-style:italic;font-size:11px;color:#5d584b;white-space:nowrap" ] [ HH.text g.keyLabel ]
    box lit = "display:inline-flex;align-items:center;gap:8px;padding:4px 9px;border-radius:6px;cursor:pointer;border:1px solid "
      <> (if lit then "#8a7a4a;background:#e6dfc8" else "#00000026;background:#efece1")
    plain t = HH.span [ style "font-family:Georgia,serif;font-size:12px;color:#5d584b" ] [ HH.text t ]
    choice g = HH.div
      [ HE.onClick \_ -> PickGround g.name, HP.title g.name, style (box (map _.name picked == Just g.name)) ]
      [ rebusOf g, keyOf g ]
  in
    HH.div [ style "margin:0 0 14px;padding-bottom:10px;border-bottom:1px solid #00000018" ]
      [ HH.div [ style $ engrave <> ";font-size:8px;letter-spacing:0.16em;color:#5a4a1f;margin-bottom:5px" ]
          [ HH.text "GROUND · A PROGRESSION FROM VETULA · PICKING ONE SETS THE SCALE TO ITS KEY" ]
      , HH.div [ style "display:flex;align-items:center;gap:14px;flex-wrap:wrap" ]
          [ HH.div
              [ HE.onClick \_ -> ToggleGroundList
              , HP.title (maybe "pick a progression kept in Vetula, as this browser last had it" _.name picked)
              , style (box false) ]
              [ case picked of
                  Just g -> HH.span [ style "display:inline-flex;align-items:center;gap:8px" ] [ rebusOf g, keyOf g ]
                  Nothing -> plain (if on then "as saved" else "none")
              , HH.span [ style "font-size:9px;color:#6a6456" ] [ HH.text (if s.groundOpen then "▲" else "▼") ]
              ]
          , HH.div [ HP.title "bars to a chord, counted in time: every head is under the same chord" ]
              [ HH.div [ style $ engrave <> ";font-size:8px;margin-bottom:2px" ] [ HH.text "BARS A CHORD" ]
              , HH.div [ style "display:flex;gap:3px;width:130px" ]
                  (map (\b -> tabBtn (show b) (b == l.every) (SetLineEvery b)) [ 1, 2, 4, 8 ])
              ]
          , HH.div [ HP.title "how much of the line goes to the chord: the weightiest notes first, every note at 100" ]
              [ miniKnob LinePull l.pull "#4a463b" "PULL" (show l.pull) ]
          , HH.div
              [ HP.title (if s.stringsChannel == Nothing then "Continuo is hosting no strings for the chords to be held on" else "hold each chord of the ground on Continuo's strings, under the heads")
              , style "width:74px" ]
              [ tabBtn "STRINGS" (s.strings && s.stringsChannel /= Nothing) ToggleStrings ]
          ]
      -- the list to pick from, opened in place
      , if s.groundOpen then
          HH.div [ style "display:flex;flex-wrap:wrap;gap:5px;margin-top:8px" ]
            ( [ HH.div [ HE.onClick \_ -> PickGround "", style (box (not on)) ] [ plain "none" ] ]
                <> map choice s.grounds )
        else HH.text ""
      -- the chords by name, in a line, the one in force lit
      , if on then HH.div [ style "display:flex;flex-wrap:wrap;gap:2px;margin-top:8px" ] (mapWithIndex chip l.ground)
        else HH.text ""
      ]

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
