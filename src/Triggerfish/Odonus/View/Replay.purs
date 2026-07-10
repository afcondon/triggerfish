-- | REPLAY tab (#151, R2a) — a read-only overview of the whole logbook laid out
-- | on one horizontal timeline: every retained note as a small mark, the flagged
-- | "good bits" as gold lines, and a default loop window (one bar, from the live
-- | tempo) drawn around each mark. This is the surface the playback + resizable
-- | clip editing (R2b/R2c) will grow on; for now it just shows what's captured.
-- |
-- | The `modeBar` (LIVE / REPLAY) floats over the surface so switching never
-- | disturbs the live panels' full-height layout.
module Triggerfish.Odonus.View.Replay (replayPanel, modeBar) where

import Prelude

import Data.Array (concatMap, length, mapWithIndex, null)
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Triggerfish.Odonus.Grid.Types (Action(..), Mark, NoteEvent, OdonusView(..), Slots, State)
import Triggerfish.Odonus.Grid.Widgets (clampI, headColor, style, svgAttr, svgEl)

-- viewBox units — the timeline's internal coordinate space (stretched to fit).
tlW :: Number
tlW = 1000.0

tlH :: Number
tlH = 520.0

-- | Cap how many note rects we draw at the whole-recording zoom: a long session
-- | is ~100k notes, far too many DOM nodes. Decimating to an overview keeps the
-- | shape without the cost (playback/clip editing read the FULL buffer, not this).
maxDraw :: Int
maxDraw = 3000

pitchToY :: Int -> Number
pitchToY pitch = tlH * (1.0 - (toNumber (clampI 24 96 pitch) - 24.0) / 72.0)

-- | The floating LIVE / REPLAY switch, top-right of the Odonus surface.
modeBar :: forall m. State -> H.ComponentHTML Action Slots m
modeBar s =
  HH.div
    [ style $ "position:absolute;top:9px;right:11px;z-index:6;display:flex;gap:2px;padding:2px;"
        <> "border-radius:8px;background:#00000022;border:1px solid #ffffff14" ]
    [ tab s VLive "LIVE", tab s VReplay "REPLAY" ]
  where
  tab st v label =
    let on = st.view == v
    in HH.button
        [ HE.onClick \_ -> SetView v
        , style $ "padding:3px 11px;border-radius:6px;cursor:pointer;border:none;font-family:Georgia,serif;"
            <> "font-size:10px;letter-spacing:0.08em;"
            <> (if on then "background:#efece1;color:#2b2822;font-weight:600"
                      else "background:transparent;color:#e8e4d8aa") ]
        [ HH.text label ]

-- | The whole-recording timeline surface.
replayPanel :: forall m. State -> H.ComponentHTML Action Slots m
replayPanel s =
  let
    lb = s.logbook
    events = lb.live <> concatMap _.events lb.chunks
  in
    HH.div
      [ style $ "height:100%;position:relative;overflow:hidden;"
          <> "background:radial-gradient(140% 120% at 50% 40%,#15140f,#0b0a07)" ]
      ( if null events then [ emptyState ]
        else
          let
            tMax = s.nowMicros
            tMin = foldl (\a e -> min a e.fireUnixMicros) tMax events
            span = max 1.0 (tMax - tMin)
            xOf t = (t - tMin) / span * tlW
            barMicros = 4.0 * 60.0e6 / (if s.clockTempo > 1.0 then s.clockTempo else 120.0)
          in
            [ svgEl "svg"
                [ svgAttr "width" "100%", svgAttr "height" "100%"
                , svgAttr "viewBox" ("0 0 " <> show tlW <> " " <> show tlH)
                , svgAttr "preserveAspectRatio" "none"
                , style "position:absolute;inset:0" ]
                ( map (noteDot xOf) (decimate events)
                    <> map (regionBand xOf barMicros) lb.marks
                    <> map (markLine xOf) lb.marks )
            , caption (length events) (length lb.marks)
            ]
      )

-- | Keep at most `maxDraw` notes by taking every stride-th one — enough for the
-- | overview shape.
decimate :: Array NoteEvent -> Array NoteEvent
decimate es =
  let n = length es
  in if n <= maxDraw then es
     else let stride = 1 + n / maxDraw
          in map _.value (filterStride stride (mapWithIndex (\i e -> { i, value: e }) es))
  where
  filterStride k = filterA (\r -> r.i `mod` k == 0)
  filterA p xs = foldl (\acc x -> if p x then acc <> [ x ] else acc) [] xs

noteDot :: forall m. (Number -> Number) -> NoteEvent -> H.ComponentHTML Action Slots m
noteDot xOf e =
  svgEl "rect"
    [ svgAttr "x" (show (xOf e.fireUnixMicros)), svgAttr "y" (show (pitchToY e.pitch))
    , svgAttr "width" "2", svgAttr "height" "3", svgAttr "rx" "1"
    , svgAttr "fill" (headColor e.headIdx), svgAttr "opacity" "0.72"
    ] []

-- | The default one-bar loop window around a mark — a faint gold band.
regionBand :: forall m. (Number -> Number) -> Number -> Mark -> H.ComponentHTML Action Slots m
regionBand xOf barMicros m =
  let x0 = xOf (m.atMicros - barMicros / 2.0)
      x1 = xOf (m.atMicros + barMicros / 2.0)
  in
    svgEl "rect"
      [ svgAttr "x" (show x0), svgAttr "y" "0"
      , svgAttr "width" (show (max 1.0 (x1 - x0))), svgAttr "height" (show tlH)
      , svgAttr "fill" "#e8c14a", svgAttr "opacity" "0.1"
      , svgAttr "stroke" "#e8c14a", svgAttr "stroke-opacity" "0.35", svgAttr "stroke-width" "1"
      ] []

markLine :: forall m. (Number -> Number) -> Mark -> H.ComponentHTML Action Slots m
markLine xOf m =
  svgEl "rect"
    [ svgAttr "x" (show (xOf m.atMicros)), svgAttr "y" "0"
    , svgAttr "width" "1.5", svgAttr "height" (show tlH)
    , svgAttr "fill" "#e8c14a", svgAttr "opacity" "0.65"
    ] []

caption :: forall m. Int -> Int -> H.ComponentHTML Action Slots m
caption notes marks =
  HH.div
    [ style $ "position:absolute;bottom:10px;left:12px;font-family:'SF Mono',Menlo,monospace;"
        <> "font-size:9px;color:#ffffff44" ]
    [ HH.text (show notes <> " notes · " <> show marks <> " marks · whole session") ]

emptyState :: forall m. H.ComponentHTML Action Slots m
emptyState =
  HH.div
    [ style $ "position:absolute;inset:0;display:flex;align-items:center;justify-content:center;"
        <> "font-family:Georgia,serif;font-size:13px;color:#ffffff3a;text-align:center;padding:0 40px" ]
    [ HH.text "Nothing captured yet. Play in LIVE, tap ◆ mark on the good bits, then come back here." ]
