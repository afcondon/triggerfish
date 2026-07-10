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
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Triggerfish.Odonus.Grid.Types (Action(..), Mark, NoteEvent, OdonusView(..), PlayState, Slots, State)
import Triggerfish.Odonus.Grid.Widgets (clampI, headColor, style, svgAttr, svgEl)
import Triggerfish.Odonus.Logbook (regionBounds)

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
            -- The timeline spans the actual recording — earliest to LATEST
            -- captured note — NOT the live `now`, so it stops stretching the
            -- moment capture stops (while capturing, the last note ≈ now anyway).
            tMin = foldl (\a e -> min a e.fireUnixMicros) 1.0e18 events
            tMax = foldl (\a e -> max a e.fireUnixMicros) 0.0 events
            span = max 1.0 (tMax - tMin)
            xOf t = (t - tMin) / span * tlW      -- svg viewBox units (0..tlW)
            pctOf t = (t - tMin) / span * 100.0  -- percent, for HTML overlays
          in
            [ svgEl "svg"
                [ svgAttr "width" "100%", svgAttr "height" "100%"
                , svgAttr "viewBox" ("0 0 " <> show tlW <> " " <> show tlH)
                , svgAttr "preserveAspectRatio" "none"
                , style "position:absolute;inset:0" ]
                (map (noteDot xOf) (decimate events) <> map (markLine xOf) lb.marks)
            ]
              <> mapWithIndex (regionBand pctOf s.clockTempo s.playing) lb.marks
              <> playhead pctOf s.playing
              <> [ caption (length events) (length lb.marks), transport s.playing ]
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

-- | The bar-aligned loop window around a mark — a clickable gold band (an HTML
-- | overlay, percent-positioned over the svg) that starts the region looping.
-- | Uses the same regionBounds as playback, so what you see is what loops.
-- | Brighter while it's the one playing.
regionBand :: forall m. (Number -> Number) -> Number -> Maybe PlayState -> Int -> Mark -> H.ComponentHTML Action Slots m
regionBand pctOf tempo playing i m =
  let rb = regionBounds tempo m
      l = pctOf rb.from
      r = pctOf rb.to
      active = case playing of
        Just p -> p.markIdx == i
        Nothing -> false
  in
    HH.div
      [ HE.onClick \_ -> PlayRegion i
      , style $ "position:absolute;top:0;bottom:0;left:" <> show l <> "%;width:" <> show (max 0.3 (r - l)) <> "%;"
          <> "cursor:pointer;border-left:1px solid #e8c14a66;border-right:1px solid #e8c14a66;"
          <> "background:rgba(232,193,74," <> (if active then "0.22" else "0.10") <> ")"
          <> (if active then ";box-shadow:inset 0 0 0 1px #e8c14a" else "") ]
      []

-- | The moving loop playhead, shown while replaying.
playhead :: forall m. (Number -> Number) -> Maybe PlayState -> Array (H.ComponentHTML Action Slots m)
playhead pctOf = case _ of
  Nothing -> []
  Just p ->
    let x = pctOf (p.fromMicros + p.playheadFrac * (p.toMicros - p.fromMicros))
    in [ HH.div
           [ style $ "position:absolute;top:0;bottom:0;left:" <> show x <> "%;width:2px;"
               <> "background:#ffffff;opacity:0.85;pointer-events:none" ]
           [] ]

-- | The replay transport: a STOP control shown while a loop runs.
transport :: forall m. Maybe PlayState -> H.ComponentHTML Action Slots m
transport = case _ of
  Nothing ->
    HH.div
      [ style $ "position:absolute;bottom:9px;right:12px;font-family:Georgia,serif;font-size:10px;color:#ffffff44" ]
      [ HH.text "click a gold band to loop it" ]
  Just p ->
    HH.button
      [ HE.onClick \_ -> StopPlay
      , style $ "position:absolute;bottom:9px;right:12px;padding:4px 12px;border-radius:7px;cursor:pointer;"
          <> "border:1px solid #e8c14a66;background:#e8c14a1f;color:#e8c14a;font-family:Georgia,serif;font-size:11px" ]
      [ HH.text ("■ stop · looping mark " <> show (p.markIdx + 1)) ]

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
