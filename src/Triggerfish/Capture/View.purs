-- | `Triggerfish.Capture.View` — the machine-agnostic capture/replay surface (#28,
-- | docs/DESIGN-capture-surface.md). One renderer, three orientations: `Horizontal`
-- | lays time along X oldest-first (Odonus), `HorizontalOutward` reverses it so the
-- | newest note enters at the left and ages rightward (Vetula, whose surface sits to
-- | the right of its voices), `Vertical` lays time down Y. Same data, same gestures,
-- | different projection — see `Capture.Types.Orientation`.
-- |
-- | Pure and POLYMORPHIC in the host's `action`: the view never imports a machine's
-- | Action type (that would be a cycle, and machine-specific). Instead the host
-- | passes a `CaptureWiring` of the constructors it wants emitted plus the two
-- | machine-specific reads (voice colour, harmonic context). So Odonus, Vetula and
-- | Balistes each wire their own State/Action to one shared surface.
-- |
-- | Generalised from `Odonus.View.Replay` (now a thin adapter over this). The old
-- | on-surface CLIPS strip is gone: capture saves straight to the shared library
-- | (`Triggerfish.Clips.Store`), and clips are played/managed in the library modal.
module Triggerfish.Capture.View
  ( CaptureState
  , CaptureWiring
  , ContextSummary
  , capturePanel
  ) where

import Prelude

import Data.Array (concat, concatMap, filter, foldl, length, mapWithIndex, null, (!!))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.String.Common (joinWith)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.MouseEvent as ME
import Triggerfish.Capture.Types (Logbook, Mark, Orientation(..), PlaySource(..), PlayState, RegionDrag, RegionEdge(..))
import Triggerfish.Clips (NoteEvent)
import Halogen.Widgets.Svg (svgAttr, svgEl)

-- | The capture sub-state every machine holds: the always-on logbook, a replay loop
-- | in flight (if any), a region drag in progress, and whether the harmonic-context
-- | panel is open on the playing region's card.
type CaptureState =
  { logbook :: Logbook
  , playing :: Maybe PlayState
  , regionDrag :: Maybe RegionDrag  -- host-owned; the view only reads logbook/playing/context
  , contextOpen :: Boolean
  }

-- | The harmonic context read off a mark's captured patch — the same shape as
-- | Odonus's `HarmonicContext`, kept structural so `harmonicSummary` drops straight
-- | in. A machine with no harmony analogue passes `const Nothing`.
type ContextSummary =
  { root :: String, scale :: String, chord :: Maybe String, notes :: Array String }

-- | What the host gives the shared view: the orientation, a unique DOM id for
-- | pointer math, the two machine-specific reads (voice colour, context), and the
-- | gesture constructors. `saveScene` is optional (Nothing → no scene button).
type CaptureWiring action =
  { orientation :: Orientation
  , timelineId :: String
  , headColor :: Int -> String
  , contextSummary :: String -> Maybe ContextSummary
  , regionDown :: Int -> RegionEdge -> Int -> Int -> action
  , stopPlay :: action
  , saveClip :: Int -> action
  , saveScene :: Maybe (Int -> action)
  , toggleContext :: action
  }

-- viewBox units — the timeline's internal coordinate space (stretched to fit). The
-- long axis is whichever one carries time; `preserveAspectRatio none` lets the
-- container decide the real proportions.
tlW :: Number
tlW = 1000.0

tlH :: Number
tlH = 520.0

-- | Cap how many note rects we draw: a long session is ~100k notes, far too many
-- | DOM nodes. Decimating to an overview keeps the shape without the cost (playback
-- | reads the FULL buffer, not this).
maxDraw :: Int
maxDraw = 3000

-- ── tiny generic helpers (duplicated from Odonus.Grid.Widgets so this stays a
-- ── leaf module, not a dependant of Odonus) ──────────────────────────────────
style :: forall r i. String -> HP.IProp r i
style = HP.attr (H.AttrName "style")

-- ── coordinate projections (the only orientation-aware code) ─────────────────

-- | Position along the PITCH axis, in viewBox units. Either horizontal orientation
-- | → Y (high at top); Vertical → X (low at left, high at right).
pitchCoord :: Orientation -> Int -> Number
pitchCoord o pitch =
  let norm = (toNumber (clamp 24 96 pitch) - 24.0) / 72.0
  in case o of
       Vertical -> tlW * norm
       _ -> tlH * (1.0 - norm)

-- | Position along the TIME axis, in viewBox units, for a 0..1 fraction (0 =
-- | earliest note, 1 = newest). Horizontal → X, earliest at left. HorizontalOutward
-- | → X flipped, NEWEST AT LEFT so the roll ages away from the voices. Vertical →
-- | Y, NEWEST AT TOP (AC's call): frac 1 maps to Y 0, ageing downward.
timeCoord :: Orientation -> Number -> Number
timeCoord o frac = case o of
  Horizontal -> frac * tlW
  HorizontalOutward -> (1.0 - frac) * tlW
  Vertical -> (1.0 - frac) * tlH

-- | Position along the TIME axis as a PERCENT (0..100) for HTML overlays — same
-- | convention as `timeCoord` (Horizontal → % from left, oldest first;
-- | HorizontalOutward → % from left, NEWEST first; Vertical → % from top, newest at
-- | top). Region bands/handles/playhead all place through this.
axisPos :: Orientation -> Number -> Number
axisPos o frac = case o of
  Horizontal -> frac * 100.0
  HorizontalOutward -> (1.0 - frac) * 100.0
  Vertical -> (1.0 - frac) * 100.0

-- ── the surface ──────────────────────────────────────────────────────────────

-- | The whole-recording capture surface: every retained note as a small mark, the
-- | flagged good bits as gold lines, a draggable loop window around each, and the
-- | control card on the playing region.
capturePanel :: forall action slots m. CaptureWiring action -> CaptureState -> H.ComponentHTML action slots m
capturePanel w cap =
  let
    lb = cap.logbook
    events = lb.live <> concatMap _.events lb.chunks
  in
    HH.div
      [ HP.id w.timelineId
      , style $ "height:100%;position:relative;overflow:hidden;"
          <> "background:radial-gradient(140% 120% at 50% 40%,#15140f,#0b0a07)" ]
      ( if null events then [ emptyState ]
        else
          let
            tMin = foldl (\a e -> min a e.fireUnixMicros) 1.0e18 events
            tMax = foldl (\a e -> max a e.fireUnixMicros) 0.0 events
            span = max 1.0 (tMax - tMin)
            fracOf t = (t - tMin) / span              -- 0..1 along the time axis
            posOf t = axisPos w.orientation (fracOf t)  -- percent from the axis origin
          in
            [ svgEl "svg"
                [ svgAttr "width" "100%", svgAttr "height" "100%"
                , svgAttr "viewBox" ("0 0 " <> show tlW <> " " <> show tlH)
                , svgAttr "preserveAspectRatio" "none"
                , style "position:absolute;inset:0" ]
                (map (noteDot w fracOf) (decimate events) <> map (markLine w fracOf) lb.marks)
            ]
              <> concat (mapWithIndex (regionBand w posOf cap.playing) lb.marks)
              <> playhead w posOf cap.playing
              <> [ caption (length events) (length lb.marks) ]
              <> controlCard w posOf cap
      )

-- | Keep at most `maxDraw` notes by taking every stride-th one.
decimate :: Array NoteEvent -> Array NoteEvent
decimate es =
  let n = length es
  in if n <= maxDraw then es
     else let stride = 1 + n / maxDraw
          in map _.value (filterStride stride (mapWithIndex (\i e -> { i, value: e }) es))
  where
  filterStride k = filter (\r -> r.i `mod` k == 0)

noteDot :: forall action slots m. CaptureWiring action -> (Number -> Number) -> NoteEvent -> H.ComponentHTML action slots m
noteDot w fracOf e =
  let t = timeCoord w.orientation (fracOf e.fireUnixMicros)
      p = pitchCoord w.orientation e.pitch
      -- (x,y,width,height): time on X for either horizontal, on Y for Vertical.
      coords = case w.orientation of
        Vertical -> { x: p, y: t, ww: "3", hh: "2" }
        _ -> { x: t, y: p, ww: "2", hh: "3" }
  in svgEl "rect"
    [ svgAttr "x" (show coords.x), svgAttr "y" (show coords.y)
    , svgAttr "width" coords.ww, svgAttr "height" coords.hh, svgAttr "rx" "1"
    , svgAttr "fill" (w.headColor e.headIdx), svgAttr "opacity" "0.72"
    ] []

-- | A mark: a gold line ACROSS the pitch axis at the mark's time.
markLine :: forall action slots m. CaptureWiring action -> (Number -> Number) -> Mark -> H.ComponentHTML action slots m
markLine w fracOf m =
  let t = timeCoord w.orientation (fracOf m.atMicros)
      coords = case w.orientation of
        Vertical -> { x: "0", y: show t, ww: show tlW, hh: "1.5" }
        _ -> { x: show t, y: "0", ww: "1.5", hh: show tlH }
  in svgEl "rect"
    [ svgAttr "x" coords.x, svgAttr "y" coords.y
    , svgAttr "width" coords.ww, svgAttr "height" coords.hh
    , svgAttr "fill" "#e8c14a", svgAttr "opacity" "0.65"
    ] []

-- | A loop region — the gold band [from,to] as HTML overlays. Body (grab to slide,
-- | click to play) and two edge handles (grab to resize) are SIBLINGS so an edge
-- | grab doesn't also fire the body's mousedown. Brighter while it's the one playing.
-- | Either horizontal → a vertical strip spanning the height; Vertical → a horizontal strip
-- | spanning the width.
regionBand :: forall action slots m. CaptureWiring action -> (Number -> Number) -> Maybe PlayState -> Int -> Mark -> Array (H.ComponentHTML action slots m)
regionBand w posOf playing i m =
  let pf = posOf m.from
      pt = posOf m.to
      -- the band spans between the two endpoints; which is smaller flips with the
      -- axis direction (Vertical and HorizontalOutward both run newest-first), so
      -- take min/abs and the band placement is generic.
      start = min pf pt
      len = max 0.3 (abs (pt - pf))
      active = case playing of
        Just p -> p.source == FromRegion i
        Nothing -> false
      bandStyle = case w.orientation of
        Vertical -> "left:0;right:0;top:" <> show start <> "%;height:" <> show len <> "%;cursor:grab;"
          <> "border-top:1px solid #e8c14a66;border-bottom:1px solid #e8c14a66;"
        _ -> "top:0;bottom:0;left:" <> show start <> "%;width:" <> show len <> "%;cursor:grab;"
          <> "border-left:1px solid #e8c14a66;border-right:1px solid #e8c14a66;"
  in
    [ HH.div
        [ HE.onMouseDown \me -> w.regionDown i EdgeBody (ME.clientX me) (ME.clientY me)
        , style $ "position:absolute;" <> bandStyle
            <> "background:rgba(232,193,74," <> (if active then "0.22" else "0.10") <> ")"
            <> (if active then ";box-shadow:inset 0 0 0 1px #e8c14a" else "") ]
        []
    , edgeHandle w i EdgeFrom pf
    , edgeHandle w i EdgeTo pt
    ]

-- | Absolute value on `Number` (Data.Ord.abs is fine but avoids an extra import).
abs :: Number -> Number
abs n = if n < 0.0 then -n else n

-- | A thin resize grip at a region edge (percent along the time axis), on top of the
-- | body. `ew-resize` for either horizontal, `ns-resize` for Vertical.
edgeHandle :: forall action slots m. CaptureWiring action -> Int -> RegionEdge -> Number -> H.ComponentHTML action slots m
edgeHandle w i edge pct =
  let gripStyle = case w.orientation of
        Vertical -> "left:0;right:0;top:calc(" <> show pct <> "% - 4px);height:8px;cursor:ns-resize;"
        _ -> "top:0;bottom:0;left:calc(" <> show pct <> "% - 4px);width:8px;cursor:ew-resize;"
  in HH.div
    [ HE.onMouseDown \me -> w.regionDown i edge (ME.clientX me) (ME.clientY me)
    , style $ "position:absolute;" <> gripStyle <> "background:rgba(232,193,74,0.4)" ]
    []

-- | The moving loop playhead while replaying a REGION (a saved clip isn't on the
-- | timeline). A line perpendicular to the time axis, moving along it.
playhead :: forall action slots m. CaptureWiring action -> (Number -> Number) -> Maybe PlayState -> Array (H.ComponentHTML action slots m)
playhead w posOf = case _ of
  Just p | FromRegion _ <- p.source ->
    let pos = posOf (p.fromMicros + p.playheadFrac * (p.toMicros - p.fromMicros))
        headStyle = case w.orientation of
          Vertical -> "left:0;right:0;top:" <> show pos <> "%;height:2px;"
          _ -> "top:0;bottom:0;left:" <> show pos <> "%;width:2px;"
    in [ HH.div
           [ style $ "position:absolute;" <> headStyle <> "background:#ffffff;opacity:0.85;pointer-events:none" ]
           [] ]
  _ -> []

-- | The control card on the playing region — stop, lift-to-clip, (optional) save
-- | scene, and the harmonic context to jam over. When nothing plays it's just the
-- | hint. Anchors near the region's start along the time axis.
controlCard :: forall action slots m. CaptureWiring action -> (Number -> Number) -> CaptureState -> Array (H.ComponentHTML action slots m)
controlCard w posOf cap = case cap.playing of
  Nothing ->
    [ HH.div
        [ style "position:absolute;bottom:9px;right:12px;font-family:Georgia,serif;font-size:10px;color:#ffffff44" ]
        [ HH.text "click a gold band to loop it" ]
    ]
  Just p -> case p.source of
    FromClip _ -> []
    FromRegion i -> case cap.logbook.marks !! i of
      Nothing -> []
      Just m ->
        let
          pf = posOf m.from
          pt = posOf m.to
          start = min pf pt   -- top/left edge of the band, whichever way the axis runs
          -- anchor near the band's start, flipping past the midpoint so it never runs off.
          anchor = case w.orientation of
            Vertical -> if start < 70.0 then "left:8px;top:calc(" <> show start <> "% + 4px)" else "left:8px;bottom:calc(" <> show (100.0 - max pf pt) <> "% + 4px)"
            _ -> if start < 55.0 then "top:6px;left:" <> show start <> "%" else "top:6px;right:" <> show (100.0 - max pf pt) <> "%"
        in
          [ HH.div
              [ style $ "position:absolute;" <> anchor <> ";z-index:7;min-width:150px;"
                  <> "border-radius:9px;padding:7px 8px;background:#151310ee;border:1px solid #e8c14a55;"
                  <> "box-shadow:0 4px 14px #00000066" ]
              ( [ HH.div [ style $ "font-family:'SF Mono',Menlo,monospace;font-size:8px;letter-spacing:0.1em;"
                      <> "color:#e8c14a;margin-bottom:6px" ]
                    [ HH.text ("LOOPING · MARK " <> show (i + 1)) ]
                , HH.div [ style "display:flex;gap:4px;flex-wrap:wrap" ]
                    ( [ cardBtn w.stopPlay "#e8c14a" "stop the loop" "■ stop"
                      , cardBtn (w.saveClip i) "#cdb98a" "lift this loop into the shared clip library" "⧉ clip"
                      ]
                      <> (case w.saveScene of
                            Just mk -> [ cardBtn (mk i) "#cdb98a" "save this good bit into the SCENES list" "⛭ scene" ]
                            Nothing -> [])
                      <> [ cardBtn w.toggleContext (if cap.contextOpen then "#e8c14a" else "#cdb98a")
                             "show the key & chord to jam over" "♫ context" ]
                    )
                ]
                <> (if cap.contextOpen then [ contextPanel w m ] else [])
              )
          ]

cardBtn :: forall action slots m. action -> String -> String -> String -> H.ComponentHTML action slots m
cardBtn act fg titleTxt label =
  HH.button
    [ HE.onClick \_ -> act
    , HP.title titleTxt
    , style $ "padding:3px 8px;border-radius:6px;cursor:pointer;border:1px solid #ffffff1a;"
        <> "background:#ffffff10;font-family:Georgia,serif;font-size:10px;color:" <> fg ]
    [ HH.text label ]

-- | The harmonic context of the looped mark, via the host's `contextSummary` read.
contextPanel :: forall action slots m. CaptureWiring action -> Mark -> H.ComponentHTML action slots m
contextPanel w m =
  HH.div [ style "margin-top:7px;padding-top:6px;border-top:1px solid #ffffff14" ]
    ( case w.contextSummary m.patch of
        Nothing -> [ HH.div [ ctxStyle "#ffffff44" ] [ HH.text "harmony unavailable" ] ]
        Just h ->
          [ HH.div [ style "font-family:Georgia,serif;font-size:13px;color:#f0ead8;margin-bottom:3px" ]
              [ HH.text (h.root <> " " <> h.scale) ]
          ]
            <> (case h.chord of
                  Just c -> [ HH.div [ ctxStyle "#e8c14a" ] [ HH.text ("chord · " <> c) ] ]
                  Nothing -> [ HH.div [ ctxStyle "#8a8578" ] [ HH.text "no chord colour" ] ])
            <> [ HH.div [ ctxStyle "#b7b09c" ] [ HH.text (joinWith " " h.notes) ] ]
    )
  where
  ctxStyle col = style $ "font-family:'SF Mono',Menlo,monospace;font-size:10px;margin-top:2px;color:" <> col

caption :: forall action slots m. Int -> Int -> H.ComponentHTML action slots m
caption notes marks =
  HH.div
    [ style $ "position:absolute;bottom:10px;left:12px;font-family:'SF Mono',Menlo,monospace;"
        <> "font-size:9px;color:#ffffff44" ]
    [ HH.text (show notes <> " notes · " <> show marks <> " marks · whole session") ]

emptyState :: forall action slots m. H.ComponentHTML action slots m
emptyState =
  HH.div
    [ style $ "position:absolute;inset:0;display:flex;align-items:center;justify-content:center;"
        <> "font-family:Georgia,serif;font-size:13px;color:#ffffff3a;text-align:center;padding:0 40px" ]
    [ HH.text "Nothing captured yet. Play, tap ◆ mark on the good bits, then come back here." ]
