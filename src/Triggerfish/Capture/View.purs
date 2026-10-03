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
  , markCode
  , bounds
  , pointerFrac
  ) where

import Prelude

import Data.Array (concat, concatMap, filter, findIndex, foldl, length, mapMaybe, mapWithIndex, null, (!!))
import Data.Array as Array
import Data.Int (toNumber)
import Data.Int as Int
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.String.Common (joinWith)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.MouseEvent as ME
import Triggerfish.Capture.RigLoops (Clock)
import Triggerfish.Capture.Runs (Axis)
import Triggerfish.Capture.Runs as Runs
import Triggerfish.Capture.RigLoops as RL
import Triggerfish.Capture.Types (Logbook, Mark, Orientation(..), PlaySource(..), PlayState, RegionDrag, RegionEdge(..), Zoom(..))
import Triggerfish.Clips (NoteEvent)
import Halogen.Widgets.Svg (svgAttr, svgEl)
import Triggerfish.Ui.Style (style)

-- | The capture sub-state every machine holds: the always-on logbook, a replay loop
-- | in flight (if any), a region drag in progress, and whether the harmonic-context
-- | panel is open on the playing region's card.
type CaptureState =
  { logbook :: Logbook
  , playing :: Maybe PlayState
  , regionDrag :: Maybe RegionDrag  -- host-owned; the view only reads logbook/playing/context
  , contextOpen :: Boolean
  -- the looped mark shown as code (`markCode`), with → Limulus
  , codeOpen :: Boolean
  , zoom :: Zoom
  -- the rig keeps the marks and plays the loops (Capture.RigLoops): the
  -- clock to place its loops' playheads by; Nothing when the page does
  , rig :: Maybe Clock
  -- ✂ armed: a drag across the surface selects a stretch to cut; and the
  -- stretch being dragged (surface µs)
  , cutting :: Boolean
  , cutSel :: Maybe { from :: Number, to :: Number }
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
  , contextSummary :: Mark -> Maybe ContextSummary
  , regionDown :: Int -> RegionEdge -> Int -> Int -> action
  , stopPlay :: action
  , saveClip :: Int -> action
  , saveScene :: Maybe (Int -> action)
  , toggleContext :: action
  , setZoom :: Zoom -> action
  -- the mark as code: this machine's slot (`odonus`), the toggle, and
  -- handing mark i's code to Limulus
  , machine :: String
  , ownCode :: Mark -> String
  , toggleCode :: action
  , toLimulus :: Int -> action
  -- editing the record buffer, with a rig: trim to the marks, undo the last
  -- edit, and (a host that can drag) ✂ arm and the drag's start
  , edits :: Maybe { trim :: action, undo :: action, cut :: Maybe { arm :: action, down :: Int -> Int -> action } }
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
            b = bounds cap.zoom lb
            fracOf = b.toFrac                           -- 0..1 along the time axis
            posOf t = axisPos w.orientation (fracOf t)  -- percent from the axis origin
            -- the notes in view, thinned only after cropping, so a zoom shows them all
            shown = filter (\e -> let f = fracOf e.fireUnixMicros in f >= 0.0 && f <= 1.0) events
          in
            [ svgEl "svg"
                [ svgAttr "width" "100%", svgAttr "height" "100%"
                , svgAttr "viewBox" ("0 0 " <> show tlW <> " " <> show tlH)
                , svgAttr "preserveAspectRatio" "none"
                , style "position:absolute;inset:0" ]
                (map (seamLine w) (filter (\f -> f > 0.0 && f < 1.0) b.seams)
                  <> map (noteDot w fracOf) (decimate shown) <> map (markLine w fracOf) lb.marks)
            ]
              <> concat (mapWithIndex (regionBand w posOf (activeAt cap)) lb.marks)
              <> playheads w posOf cap
              <> cutBand w posOf cap
              <> [ caption (length shown) (length lb.marks) cap.zoom b.span, zoomBar w cap ]
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

-- | Where the transport stopped and started again: a faint dashed line, the
-- | pause between sliced out (Capture.Runs).
seamLine :: forall action slots m. CaptureWiring action -> Number -> H.ComponentHTML action slots m
seamLine w f =
  let t = timeCoord w.orientation f
      coords = case w.orientation of
        Vertical -> { x1: "0", y1: show t, x2: show tlW, y2: show t }
        _ -> { x1: show t, y1: "0", x2: show t, y2: show tlH }
  in svgEl "line"
    [ svgAttr "x1" coords.x1, svgAttr "y1" coords.y1, svgAttr "x2" coords.x2, svgAttr "y2" coords.y2
    , svgAttr "stroke" "#ffffff", svgAttr "stroke-opacity" "0.22", svgAttr "stroke-width" "1"
    , svgAttr "stroke-dasharray" "4 6", svgAttr "vector-effect" "non-scaling-stroke"
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
regionBand :: forall action slots m. CaptureWiring action -> (Number -> Number) -> (Int -> Mark -> Boolean) -> Int -> Mark -> Array (H.ComponentHTML action slots m)
regionBand w posOf isActive i m =
  let pf = posOf m.from
      pt = posOf m.to
      -- the band spans between the two endpoints; which is smaller flips with the
      -- axis direction (Vertical and HorizontalOutward both run newest-first), so
      -- take min/abs and the band placement is generic.
      start = min pf pt
      len = max 0.3 (abs (pt - pf))
      active = isActive i m
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
        [ HH.span
            [ style $ "position:absolute;top:3px;left:4px;pointer-events:none;"
                <> "font-family:'SF Mono',Menlo,monospace;font-size:9px;"
                <> "color:" <> (if active then "#e8c14a" else "#e8c14a88") ]
            [ HH.text (show m.n) ] ]
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

-- | Whether mark `i` is looping: the page's own loop, or with a rig, one of
-- | the rig's.
activeAt :: CaptureState -> Int -> Mark -> Boolean
activeAt cap i m = case cap.rig of
  Just _ -> RL.looping m
  Nothing -> case cap.playing of
    Just p -> p.source == FromRegion i
    Nothing -> false

-- | The moving loop playheads: the page's loop of a REGION (a saved clip isn't
-- | on the timeline), or each of the rig's loops. A line perpendicular to the
-- | time axis, moving along it.
playheads :: forall action slots m. CaptureWiring action -> (Number -> Number) -> CaptureState -> Array (H.ComponentHTML action slots m)
playheads w posOf cap = case cap.rig of
  Just c -> mapMaybe (\m -> RL.playheadFrac c m <#> \f -> line (m.from + f * (m.to - m.from))) cap.logbook.marks
  Nothing -> case cap.playing of
    Just p | FromRegion _ <- p.source -> [ line (p.fromMicros + p.playheadFrac * (p.toMicros - p.fromMicros)) ]
    _ -> []
  where
  line t =
    let pos = posOf t
        headStyle = case w.orientation of
          Vertical -> "left:0;right:0;top:" <> show pos <> "%;height:2px;"
          _ -> "top:0;bottom:0;left:" <> show pos <> "%;width:2px;"
    in HH.div
         [ style $ "position:absolute;" <> headStyle <> "background:#ffffff;opacity:0.85;pointer-events:none" ]
         []

-- | The mark the control card is on: the page's looping region, or the rig's
-- | loop started last.
cardMark :: CaptureState -> Maybe Int
cardMark cap = case cap.rig of
  Just _ -> RL.focus cap.logbook.marks >>= \f -> findIndex (\m -> m.n == f.n) cap.logbook.marks
  Nothing -> case cap.playing of
    Just { source: FromRegion i } -> Just i
    _ -> Nothing

-- | The control card on the playing region — stop, lift-to-clip, (optional) save
-- | scene, and the harmonic context to jam over. When nothing plays it's just the
-- | hint. Anchors near the region's start along the time axis.
controlCard :: forall action slots m. CaptureWiring action -> (Number -> Number) -> CaptureState -> Array (H.ComponentHTML action slots m)
controlCard w posOf cap = case cardMark cap of
  Nothing ->
    [ HH.div
        [ style "position:absolute;bottom:9px;right:12px;font-family:Georgia,serif;font-size:10px;color:#ffffff44" ]
        [ HH.text (case cap.rig of
                     Just _ -> "click a gold band to loop it on the rig; click again to stop it"
                     Nothing -> "click a gold band to loop it") ]
    ]
  Just i -> case cap.logbook.marks !! i of
      Nothing -> []
      Just m ->
        let
          pf = posOf m.from
          pt = posOf m.to
          start = min pf pt   -- top/left edge of the band, whichever way the axis runs
          -- anchor near the band's start, flipping past the midpoint so it never runs off.
          anchor = case w.orientation of
            Vertical -> if start < 70.0 then "left:8px;top:calc(" <> show start <> "% + 4px)" else "left:8px;bottom:calc(" <> show (100.0 - max pf pt) <> "% + 4px)"
            -- below the zoom bar's row, which would otherwise lie over it
            _ -> if start < 55.0 then "top:32px;left:" <> show start <> "%" else "top:32px;right:" <> show (100.0 - max pf pt) <> "%"
        in
          [ HH.div
              [ style $ "position:absolute;" <> anchor <> ";z-index:7;min-width:150px;"
                  <> "border-radius:9px;padding:7px 8px;background:#151310ee;border:1px solid #e8c14a55;"
                  <> "box-shadow:0 4px 14px #00000066" ]
              ( [ HH.div [ style $ "font-family:'SF Mono',Menlo,monospace;font-size:8px;letter-spacing:0.1em;"
                      <> "color:#e8c14a;margin-bottom:6px" ]
                    [ HH.text ("LOOPING · MARK " <> show m.n <> (case cap.rig of
                                                                 Just _ -> " · ON THE RIG"
                                                                 Nothing -> "")) ]
                , HH.div [ style "display:flex;gap:4px;flex-wrap:wrap" ]
                    ( [ cardBtn w.stopPlay "#e8c14a" "stop the loop" "■ stop"
                      , cardBtn (w.saveClip i) "#cdb98a" "lift this loop into the shared clip library" "⧉ clip"
                      , cardBtn (w.setZoom (cropTo m)) "#cdb98a" "zoom the surface to this loop" "⌕ zoom"
                      ]
                      <> (case w.saveScene of
                            Just mk -> [ cardBtn (mk i) "#cdb98a" "save this good bit into the SCENES list" "⛭ scene" ]
                            Nothing -> [])
                      <> [ cardBtn w.toggleContext (if cap.contextOpen then "#e8c14a" else "#cdb98a")
                             "show the key & chord to jam over" "♫ context"
                         , cardBtn w.toggleCode (if cap.codeOpen then "#e8c14a" else "#cdb98a")
                             "this mark as code: every open machine as it was at the mark" "{ } code"
                         ]
                    )
                ]
                <> (if cap.contextOpen then [ contextPanel w m ] else [])
                <> (if cap.codeOpen then [ codePanel w i m ] else [])
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

-- | A mark as code: the marking machine first, then each other machine that
-- | was open, each under a `-- <machine>` line, as Limulus will show it.
-- | Vetula's cards are Limulus lines already; Odonus's patch is shown as
-- | Lepidoptera text, which Tidal++ cannot evaluate yet
-- | (docs/kb/plans/the-deck.md, step 2).
markCode :: String -> (Mark -> String) -> Mark -> String
markCode machine own m =
  joinWith "\n\n"
    ([ "-- mark · " <> show (Int.round m.tempo) <> " bpm" ] <> map section ([ { machine, text: own m } ] <> m.rig))
  where
  -- a comment on its own: Limulus sends a block led by `--` to Tidal
  section r = "-- " <> r.machine <> "\n\n" <> r.text

codePanel :: forall action slots m. CaptureWiring action -> Int -> Mark -> H.ComponentHTML action slots m
codePanel w i m =
  HH.div [ style "margin-top:7px;padding-top:6px;border-top:1px solid #ffffff14;max-width:520px" ]
    [ HH.pre
        [ style $ "margin:0 0 6px;max-height:220px;overflow:auto;white-space:pre-wrap;"
            <> "font-family:'SF Mono',Menlo,monospace;font-size:10px;line-height:1.45;color:#e9e3d0" ]
        [ HH.text (markCode w.machine w.ownCode m) ]
    , HH.div [ style "display:flex;gap:6px;align-items:center" ]
        [ cardBtn (w.toLimulus i) "#e8c14a" "add this to the end of Limulus's buffer" "→ Limulus"
        , HH.span [ style "font-family:Georgia,serif;font-size:9px;color:#ffffff55" ]
            [ HH.text (show (1 + length m.rig) <> " machine" <> (if null m.rig then "" else "s")) ]
        ]
    ]

-- | The harmonic context of the looped mark, via the host's `contextSummary` read.
contextPanel :: forall action slots m. CaptureWiring action -> Mark -> H.ComponentHTML action slots m
contextPanel w m =
  HH.div [ style "margin-top:7px;padding-top:6px;border-top:1px solid #ffffff14" ]
    ( case w.contextSummary m of
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

caption :: forall action slots m. Int -> Int -> Zoom -> Number -> H.ComponentHTML action slots m
caption notes marks zoom span =
  HH.div
    [ style $ "position:absolute;bottom:10px;left:12px;font-family:'SF Mono',Menlo,monospace;"
        <> "font-size:9px;color:#ffffff44" ]
    [ HH.text (show notes <> " notes · " <> show marks <> " marks · " <> what) ]
  where
  what = case zoom of
    Whole -> "whole session, " <> duration span <> " across"
    Last d -> "the last " <> duration d
    Window _ -> duration span <> " cropped"

-- | A pointer's place along the time axis, 0..1 as `bounds` reads it, from
-- | its position in the timeline element (x right, y down): mirrored where
-- | the axis runs newest-first.
pointerFrac :: Orientation -> { x :: Number, y :: Number } -> Number
pointerFrac o p = case o of
  Horizontal -> p.x
  HorizontalOutward -> 1.0 - p.x
  Vertical -> 1.0 - p.y

-- | The surface's time axis: what it shows, over played time (the pauses
-- | between runs sliced out, Capture.Runs). Hosts that turn a pointer into a
-- | time use this too (`fromFrac`), so a drag lands where it is drawn at any
-- | zoom.
bounds :: Zoom -> Logbook -> Axis
bounds zoom lb =
  let
    played = (Runs.axis lb.runs lb.cuts { lo: 0.0, span: 1.0 }).toFrac
    events = lb.live <> concatMap _.events lb.chunks
    first = played (foldl (\a e -> min a e.fireUnixMicros) 1.0e18 events)
    newest = played (foldl (\a e -> max a e.fireUnixMicros) 0.0 events)
  in Runs.axis lb.runs lb.cuts case zoom of
    Whole -> { lo: first, span: steppedSpan (newest - first) }
    Last d -> { lo: newest - d, span: d }
    Window w -> { lo: played w.from, span: max 1.0 (played w.to - played w.from) }

-- | The whole-session span, rounded up to the next of a few lengths, so the
-- | surface rescales now and then as a take grows, not with every note.
steppedSpan :: Number -> Number
steppedSpan raw =
  let steps = map (_ * 1.0e6) [ 10.0, 30.0, 60.0, 120.0, 300.0, 600.0, 900.0, 1800.0, 3600.0, 7200.0 ]
  in case Array.find (_ >= raw) steps of
       Just s -> s
       Nothing -> max 1.0 raw

-- | A crop to a mark's loop, with half its length either side.
cropTo :: Mark -> Zoom
cropTo m =
  let len = max 1.0e6 (abs (m.to - m.from))
      lo = min m.from m.to
      hi = max m.from m.to
  in Window { from: lo - len * 0.5, to: hi + len * 0.5 }

duration :: Number -> String
duration us =
  let secs = Int.round (us / 1.0e6)
  in if secs < 60 then show secs <> " s"
     else show (secs / 60) <> " min" <> (if secs `mod` 60 == 0 then "" else " " <> show (secs `mod` 60) <> " s")

-- | While ✂ is armed, a sheet over the surface takes the drag (so a band
-- | is not grabbed instead), and the stretch selected shows red.
cutBand :: forall action slots m. CaptureWiring action -> (Number -> Number) -> CaptureState -> Array (H.ComponentHTML action slots m)
cutBand w posOf cap = case w.edits >>= _.cut of
  Just c | cap.cutting ->
    [ HH.div
        [ HE.onMouseDown \me -> c.down (ME.clientX me) (ME.clientY me)
        , style "position:absolute;inset:0;z-index:8;cursor:crosshair;background:rgba(200,60,40,0.04)" ]
        ( case cap.cutSel of
            Just sel ->
              let a = posOf sel.from
                  b = posOf sel.to
                  lo = min a b
                  len = abs (b - a)
              in [ HH.div
                     [ style $ "position:absolute;pointer-events:none;background:rgba(200,60,40,0.25);"
                         <> "border-left:1px solid #d0503a;border-right:1px solid #d0503a;"
                         <> case w.orientation of
                              Vertical -> "left:0;right:0;top:" <> show lo <> "%;height:" <> show len <> "%"
                              _ -> "top:0;bottom:0;left:" <> show lo <> "%;width:" <> show len <> "%" ]
                     [] ]
            Nothing ->
              [ HH.div
                  [ style "position:absolute;bottom:9px;right:12px;font-family:Georgia,serif;font-size:10px;color:#e0907a" ]
                  [ HH.text "drag across what to cut; it stops at a mark's window" ] ] )
    ]
  _ -> []

-- | Whole · 5 min · 1 min · 20 s, top right; the current one lit. With a rig,
-- | the edits after them: ✂ cut, trim, undo.
zoomBar :: forall action slots m. CaptureWiring action -> CaptureState -> H.ComponentHTML action slots m
zoomBar w cap =
  HH.div [ style "position:absolute;top:8px;right:10px;display:flex;gap:3px;z-index:9" ]
    (map btn [ Tuple "whole" Whole, Tuple "5 min" (Last 300.0e6), Tuple "1 min" (Last 60.0e6), Tuple "20 s" (Last 20.0e6) ]
      <> (case zoom of
           Window _ -> [ btn (Tuple "cropped" zoom) ]
           _ -> [])
      <> case w.edits of
           Just e ->
             [ HH.span [ style "width:8px" ] [] ]
               <> (case e.cut of
                     Just c -> [ editBtn c.arm cap.cutting "drag across the surface to cut that stretch out of the record buffer" "✂ cut" ]
                     Nothing -> [])
               <> [ editBtn e.trim false "cut everything outside the marks' windows, a bar either side" "trim"
                  , editBtn e.undo false "put back the last cut or trim" "undo" ]
           Nothing -> [])
  where
  zoom = cap.zoom
  editBtn act lit t label =
    HH.button
      [ HE.onClick \_ -> act
      , HP.title t
      , style $ "padding:2px 7px;border-radius:5px;cursor:pointer;border:1px solid #ffffff1a;"
          <> "font-family:'SF Mono',Menlo,monospace;font-size:9px;"
          <> (if lit then "background:#d0503a44;color:#f0a090" else "background:#ffffff0a;color:#ffffff66") ]
      [ HH.text label ]
  btn (Tuple label z) =
    HH.button
      [ HE.onClick \_ -> w.setZoom z
      , HP.title (if z == Whole then "fit the whole session" else "show " <> label)
      , style $ "padding:2px 7px;border-radius:5px;cursor:pointer;border:1px solid #ffffff1a;"
          <> "font-family:'SF Mono',Menlo,monospace;font-size:9px;"
          <> (if z == zoom then "background:#e8c14a33;color:#e8c14a" else "background:#ffffff0a;color:#ffffff66") ]
      [ HH.text label ]

emptyState :: forall action slots m. H.ComponentHTML action slots m
emptyState =
  HH.div
    [ style $ "position:absolute;inset:0;display:flex;align-items:center;justify-content:center;"
        <> "font-family:Georgia,serif;font-size:13px;color:#ffffff3a;text-align:center;padding:0 40px" ]
    [ HH.text "Nothing captured yet. Play, tap ◆ mark on the good bits, then come back here." ]
