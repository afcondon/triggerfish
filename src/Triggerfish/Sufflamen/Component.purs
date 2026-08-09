-- | Triggerfish.Sufflamen — the SuperDirt instrument, dressed as a film/tape
-- | CUTTING ROOM (Steenbeck flatbed, splicing block, china marker). Where
-- | Odonus wears the test-gear panel, Sufflamen wears the editing bench.
-- |
-- | This is the D1 *visualizer heart*, standing on its own ahead of the audio
-- | gates (C = real SuperDirt OSC in purerl-tidal, B = the routing layer). By
-- | design decision 3 (docs/SUFFLAMEN-DESIGN.md) Sufflamen is RIG-ONLY: it makes
-- | no browser sound, so the browser side is a *pure editor + visualizer*. That
-- | is exactly what this prototype is — the visual heart, silent, which is what
-- | ships until the rig path lands. Nothing here is throwaway.
-- |
-- | The one idea is the VOICE RACK: a named voice = a sample buffer + a read
-- | policy (begin/end splice window, chop count, reverse) + (later) a per-event
-- | param bag. Triggers from the rest of the family will route *into* voices by
-- | name; here we author and read them.
-- |
-- | The signature move is drawing SuperDirt's most-confused pair as a picture:
-- |   • CHOP    — each sample is cut into n contiguous slices, read in order,
-- |               *within its own lane*. On the bench the read-order arcs stay
-- |               low and horizontal, then leap to the next lane.
-- |   • STRIATE — the slice index is read *across* the voices first: slice 0 of
-- |               every voice, then slice 1, … The arcs zig-zag between lanes.
-- | The read-head cursor is COMPUTED here (a free-running phase), never reported
-- | — a visualization, per decision 3. On the rig the BEAM is the sole sound.
module Triggerfish.Sufflamen.Component (component) where

import Prelude

import Data.Array (concatMap, length, mapWithIndex, modifyAt, range, (!!))
import Data.Const (Const)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (abs, sin) as Num
import Data.String.Common (joinWith)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Timer (setInterval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Subscription as HS
import Triggerfish.Ui.Knob (knob)
import Triggerfish.Ui.Pointer (padNorm)
import Triggerfish.Odonus.Grid.Widgets (style, engrave)
import Halogen.Widgets.Svg (svgAttr, svgEl)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (EventType(..))
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME

-- A mouse handler that attaches to an SVG (namespaced) element: `HE.handler`
-- is row-polymorphic so it unifies with svgEl's `IProp ()`, unlike the concrete
-- `HE.onMouseDown` &c. The event is a MouseEvent at runtime (safe coerce).
onSvg :: forall i. String -> (MouseEvent -> i) -> HH.IProp () i
onSvg name f = HE.handler (EventType name) (\e -> f (unsafeCoerce e))

-- ---------------------------------------------------------------------------
-- Model
-- ---------------------------------------------------------------------------

-- | One rack voice. `wave` is a synthesized magnitude envelope (a stand-in for
-- | the real sample buffer — this is a visualizer, decision 3); `begin`/`end`
-- | are the splice window in [0,1] of the buffer; `reverse` flips the read
-- | direction of that voice's slices.
type Voice =
  { name :: String        -- the rack key; doubles as the SuperDirt `s` value
  , s :: String           -- the sound/folder it points at (msm supplies these in D2)
  , wave :: Array Number   -- magnitude envelope in [0,1]
  , begin :: Number
  , end :: Number
  , reverse :: Boolean
  }

-- | How the rack's one pattern line decomposes the slices into read events.
data SliceMode = Chop | Striate

derive instance Eq SliceMode

-- | Which splice handle a lane drag is moving.
data Drag = DBegin Int | DEnd Int

type State =
  { rack :: Array Voice
  , sel :: Int            -- selected voice (the one whose controls the strip edits)
  , chopN :: Int          -- the rack chop/striate count, 1..32
  , mode :: SliceMode
  , phase :: Number       -- the computed read-head position over the whole read order, [0,1)
  , playing :: Boolean
  , drag :: Maybe Drag
  }

data Action
  = Init
  | Tick
  | SelectVoice Int
  | StepChop Int          -- +1 / -1 detent
  | SetMode SliceMode
  | ToggleReverse Int
  | TogglePlay
  | LaneDown Int Int Int   -- voice, clientX, clientY  — begin a splice drag on nearest handle
  | LaneMove Int Int Int
  | LaneUp
  | KnobNoop

-- ---------------------------------------------------------------------------
-- Bench geometry (SVG user units)
-- ---------------------------------------------------------------------------

vbW :: Number
vbW = 1000.0

benchX0 :: Number
benchX0 = 70.0

benchX1 :: Number
benchX1 = 952.0

benchW :: Number
benchW = benchX1 - benchX0

laneTopY :: Number
laneTopY = 86.0

laneH :: Number
laneH = 88.0

laneGap :: Number
laneGap = 32.0

waveBars :: Int
waveBars = 88

laneTop :: Int -> Number
laneTop v = laneTopY + toNumber v * (laneH + laneGap)

laneCenter :: Int -> Number
laneCenter v = laneTop v + laneH / 2.0

vbH :: Int -> Number
vbH n = laneTopY + toNumber n * (laneH + laneGap) + 26.0

laneId :: Int -> String
laneId v = "suf-lane-" <> show v

-- ---------------------------------------------------------------------------
-- Waveform synthesis (deterministic; a visual stand-in for a real buffer)
-- ---------------------------------------------------------------------------

-- A cheap deterministic hash → [0,1). Enough to give each voice its own grain.
hashNoise :: Int -> Int -> Number
hashNoise seed i =
  let x = Num.sin (toNumber (seed * 374761 + i * 668265)) * 43758.5453
  in x - toNumber (floor x)

-- A drum/break-ish magnitude envelope: `transients` sharp attacks that decay,
-- roughened by the hash. Different `seed`/`transients` per voice = different
-- silhouettes on the bench.
synthWave :: Int -> Int -> Array Number
synthWave seed transients =
  map sample (range 0 (waveBars - 1))
  where
  n = toNumber waveBars
  sample i =
    let
      t = toNumber i / n
      -- nearest transient onset and its decay
      seg = t * toNumber transients
      frac = seg - toNumber (floor seg)
      env = decay frac
      grain = 0.55 + 0.45 * hashNoise seed i
    in
      clamp01 (env * grain)
  decay f = if f < 0.06 then f / 0.06 else expDecay ((f - 0.06) / 0.94)
  -- a crude e^{-3x} without importing exp: (1-x)^3 is close enough for a look
  expDecay x = let y = 1.0 - x in y * y * y
  clamp01 x = if x < 0.0 then 0.0 else if x > 1.0 then 1.0 else x

initialRack :: Array Voice
initialRack =
  [ { name: "bd",    s: "bd:3",   wave: synthWave 11 4,  begin: 0.0,  end: 1.0,  reverse: false }
  , { name: "sn",    s: "sn:1",   wave: synthWave 29 3,  begin: 0.08, end: 0.86, reverse: false }
  , { name: "break", s: "amen",   wave: synthWave 7  9,  begin: 0.0,  end: 1.0,  reverse: false }
  ]

-- ---------------------------------------------------------------------------
-- Read order — the chop/striate decomposition made explicit
-- ---------------------------------------------------------------------------

type Cell = { v :: Int, sl :: Int }

-- Slice index actually read, honouring a voice's reverse flag.
readSlice :: Boolean -> Int -> Int -> Int
readSlice rev n sl = if rev then n - 1 - sl else sl

-- The sequence of (voice, slice) reads the rack's pattern line produces.
--   Chop:    all of voice 0's slices in order, then voice 1's, … (stays in lane)
--   Striate: slice 0 of every voice, then slice 1 of every voice, … (crosses lanes)
readOrder :: SliceMode -> Array Voice -> Int -> Array Cell
readOrder mode rack n =
  case mode of
    Chop ->
      concatMap (\v -> map (\sl -> { v, sl: rs v sl }) idxs) voices
    Striate ->
      concatMap (\sl -> map (\v -> { v, sl: rs v sl }) voices) idxs
  where
  voices = range 0 (length rack - 1)
  idxs = range 0 (n - 1)
  rs v sl = readSlice (fromMaybe false (map _.reverse (rack !! v))) n sl

-- Window x-bounds and cell width for a voice.
winX :: Voice -> { x0 :: Number, x1 :: Number }
winX vc = { x0: benchX0 + vc.begin * benchW, x1: benchX0 + vc.end * benchW }

cellX :: Voice -> Int -> Int -> { left :: Number, center :: Number, w :: Number }
cellX vc n sl =
  let { x0, x1 } = winX vc
      w = (x1 - x0) / toNumber (max 1 n)
  in { left: x0 + toNumber sl * w, center: x0 + (toNumber sl + 0.5) * w, w }

-- ---------------------------------------------------------------------------
-- Component
-- ---------------------------------------------------------------------------

component :: forall m. MonadAff m => H.Component (Const Void) Unit Void m
component =
  H.mkComponent
    { initialState: \_ ->
        { rack: initialRack, sel: 0, chopN: 4, mode: Chop
        , phase: 0.0, playing: true, drag: Nothing }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Init }
    }

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Void m Unit
handleAction = case _ of
  Init -> do
    { emitter, listener } <- liftEffect HS.create
    _ <- H.subscribe emitter
    _ <- liftEffect $ setInterval 45 (HS.notify listener Tick)
    pure unit

  Tick -> do
    st <- H.get
    when st.playing do
      let steps = max 1 (length st.rack * st.chopN)
          -- ~6 ticks per cell → a legible sweep
          dp = 1.0 / (toNumber steps * 6.0)
          p = st.phase + dp
      H.modify_ _ { phase = if p >= 1.0 then p - 1.0 else p }

  SelectVoice v -> H.modify_ _ { sel = v }

  StepChop d -> H.modify_ \st -> st { chopN = clamp 1 32 (st.chopN + d), phase = 0.0 }

  SetMode m -> H.modify_ _ { mode = m, phase = 0.0 }

  ToggleReverse v ->
    H.modify_ \st -> st { rack = fromMaybe st.rack (modifyAt v (\vc -> vc { reverse = not vc.reverse }) st.rack) }

  TogglePlay -> H.modify_ \st -> st { playing = not st.playing }

  LaneDown v cx cy -> do
    p <- liftEffect $ padNorm (laneId v) cx cy
    let nx = clamp01 p.x
    st <- H.get
    case st.rack !! v of
      Nothing -> pure unit
      Just vc -> do
        let d = if Num.abs (nx - vc.begin) <= Num.abs (nx - vc.end) then DBegin v else DEnd v
        H.modify_ _ { sel = v, drag = Just d }
        applyDrag d nx

  LaneMove v cx cy -> do
    st <- H.get
    case st.drag of
      Just d | dragVoice d == v -> do
        p <- liftEffect $ padNorm (laneId v) cx cy
        applyDrag d (clamp01 p.x)
      _ -> pure unit

  LaneUp -> H.modify_ _ { drag = Nothing }

  KnobNoop -> pure unit

dragVoice :: Drag -> Int
dragVoice = case _ of
  DBegin v -> v
  DEnd v -> v

-- Move one splice handle to `nx`, keeping a minimum window and begin < end.
applyDrag :: forall m. Drag -> Number -> H.HalogenM State Action () Void m Unit
applyDrag d nx =
  H.modify_ \st -> st { rack = fromMaybe st.rack (modifyAt (dragVoice d) upd st.rack) }
  where
  gap = 0.05
  upd vc = case d of
    DBegin _ -> vc { begin = clampN 0.0 (vc.end - gap) nx }
    DEnd _ -> vc { end = clampN (vc.begin + gap) 1.0 nx }

clamp01 :: Number -> Number
clamp01 = clampN 0.0 1.0

clampN :: Number -> Number -> Number -> Number
clampN lo hi v = if v < lo then lo else if v > hi then hi else v

-- ---------------------------------------------------------------------------
-- View
-- ---------------------------------------------------------------------------

-- Cutting-room palette (warm charcoal bench, tape brown, china-marker chalk,
-- amber splice marks) — a darker sibling of the family's Hainbach beige.
chalk :: String
chalk = "#cbbf9e"

amber :: String
amber = "#e6a63a"

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  HH.div
    [ style $ "position:fixed;top:var(--tf-bar);left:0;right:0;bottom:0;box-sizing:border-box;"
        <> "display:flex;flex-direction:column;background:#1d1b16;color:" <> chalk
        <> ";font-family:Georgia,serif;overflow:hidden" ]
    [ nameplate
    , HH.div
        [ style "flex:1 1 auto;display:flex;min-height:0" ]
        [ rackRail st
        , HH.div [ style "flex:1 1 auto;min-width:0;padding:14px 18px;overflow:auto" ] [ bench st ]
        ]
    , controlStrip st
    ]

-- The engraved nameplate + the honest rig-only capability chip.
nameplate :: forall m. H.ComponentHTML Action () m
nameplate =
  HH.div
    [ style $ "flex:0 0 auto;display:flex;align-items:center;justify-content:space-between;"
        <> "padding:9px 18px;background:linear-gradient(#2b2820,#211e18);border-bottom:1px solid #000" ]
    [ HH.div [ style "display:flex;align-items:baseline;gap:12px" ]
        [ HH.span [ style "font-family:Georgia,serif;font-size:15px;letter-spacing:0.28em;color:#e7dcbd" ]
            [ HH.text "TRIGGERFISH" ]
        , HH.span [ style "font-size:11px;letter-spacing:0.34em;color:#8f866c" ] [ HH.text "· MODEL SUFFLAMEN" ]
        ]
    , HH.span
        [ style $ "font-size:9px;letter-spacing:0.18em;text-transform:uppercase;padding:4px 10px;border-radius:3px;"
            <> "background:#3a2c12;border:1px solid #5a4517;color:" <> amber ]
        [ HH.text "Rig-only · SuperDirt via Atlantis" ]
    ]

-- The voice rack: the instrument's ONE idea. Named voices; the selected one is
-- lit; a green pilot lamp = a routable target exists (the design's "no-target"
-- lamp goes red/dim when a routed trigger finds no voice — all present here).
rackRail :: forall m. State -> H.ComponentHTML Action () m
rackRail st =
  HH.div
    [ style $ "flex:0 0 214px;box-sizing:border-box;padding:14px 12px;overflow-y:auto;"
        <> "background:#211e18;border-right:1px solid #000" ]
    ( [ HH.div [ style $ engrave <> ";font-size:11px;color:#8f866c;margin-bottom:10px" ]
          [ HH.text "Voice Rack" ]
      ] <> mapWithIndex (voiceRow st) st.rack
        <> [ HH.div [ style "margin-top:12px;font-size:10px;line-height:1.5;color:#6f6753;font-style:italic" ]
               [ HH.text "Triggers from Balistes · Selene · Odonus route in here by name." ] ]
    )

voiceRow :: forall m. State -> Int -> Voice -> H.ComponentHTML Action () m
voiceRow st i vc =
  let selected = st.sel == i
  in HH.div
      [ HE.onClick \_ -> SelectVoice i
      , style $ "display:flex;align-items:center;gap:9px;padding:8px 10px;margin-bottom:6px;cursor:pointer;"
          <> "border-radius:5px;border:1px solid " <> (if selected then amber else "#332f26")
          <> ";background:" <> (if selected then "#2e281b" else "#26221b") ]
      [ HH.span [ style "width:7px;height:7px;border-radius:50%;background:#5fae5f;box-shadow:0 0 5px #5fae5f88" ] []
      , HH.div [ style "flex:1 1 auto;min-width:0" ]
          [ HH.div [ style $ "font-size:14px;letter-spacing:0.04em;color:" <> (if selected then "#f0e6c8" else chalk) ]
              [ HH.text vc.name ]
          , HH.div [ style "font-family:'SF Mono',Menlo,monospace;font-size:10px;color:#8a8068" ]
              [ HH.text ("s \"" <> vc.s <> "\"" <> (if vc.reverse then "  ◀ rev" else "")) ]
          ]
      ]

-- The Steenbeck bench: one tape lane per voice, slice grid on the splice window,
-- draggable begin/end marks, the read-order arcs above, the computed cursor.
bench :: forall m. State -> H.ComponentHTML Action () m
bench st =
  let n = length st.rack
      order = readOrder st.mode st.rack st.chopN
  in svgEl "svg"
      [ svgAttr "viewBox" ("0 0 " <> show vbW <> " " <> show (vbH n))
      , svgAttr "width" "100%"
      , svgAttr "preserveAspectRatio" "xMidYMid meet"
      , svgAttr "style" "display:block;user-select:none"
      ]
      ( concatMap (laneGroup st) (mapWithIndex (\i vc -> { i, vc }) st.rack)
        -- arcs + cursor are non-interactive overlays: never let them steal the
        -- splice-handle drag from the lane rects underneath.
        <> [ svgEl "g" [ svgAttr "style" "pointer-events:none" ] (arcLayer st order)
           , svgEl "g" [ svgAttr "style" "pointer-events:none" ] (cursorLayer st order)
           ]
      )

-- One lane: backing, waveform bars, the dimmed-outside-window mask, slice grid,
-- splice handles, and the transparent full-lane drag surface (padNorm target).
laneGroup :: forall m. State -> { i :: Int, vc :: Voice } -> Array (H.ComponentHTML Action () m)
laneGroup st { i, vc } =
  let ty = laneTop i
      cy = laneCenter i
      selected = st.sel == i
      { x0, x1 } = winX vc
      n = st.chopN
  in
    [ svgEl "rect"
        [ svgAttr "x" (show benchX0), svgAttr "y" (show ty)
        , svgAttr "width" (show benchW), svgAttr "height" (show laneH)
        , svgAttr "rx" "4"
        , svgAttr "fill" (if selected then "#2a2419" else "#232019")
        , svgAttr "stroke" (if selected then "#4a3f22" else "#302b21")
        ] []
    -- the lane name, engraved into the bench at the left margin
    , svgEl "text"
        [ svgAttr "x" (show (benchX0 - 8.0)), svgAttr "y" (show (cy + 4.0))
        , svgAttr "text-anchor" "end"
        , svgAttr "style" ("font-family:Georgia,serif;font-size:13px;letter-spacing:0.06em;fill:"
            <> (if selected then "#f0e6c8" else "#8f866c")) ]
        [ HH.text vc.name ]
    ]
    <> waveform i vc
    -- active-window highlight (the spliced-in region reads warm)
    <> [ svgEl "rect"
           [ svgAttr "x" (show x0), svgAttr "y" (show ty)
           , svgAttr "width" (show (x1 - x0)), svgAttr "height" (show laneH)
           , svgAttr "fill" amber, svgAttr "fill-opacity" "0.05" ] [] ]
    <> sliceGrid i vc n
    <> spliceHandles i vc
    <> -- the drag surface: full-lane transparent rect, id'd for padNorm
       [ svgEl "rect"
           [ svgAttr "id" (laneId i)
           , svgAttr "x" (show benchX0), svgAttr "y" (show ty)
           , svgAttr "width" (show benchW), svgAttr "height" (show laneH)
           , svgAttr "fill" "#000", svgAttr "fill-opacity" "0"
           , svgAttr "style" "cursor:col-resize"
           , onSvg "mousedown" \e -> LaneDown i (ME.clientX e) (ME.clientY e)
           , onSvg "mousemove" \e -> LaneMove i (ME.clientX e) (ME.clientY e)
           , onSvg "mouseup" \_ -> LaneUp
           , onSvg "mouseleave" \_ -> LaneUp
           ] [] ]

-- The tape: magnitude bars mirrored around the lane centre.
waveform :: forall m. Int -> Voice -> Array (H.ComponentHTML Action () m)
waveform i vc =
  let cy = laneCenter i
      hMax = laneH * 0.42
      bw = benchW / toNumber waveBars
      bar j mag =
        let x = benchX0 + toNumber j * bw
            h = mag * hMax
        in svgEl "rect"
             [ svgAttr "x" (show (x + bw * 0.15)), svgAttr "y" (show (cy - h))
             , svgAttr "width" (show (bw * 0.7)), svgAttr "height" (show (h * 2.0))
             , svgAttr "fill" "#9c6f3f", svgAttr "fill-opacity" "0.85"
             ] []
  in mapWithIndex bar vc.wave

-- The slice grid inside the splice window: n cells, faint dividers + tiny index.
sliceGrid :: forall m. Int -> Voice -> Int -> Array (H.ComponentHTML Action () m)
sliceGrid i vc n =
  let ty = laneTop i
      dividers = concatMap divider (range 0 n)
      labels = concatMap label (range 0 (n - 1))
      divider sl =
        let c = cellX vc n sl
        in [ svgEl "line"
               [ svgAttr "x1" (show c.left), svgAttr "y1" (show ty)
               , svgAttr "x2" (show c.left), svgAttr "y2" (show (ty + laneH))
               , svgAttr "stroke" "#e8dcb4", svgAttr "stroke-opacity" "0.18"
               , svgAttr "stroke-dasharray" "2 3" ] [] ]
      label sl =
        let c = cellX vc n sl
        in if c.w < 13.0 then []
           else [ svgEl "text"
                    [ svgAttr "x" (show c.center), svgAttr "y" (show (ty + laneH - 6.0))
                    , svgAttr "text-anchor" "middle"
                    , svgAttr "style" "font-family:'SF Mono',monospace;font-size:8px;fill:#8f866c" ]
                    [ HH.text (show sl) ] ]
  in dividers <> labels

-- Begin/end splice marks: an amber vertical rule + a triangular china-marker tab.
spliceHandles :: forall m. Int -> Voice -> Array (H.ComponentHTML Action () m)
spliceHandles i vc =
  let ty = laneTop i
      { x0, x1 } = winX vc
      mark x label =
        [ svgEl "line"
            [ svgAttr "x1" (show x), svgAttr "y1" (show (ty - 4.0))
            , svgAttr "x2" (show x), svgAttr "y2" (show (ty + laneH + 4.0))
            , svgAttr "stroke" amber, svgAttr "stroke-width" "1.5" ] []
        , svgEl "path"
            [ svgAttr "d" ("M" <> show (x - 5.0) <> " " <> show (ty - 4.0)
                <> " L" <> show (x + 5.0) <> " " <> show (ty - 4.0)
                <> " L" <> show x <> " " <> show (ty + 4.0) <> " Z")
            , svgAttr "fill" amber ] []
        , svgEl "text"
            [ svgAttr "x" (show x), svgAttr "y" (show (ty - 8.0))
            , svgAttr "text-anchor" "middle"
            , svgAttr "style" ("font-family:'SF Mono',monospace;font-size:7px;fill:" <> amber) ]
            [ HH.text label ]
        ]
  in mark x0 "BEG" <> mark x1 "END"

-- The read-order arcs: the whole point. Each consecutive pair of reads is a
-- quadratic bow from one cell's top-centre to the next. CHOP → low horizontal
-- hops that stay in a lane; STRIATE → arcs that leap between lanes every step.
arcLayer :: forall m. State -> Array Cell -> Array (H.ComponentHTML Action () m)
arcLayer st order =
  if length order < 2 then []
  else concatMap seg (range 0 (length order - 2))
  where
  n = st.chopN
  cur = currentStep st order
  pt c = map (\vc -> { x: (cellX vc n c.sl).center, y: laneTop c.v }) (st.rack !! c.v)
  seg k = fromMaybe [] do
    a <- order !! k
    b <- order !! (k + 1)
    pa <- pt a
    pb <- pt b
    pure (arc pa pb (k == cur))
  arc a b isCur =
    let mx = (a.x + b.x) / 2.0
        bow = 20.0 + Num.abs (a.y - b.y) * 0.28
        my = (if a.y < b.y then a.y else b.y) - bow
        d = "M" <> show a.x <> " " <> show a.y
              <> " Q" <> show mx <> " " <> show my
              <> " " <> show b.x <> " " <> show b.y
    in [ svgEl "path"
           [ svgAttr "d" d, svgAttr "fill" "none"
           , svgAttr "stroke" (if isCur then "#ffd88a" else amber)
           , svgAttr "stroke-width" (if isCur then "2.4" else "1")
           , svgAttr "stroke-opacity" (if isCur then "1" else "0.42") ] [] ]

-- The computed read-head: which cell we're in and where inside it, from `phase`.
currentStep :: State -> Array Cell -> Int
currentStep st order =
  let s = length order
  in if s == 0 then 0 else clamp 0 (s - 1) (floor (st.phase * toNumber s))

cursorLayer :: forall m. State -> Array Cell -> Array (H.ComponentHTML Action () m)
cursorLayer st order =
  let s = length order
  in if s == 0 then []
     else
       let stepF = st.phase * toNumber s
           step = clamp 0 (s - 1) (floor stepF)
           local = stepF - toNumber step
       in case order !! step of
            Nothing -> []
            Just c -> case st.rack !! c.v of
              Nothing -> []
              Just vc ->
                let cell = cellX vc st.chopN c.sl
                    x = cell.left + local * cell.w
                    ty = laneTop c.v
                    cy = laneCenter c.v
                in [ -- highlight the cell being read
                     svgEl "rect"
                       [ svgAttr "x" (show cell.left), svgAttr "y" (show ty)
                       , svgAttr "width" (show cell.w), svgAttr "height" (show laneH)
                       , svgAttr "fill" "#ffd88a", svgAttr "fill-opacity" "0.10" ] []
                   , svgEl "line"
                       [ svgAttr "x1" (show x), svgAttr "y1" (show (ty - 2.0))
                       , svgAttr "x2" (show x), svgAttr "y2" (show (ty + laneH + 2.0))
                       , svgAttr "stroke" "#fff6d8", svgAttr "stroke-width" "1.5" ] []
                   , svgEl "circle"
                       [ svgAttr "cx" (show x), svgAttr "cy" (show cy)
                       , svgAttr "r" "4", svgAttr "fill" "#fff6d8"
                       , svgAttr "style" "filter:drop-shadow(0 0 4px #ffd88a)" ] []
                   ]

-- ---------------------------------------------------------------------------
-- Bottom control strip: chop knob + mode toggle + reverse + the pattern line.
-- ---------------------------------------------------------------------------

controlStrip :: forall m. State -> H.ComponentHTML Action () m
controlStrip st =
  let selVoice = fromMaybe { name: "—", s: "", wave: [], begin: 0.0, end: 1.0, reverse: false } (st.rack !! st.sel)
  in HH.div
      [ style $ "flex:0 0 auto;display:flex;align-items:center;gap:26px;padding:12px 20px;"
          <> "background:linear-gradient(#2b2820,#211e18);border-top:1px solid #000" ]
      [ transportBtn st
      , chopControl st
      , modeToggle st
      , reverseControl selVoice st.sel
      , patternLine st
      ]

transportBtn :: forall m. State -> H.ComponentHTML Action () m
transportBtn st =
  HH.button
    [ HE.onClick \_ -> TogglePlay
    , style $ "padding:8px 14px;border-radius:6px;cursor:pointer;border:1px solid #5a4517;"
        <> "font-family:Georgia,serif;font-size:12px;letter-spacing:0.14em;color:#211e18;"
        <> "background:" <> amber ]
    [ HH.text (if st.playing then "❚❚ SCRUB" else "▶ ROLL") ]

chopControl :: forall m. State -> H.ComponentHTML Action () m
chopControl st =
  HH.div [ style "display:flex;align-items:center;gap:10px" ]
    [ HH.div [ style "width:52px;height:52px" ]
        [ knob { cx: 24.0, cy: 24.0, rOuter: 22.0, rInner: 13.0, color: amber
               , lo: 1, hi: 32, value: st.chopN, ticks: 0 } KnobNoop ]
    , HH.div [ style "display:flex;flex-direction:column;align-items:center;gap:3px" ]
        [ HH.div [ style $ engrave <> ";font-size:9px;color:#8f866c" ] [ HH.text "Chop" ]
        , HH.div [ style "display:flex;align-items:center;gap:6px" ]
            [ detentBtn "◀" (StepChop (-1))
            , HH.span [ style ("font-family:'SF Mono',monospace;font-size:15px;min-width:22px;text-align:center;color:" <> chalk) ]
                [ HH.text (show st.chopN) ]
            , detentBtn "▶" (StepChop 1)
            ]
        ]
    ]

detentBtn :: forall m. String -> Action -> H.ComponentHTML Action () m
detentBtn label act =
  HH.button
    [ HE.onClick \_ -> act
    , style $ "width:22px;height:22px;border-radius:4px;cursor:pointer;border:1px solid #4a4230;"
        <> "background:#2e2a20;color:" <> chalk <> ";font-size:11px;line-height:1" ]
    [ HH.text label ]

modeToggle :: forall m. State -> H.ComponentHTML Action () m
modeToggle st =
  HH.div [ style "display:flex;flex-direction:column;gap:4px" ]
    [ HH.div [ style $ engrave <> ";font-size:9px;color:#8f866c" ] [ HH.text "Read order" ]
    , HH.div [ style "display:flex;border:1px solid #4a4230;border-radius:5px;overflow:hidden" ]
        [ modeSeg "CHOP" (st.mode == Chop) (SetMode Chop)
        , modeSeg "STRIATE" (st.mode == Striate) (SetMode Striate)
        ]
    ]

modeSeg :: forall m. String -> Boolean -> Action -> H.ComponentHTML Action () m
modeSeg label active act =
  HH.button
    [ HE.onClick \_ -> act
    , style $ "padding:6px 13px;border:0;cursor:pointer;font-family:Georgia,serif;font-size:11px;"
        <> "letter-spacing:0.12em;color:" <> (if active then "#211e18" else chalk)
        <> ";background:" <> (if active then amber else "#2e2a20") ]
    [ HH.text label ]

reverseControl :: forall m. Voice -> Int -> H.ComponentHTML Action () m
reverseControl vc sel =
  HH.div [ style "display:flex;flex-direction:column;gap:4px" ]
    [ HH.div [ style $ engrave <> ";font-size:9px;color:#8f866c" ] [ HH.text ("Voice · " <> vc.name) ]
    , HH.button
        [ HE.onClick \_ -> ToggleReverse sel
        , style $ "padding:6px 13px;border-radius:5px;cursor:pointer;border:1px solid #4a4230;"
            <> "font-family:Georgia,serif;font-size:11px;letter-spacing:0.12em;"
            <> "color:" <> (if vc.reverse then "#211e18" else chalk)
            <> ";background:" <> (if vc.reverse then amber else "#2e2a20") ]
        [ HH.text (if vc.reverse then "◀ REVERSE" else "▶ FORWARD") ]
    ]

-- The one rack-level OG-Tidal pattern line — Sufflamen's heritage and its
-- portability guarantee (`s "bd sn break" # chop 4` is valid upstream Tidal).
-- Rendered as a china-marker scrawl on the bench glass.
patternLine :: forall m. State -> H.ComponentHTML Action () m
patternLine st =
  let names = joinWith " " (map _.name st.rack)
      fn = case st.mode of
        Chop -> "chop"
        Striate -> "striate"
      txt = "s \"" <> names <> "\" # " <> fn <> " " <> show st.chopN
  in HH.div
       [ style $ "flex:1 1 auto;display:flex;align-items:center;justify-content:flex-end;"
           <> "font-family:'SF Mono',Menlo,monospace;font-size:13px;letter-spacing:0.02em;"
           <> "color:#d8cf9e;text-shadow:0 0 6px #00000060" ]
       [ HH.text txt ]
