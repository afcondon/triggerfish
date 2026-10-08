-- | **The lattice's field, drawn outside Halogen** (AC, 2026-10-06: "the SVG
-- | entirely outside the Halogen event loop", as in the hylograph force demos).
-- |
-- | Halogen renders one empty `<div id="vetula-field">` and hands this module a
-- | `Scene`: plain data saying where every mark is, how large, whether it is
-- | shown, and the frame to fit. The field draws it with HATS `rerender`,
-- | keyed, so a mark is the same DOM node in every arrangement, and HATS's
-- | update transitions ease its position, size and opacity to the new scene.
-- | One requestAnimationFrame loop ticks those transitions and eases the
-- | viewBox to the new frame, then stops: an idle field costs nothing, and a
-- | change of rung costs one draw, not one Halogen render a frame.
-- |
-- | Hover never redraws: it retints the marks in place (`restyle`), one
-- | attribute (`data-hi`) on each mark whose tint changed, with the tints
-- | themselves in a stylesheet the caller supplies (`Scene.css`).
-- |
-- | Every numeric attribute HATS can ease is on a mark's own element, so a
-- | mark is a nested `<svg>` positioned by x, y, width and height (its glyph
-- | drawn about 0,0 in a fixed viewBox), not a `<g>` with a transform string.
module Vetula.Field
  ( Handle
  , Scene
  , Mark
  , Edge
  , Furniture
  , ViewBox
  , new
  , draw
  , mounted
  , setView
  , restyle
  , select
  , containerId
  ) where

import Prelude

import Data.Array (drop, filter, length, mapMaybe, mapWithIndex, nub, sort, take, zip)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.String (joinWith)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Hylograph.HATS (PhaseSpec, Tree, elem, forEachWithGUP, withBehaviors, onMouseEnter, onMouseLeave, onClickWithModifier)
import Hylograph.HATS.Friendly as F
import Hylograph.HATS.InterpreterTick (rerender)
import Hylograph.HATS.Transitions (HATSTransitions, TransitionResult(..), tickTransitions)
import Hylograph.Internal.Element.Types (ElementType(..))
import Hylograph.Internal.Transition.Types (Easing(CubicInOut), transition)
import Hylograph.Transition.RAF (performanceNow, requestAnimationFrame)
import Hylograph.Transition.Tick (easeInOutCubic, lerp)
import Web.DOM.Element (Element)

type ViewBox = { x :: Number, y :: Number, w :: Number, h :: Number }

-- | One chord mark. `key` is its identity across arrangements.
type Mark =
  { key :: String
  , root :: Int
  , pcs :: Array Int
  , x :: Number
  , y :: Number
  , scale :: Number
  , shown :: Boolean
  , label :: String
  , below :: Boolean          -- label under the glyph, or beside it
  , tint :: String            -- the `data-hi` value: a key into `Scene.css`
  , enter :: Effect Unit
  , leave :: Effect Unit
  , select :: Effect Unit       -- a click: make this the field's cursor
  , take :: Effect Unit         -- a shift-click: select and take it
  }

-- | An edge belongs to the level of its higher end (0 key, 1 common, 2
-- | four-note, 3 extended). Edges fade by level, as groups: 1,300 lines at a fractional
-- | opacity each cost 80 ms a frame to paint, one group at one costs nothing.
type Edge = { x1 :: Number, y1 :: Number, x2 :: Number, y2 :: Number, level :: Int }

type Furniture =
  { rects :: Array { x :: Number, y :: Number, w :: Number, h :: Number }
  , texts :: Array { x :: Number, y :: Number, anchor :: String, text :: String, css :: String }
  }

type Scene =
  { banks :: Boolean
  , lattice :: Furniture
  , bank :: Furniture
  , edges :: Array Edge
  , edgeLevels :: Int          -- levels whose edges show: 0 none, 4 all
  , marks :: Array Mark
  , css :: String             -- the tints, as rules on `.vf-mark[data-hi=…]`
  , selected :: Maybe String  -- the cursor's mark, by key
  , path :: Array String      -- the progression, as mark keys in order
  }

-- | The field's running state: the transitions in flight, the viewBox easing,
-- | whether a frame is booked, and each mark's element and current tint.
newtype Handle = Handle
  { transitions :: Ref (Maybe HATSTransitions)
  , view :: Ref { from :: ViewBox, to :: ViewBox, elapsed :: Number }
  , looping :: Ref Boolean
  , marks :: Ref (Map String { el :: Element, root :: Int, pcs :: Array Int, tint :: String })
  , selected :: Ref (Maybe String)
  }

containerId :: String
containerId = "vetula-field"

selector :: String
selector = "#" <> containerId

-- | How long a change of arrangement takes.
duration :: Number
duration = 650.0

new :: Effect Handle
new = do
  transitions <- Ref.new Nothing
  let v0 = { x: -440.0, y: -300.0, w: 880.0, h: 600.0 }
  view <- Ref.new { from: v0, to: v0, elapsed: duration }
  looping <- Ref.new false
  marks <- Ref.new Map.empty
  selected <- Ref.new Nothing
  pure (Handle { transitions, view, looping, marks, selected })

-- | Draw a scene. Marks already on the page ease to their new places. Halogen
-- | owns the container, so after a change of stage it is a new, empty div:
-- | then everything is placed at once and fades in, and the answer is `true`
-- | so the caller sets the frame without easing it.
draw :: Handle -> Scene -> Effect Boolean
draw (Handle h) scene = hostPresent_ selector >>= if _ then go else pure false
  where
  go = do
    fresh <- ensureRoot_ selector scene.css
    when fresh (Ref.write Nothing h.transitions)
    res <- rerender (selector <> " > svg") (sceneTree scene)
    -- the new transitions supersede any in flight: an element still moving
    -- starts again from where it is now
    Ref.write res.transitions h.transitions
    -- each mark's element, for hover
    let els = fromMaybe [] (Map.lookup "vf-marks" res.selections)
        byData = Map.fromFoldable (map (\m -> Tuple m.key m) scene.marks)
    keyed <- traverse (\el -> (\k -> Tuple k el) <$> hatsKey_ el) els
    Ref.write
      (Map.fromFoldable
         (mapMaybe (\(Tuple k el) -> map (\m -> Tuple k { el, root: m.root, pcs: m.pcs, tint: m.tint }) (Map.lookup k byData)) keyed))
      h.marks
    Ref.write scene.selected h.selected
    startLoop (Handle h)
    pure fresh

-- | Whether the field's SVG is on the page. Halogen owns the container, so it
-- | can be a new, empty div after any render that rebuilt it.
mounted :: Effect Boolean
mounted = rootPresent_ selector

-- | Move the frame. Animated for a change of arrangement, immediate for a pan.
setView :: Handle -> Boolean -> ViewBox -> Effect Unit
setView (Handle h) animate vb = do
  cur <- currentView (Handle h)
  if animate && differs cur vb then do
    Ref.write { from: cur, to: vb, elapsed: 0.0 } h.view
    startLoop (Handle h)
  else do
    Ref.write { from: vb, to: vb, elapsed: duration } h.view
    setViewBox_ selector (vbStr vb)
  where
  differs a b = a.x /= b.x || a.y /= b.y || a.w /= b.w || a.h /= b.h

-- | Retint the glyphs for a hover, touching only the marks whose tint changed.
restyle :: Handle -> (Int -> Array Int -> String) -> Effect Unit
restyle (Handle h) tintOf = do
  ms <- Ref.read h.marks
  ms' <- traverse
    (\m -> do
       let t = tintOf m.root m.pcs
       if t == m.tint then pure m
       else do
         setAttr_ m.el "data-hi" t
         pure m { tint = t })
    ms
  Ref.write ms' h.marks

-- | Move the cursor ring to another mark (or none), without a redraw.
select :: Handle -> Maybe String -> Effect Unit
select (Handle h) next = do
  prev <- Ref.read h.selected
  when (prev /= next) do
    ms <- Ref.read h.marks
    for_ (prev >>= \k -> Map.lookup k ms) \m -> setAttr_ m.el "data-sel" "0"
    for_ (next >>= \k -> Map.lookup k ms) \m -> setAttr_ m.el "data-sel" "1"
    Ref.write next h.selected

-- ---------------------------------------------------------------------------
-- The animation loop
-- ---------------------------------------------------------------------------

startLoop :: Handle -> Effect Unit
startLoop (Handle h) = do
  running <- Ref.read h.looping
  unless running do
    Ref.write true h.looping
    t0 <- performanceNow
    last <- Ref.new t0
    let frame now = do
          prev <- Ref.read last
          Ref.write now last
          let dt = Number.min 50.0 (now - prev)
          -- the marks
          mt <- Ref.read h.transitions
          marksDone <- case mt of
            Nothing -> pure true
            Just ts -> tickTransitions dt ts >>= case _ of
              Complete -> Ref.write Nothing h.transitions $> true
              Running ts' -> Ref.write (Just ts') h.transitions $> false
          -- the frame
          v <- Ref.read h.view
          viewDone <-
            if v.elapsed >= duration then pure true
            else do
              let el = Number.min duration (v.elapsed + dt)
              Ref.write v { elapsed = el } h.view
              setViewBox_ selector (vbStr (viewAt v.from v.to (el / duration)))
              pure (el >= duration)
          if marksDone && viewDone then Ref.write false h.looping
          else void (requestAnimationFrame frame)
    void (requestAnimationFrame frame)

currentView :: Handle -> Effect ViewBox
currentView (Handle h) = do
  v <- Ref.read h.view
  pure (viewAt v.from v.to (v.elapsed / duration))

viewAt :: ViewBox -> ViewBox -> Number -> ViewBox
viewAt a b p =
  let e = easeInOutCubic (Number.min 1.0 (Number.max 0.0 p))
  in { x: lerp a.x b.x e, y: lerp a.y b.y e, w: lerp a.w b.w e, h: lerp a.h b.h e }

vbStr :: ViewBox -> String
vbStr v = joinWith " " (map show [ v.x, v.y, v.w, v.h ])

-- ---------------------------------------------------------------------------
-- The tree
-- ---------------------------------------------------------------------------

-- | The glyph's own box: a mark is `markBox` across at scale 1.
markBox :: Number
markBox = 40.0

glyphR :: Number
glyphR = 9.0

ease :: forall a. PhaseSpec a
ease = { attrs: [], transition: Just ((transition (Milliseconds duration)) { easing = Just CubicInOut }) }

sceneTree :: Scene -> Tree
sceneTree scene =
  elem Group [ F.class_ "vf-root" ]
    [ elem Group [ F.class_ "vf-furniture" ]
        [ forEachWithGUP "vf-furniture" Group
            [ { key: "lattice", on: not scene.banks, f: scene.lattice }
            , { key: "banks", on: scene.banks, f: scene.bank } ]
            _.key
            furnitureTree
            { enter: Nothing, update: Just ease, exit: Nothing } ]
    , elem Group [ F.class_ "vf-edges" ]
        [ forEachWithGUP "vf-edges" Group
            (map (\l -> { level: l, on: l < scene.edgeLevels, lines: filter (\e -> e.level == l) scene.edges }) [ 0, 1, 2, 3 ])
            (\g -> show g.level)
            edgeGroup
            { enter: Nothing, update: Just ease, exit: Nothing } ]
    , elem Group [ F.class_ "vf-path", F.opacity (if scene.banks then "0" else "1"), F.style "pointer-events: none;" ]
        [ forEachWithGUP "vf-path-seg" Line (pathSegments scene) _.key segTree
            { enter: Just { attrs: [ F.opacity "0" ], transition: ease.transition }, update: Just ease, exit: Nothing }
        ]
    , elem Group [ F.class_ "vf-marks" ]
        [ forEachWithGUP "vf-marks" SVG scene.marks _.key (markTree scene.selected)
            { enter: Just { attrs: [ F.opacity "0" ], transition: ease.transition }
            , update: Just ease
            , exit: Nothing } ]
    , elem Group [ F.class_ "vf-beads", F.opacity (if scene.banks then "0" else "1"), F.style "pointer-events: none;" ]
        [ forEachWithGUP "vf-path-bead" SVG (pathBeads scene) _.key beadTree
            { enter: Just { attrs: [ F.opacity "0" ], transition: ease.transition }, update: Just ease, exit: Nothing }
        ]
    ]

-- | The progression's steps, placed on their marks. A chord that recurs gets a
-- | bead per occurrence, fanned round the glyph so every number shows.
type Step = { key :: String, n :: Int, x :: Number, y :: Number, k :: Int }

pathSteps :: Scene -> Array Step
pathSteps scene =
  let at = Map.fromFoldable (map (\m -> Tuple m.key { x: m.x, y: m.y }) scene.marks)
      placed = mapMaybe (\(Tuple i key) -> map (\p -> { i, key, p }) (Map.lookup key at)) (mapWithIndex Tuple scene.path)
  in mapWithIndex
       (\j e ->
          { key: e.key, n: e.i + 1, x: e.p.x, y: e.p.y
          , k: length (filter (\o -> o.key == e.key) (take j placed)) })
       placed

pathSegments :: Scene -> Array { key :: String, x1 :: Number, y1 :: Number, x2 :: Number, y2 :: Number }
pathSegments scene =
  let st = pathSteps scene
  in mapMaybe
       (\(Tuple a b) -> if a.key == b.key then Nothing
                         else Just { key: "s" <> show a.n, x1: a.x, y1: a.y, x2: b.x, y2: b.y })
       (zip st (drop 1 st))

pathBeads :: Scene -> Array { key :: String, n :: Int, x :: Number, y :: Number }
pathBeads scene =
  map
    (\s ->
       -- top-left first, clear of a name beside the glyph, then round anticlockwise
       let a = (-3.0 * Number.pi / 4.0) - toNumber s.k * (Number.pi / 4.0)
       in { key: "b" <> show s.n, n: s.n, x: s.x + 15.0 * Number.cos a, y: s.y + 15.0 * Number.sin a })
    (pathSteps scene)

segTree :: { key :: String, x1 :: Number, y1 :: Number, x2 :: Number, y2 :: Number } -> Tree
segTree e =
  elem Line
    [ F.x1 e.x1, F.y1 e.y1, F.x2 e.x2, F.y2 e.y2, F.opacity "1"
    , F.style "stroke: #2f6f8f; stroke-width: 2; stroke-opacity: 0.55; stroke-linecap: round;" ]
    []

beadTree :: { key :: String, n :: Int, x :: Number, y :: Number } -> Tree
beadTree b =
  elem SVG
    [ F.x (b.x - 7.0), F.y (b.y - 7.0), F.width 14.0, F.height 14.0
    , F.viewBox (-7.0) (-7.0) 14.0 14.0, F.attr "overflow" "visible", F.opacity "1" ]
    [ elem Circle [ F.cx 0.0, F.cy 0.0, F.r 6.2, F.style "fill: #2f6f8f; stroke: #fff; stroke-width: 1;" ] []
    , elem Text [ F.x 0.0, F.y 2.8, F.textAnchor "middle", F.style "font-size: 8px; font-weight: 600; fill: #fff; -webkit-user-select: none; user-select: none;", F.attr "textContent" (show b.n) ] []
    ]

furnitureTree :: { key :: String, on :: Boolean, f :: Furniture } -> Tree
furnitureTree g =
  elem Group
    [ F.opacity (if g.on then "1" else "0")
    , F.style "pointer-events: none;"
    ]
    ( map (\r -> elem Rect [ F.x r.x, F.y r.y, F.width r.w, F.height r.h, F.attr "rx" "6", F.style "fill: #fbf8f0; stroke: #ece5d2; stroke-width: 1;" ] []) g.f.rects
        <> map (\t -> elem Text [ F.x t.x, F.y t.y, F.textAnchor t.anchor, F.style t.css, F.attr "textContent" t.text ] []) g.f.texts )

edgeGroup :: { level :: Int, on :: Boolean, lines :: Array Edge } -> Tree
edgeGroup g =
  elem Group
    [ F.opacity (if g.on then "1" else "0")
    , F.style "stroke: #e8e4d6; stroke-width: 1; pointer-events: none;"
    ]
    (map (\e -> elem Line [ F.x1 e.x1, F.y1 e.y1, F.x2 e.x2, F.y2 e.y2 ] []) g.lines)

markTree :: Maybe String -> Mark -> Tree
markTree selected m =
  let half = markBox * m.scale / 2.0
      sorted = sort (nub (map (\p -> mod p 12) m.pcs))
      ang p = (-Number.pi / 2.0) + toNumber p * (Number.pi / 6.0)
      pt p = { x: glyphR * Number.cos (ang p), y: glyphR * Number.sin (ang p) }
  in elem SVG
      [ F.x (m.x - half), F.y (m.y - half), F.width (2.0 * half), F.height (2.0 * half)
      , F.viewBox (-markBox / 2.0) (-markBox / 2.0) markBox markBox
      , F.attr "overflow" "visible"
      , F.class_ "vf-mark"
      , F.attr "data-hi" m.tint
      , F.attr "data-sel" (if selected == Just m.key then "1" else "0")
      , F.opacity (if m.shown then "1" else "0")
      , F.attr "pointer-events" (if m.shown then "auto" else "none")
      ]
      ( [ elem Polygon
            [ F.points (joinWith " " (map (\p -> let q = pt p in show q.x <> "," <> show q.y) sorted))
            , F.class_ "vf-poly"
            ]
            []
        ]
          <> map
               (\p ->
                  let q = pt p
                      isRoot = p == mod m.root 12
                  in elem Circle
                       [ F.cx q.x, F.cy q.y, F.r 1.6
                       , F.class_ (if isRoot then "vf-dot vf-root" else "vf-dot")
                       ]
                       [])
               sorted
          <> (if m.label == "" then []
              else
                [ elem Text
                    ( (if m.below then [ F.x 0.0, F.y (glyphR + 12.0), F.textAnchor "middle" ]
                       else [ F.x (glyphR + 10.0), F.y 4.0 ])
                        <> [ F.style ("font-size: " <> (if m.below then "8px" else "11px") <> "; fill: #5a5240; pointer-events: none; -webkit-user-select: none; user-select: none;")
                           , F.attr "textContent" m.label ] )
                    [] ])
          <> [ elem Circle [ F.cx 0.0, F.cy 0.0, F.r (glyphR + 6.5), F.class_ "vf-cursor" ] []
             , withBehaviors [ onMouseEnter m.enter, onMouseLeave m.leave, onClickWithModifier m.select m.take ]
                 (elem Circle [ F.cx 0.0, F.cy 0.0, F.r (glyphR + 3.0), F.style "fill: transparent; cursor: pointer;" ] [])
             ]
      )

foreign import ensureRoot_ :: String -> String -> Effect Boolean
foreign import setViewBox_ :: String -> String -> Effect Unit
foreign import setAttr_ :: Element -> String -> String -> Effect Unit
foreign import hatsKey_ :: Element -> Effect String
foreign import hostPresent_ :: String -> Effect Boolean
foreign import rootPresent_ :: String -> Effect Boolean
