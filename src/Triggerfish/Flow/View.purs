-- | `Triggerfish.Flow.View` — the signal-flow chart, drawn.
-- |
-- | `Triggerfish.Flow` decides what is on the chart; hylograph-layout's Sankey
-- | decides where it goes, with the columns and the order within each column
-- | held fixed (`nodeLayer`, `nodeSort`), so the chart holds still as machines
-- | open and close; this draws it. Width is streams, colour is the signal on
-- | each hop, and a machine is drawn with its fish.
module Triggerfish.Flow.View
  ( Handlers
  , Live
  , chart
  , key
  , KeyHandlers
  , Port
  , Patch
  , Dock
  , DaemonLamp
  , Lens(..)
  ) where

import Prelude

import Data.Array (filter, foldl, mapMaybe, nub, (!!))
import Data.Array as Array
import Data.Int (fromNumber, toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Number as Number
import Data.String (joinWith)
import Data.Tuple (Tuple(..), snd)
import Data.Number (abs)
import Data.String as String
import Data.Tuple.Nested ((/\))
import Data.Number.Format (fixed, toStringWith)
import DataViz.Layout.Sankey.Lanes (computeLayoutWithLanes, generateRoutePath, laneOf)
import DataViz.Layout.Sankey.Types (defaultSankeyConfig)
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..), ElemName(..), Namespace(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.MouseEvent as ME
import Triggerfish.Bosun (Lamp(..))
import Triggerfish.Flow.Order (orderOf, rankIn)
import Triggerfish.Flow (Column(..), Flow, Lasts(..), Link, Signal(..), columnTitle, keeps, layerOf, loopOf, nodeRank, onTheBeat, signalLabel, skeleton, storeLink)

-- | What the chart reports: a machine hovered (or left), and a machine picked.
-- | `link`: a link clicked, with its machine and the node it runs into.
-- | `play`: a machine's fish pressed. `port`: a harmony port clicked (`src:key`, `in:grid`); `cable`: a cable
-- | clicked, by its source port and input port.
type Handlers i =
  { hover :: Maybe String -> i, link :: String -> String -> i
  , port :: String -> i, cable :: String -> String -> i
  , play :: String -> Boolean -> i
  , peek :: String -> i
  -- | a daemon's ↻, in the X-ray: restart it (by Bosun's service id)
  , restart :: String -> i
  -- | Limulus's engine picked on its node: "architeuthis" or "ghci"
  , engine :: String -> i
  -- | a loop's starfish clicked: its machine, its mark's number, and whether
  -- | it is playing now (so the click stops it) or kept (so it starts)
  , loop :: String -> Int -> Boolean -> i
  }

-- | What is known of one process behind a node: Bosun's word on a daemon
-- | (`service`), or the rig doctor's on a device (no service, so no
-- | restart). `label`: what it is to this node, said before its state (""
-- | when the node's own name says it); `caption`: its state as the X-ray prints it; `tip`: what a
-- | restart costs; `canRestart`: false while the group is held or a restart
-- | is already on its way.
type DaemonLamp =
  { node :: String, lamp :: Lamp, title :: String
  , service :: Maybe String, label :: String, caption :: String, canRestart :: Boolean, tip :: String
  }

-- | Every machine, for the dock: the ones not on the chart (closed, or open
-- | with nothing routed) stand under the flowing ones, and a closed one is
-- | opened from there. `playable`: its fish is its play button.
-- | `rig`: its page is closed but the rig is playing it (the stage says so);
-- | its fish faces the audio and stops it on the rig.
-- | `remote`: its fish plays and stops it on the rig, page or no page
-- | (Selene, whose banks run on the modular).
type Dock = { slot :: String, name :: String, open :: Boolean, playing :: Boolean, playable :: Boolean, alias :: Maybe String, href :: String, target :: String, rig :: Boolean, remote :: Boolean }

-- | The harmony patch bay drawn on the chart. A source has an output port:
-- | on its machine's label (Vetula's key and voices), on its own node in
-- | Feeds (a routed scale or pattern), or, while an input is armed, as a
-- | ghost beside it (a scale or pattern not routed yet). Odonus has an input
-- | port for its grid and one for its out. `allowed` names the inputs a
-- | source may feed; `routes` the cables; `armed` the port clicked first.
type Port = { id :: String, short :: String, label :: String, machine :: Maybe String, allowed :: Array String }
type Patch = { sources :: Array Port, routes :: Array { input :: String, source :: String }, armed :: Maybe String }

width :: Number
width = 1500.0

-- | A link's drawn width, in streams. Signal is its streams. Control is drawn
-- | at half its machine's streams (at least one): wide enough to read as the
-- | way in, grey so it does not read as notes (AC, 2026-10-03: pretty over
-- | strict here). A recorded line is thin.
widthOf :: Link -> Number
widthOf l
  | l.bone = 1.0
  | l.signal == Recorded = 0.4
  | l.control = max 1.0 (0.5 * toNumber l.streams)
  | otherwise = toNumber l.streams

-- Room on the left for the fish and the machines' names, and on the right for
-- the last column's labels.
left :: Number
left = 230.0

right :: Number
right = 1320.0

-- | The chart's height follows the number of streams, so one sequencer playing
-- | Ableton is a modest line and not one fat ribbon.
heightOf :: Flow -> Number
heightOf f = clampN 250.0 720.0 (130.0 + toNumber streams * 11.0 + toNumber rows * 22.0)
  where
  streams = foldl (+) 0 (map _.streams (filter (\l -> l.to == "ears") f.links))
  rows = foldl max 1 (map (\c -> Array.length (filter (\nd -> nd.column == c) f.nodes)) (nub (map _.column f.nodes)))
  clampN lo hi x = max lo (min hi x)

-- | How the chart is looked at. `Flowing`: what plays, and where it goes.
-- | `XRay`: the rig's processes, on its whole skeleton, with the flow faded
-- | behind them (was the Atlantis page). `Storage`: what each node keeps and
-- | how long it lasts, on the same skeleton, so switching between the two
-- | moves nothing. Outside `Storage`, what is mostly storage (the sample
-- | sets, loops not playing, the lines that only record) is ghosted in its
-- | place rather than dropped, so the layout holds.
data Lens = Flowing | XRay | Storage

derive instance Eq Lens

-- | What the chart shows of the moment. `playing` names the machines
-- | sounding now; the rest are drawn ghosted rather than dropped, since an
-- | open page that is stopped is still part of the picture, and when nothing
-- | plays the whole chart rests. `rigUp` is whether the page reaches
-- | purerl-tidal, shown on the kraken. `tempo` sets the beat's pulse.
-- | `lamps`: what Bosun says of the daemon behind a node, when it was reached.
type Live =
  { playing :: Array String, rigUp :: Boolean, tempo :: Number
  , lamps :: Array DaemonLamp
  , lens :: Lens
  -- | where Limulus sends its Tidal: "architeuthis" or "ghci"
  , limulusEngine :: String
  -- | the key as a filter: kinds of line hidden (the chart is laid out
  -- | without them), and the kind hovered in the key (lit, the rest dimmed)
  , hidden :: Array String
  , keyHot :: Maybe String
  , patch :: Patch
  , dock :: Array Dock
  -- | the rig's clock is locked to Diaphus's Link anchor: Diaphus is active
  , locked :: Boolean
  -- | the closed machine whose 'open ↗' is showing (a plain click on it)
  , peeked :: Maybe String
  }

-- | A line's kind, as the key names it: control whatever it carries, else
-- | its signal.
kindOf :: Link -> String
kindOf l
  | l.control && l.signal /= Recorded = "control"
  | otherwise = sigClass l.signal

-- | The flow less the hidden kinds, and the nodes nothing reaches any more.
hide :: Array String -> Flow -> Flow
hide hidden f
  | Array.null hidden = f
  | otherwise = { nodes: filter (\nd -> Array.elem nd.id ends) f.nodes, links }
  where
  links = filter (\l -> not (Array.elem (kindOf l) hidden)) f.links
  ends = nub (Array.concatMap (\l -> [ l.from, l.to ]) links)

chart :: forall w i. Handlers i -> Maybe String -> Live -> Flow -> HH.HTML w i
chart on hot live = chartOf on hot live <<< (if live.lens /= Flowing then skeleton else identity) <<< hide live.hidden

chartOf :: forall w i. Handlers i -> Maybe String -> Live -> Flow -> HH.HTML w i
chartOf on hot live f =
      svg "svg"
        [ attr "viewBox" ("0 0 " <> n width <> " " <> n (max h (dockBottom + 10.0)))
        , attr "class" ("flows" <> (if hot == Nothing then "" else " hovering") <> (if live.keyHot == Nothing then "" else " keying") <> (if Array.null live.playing then " resting" else "") <> (if xray then " xray" else "") <> (if store then " storage" else ""))
        , attr "style" ("--beat: " <> n (60.0 / max 20.0 live.tempo) <> "s")
        , attr "role" "img"
        , attr "aria-label" "Where each machine's output goes: through the browser or the rig, through interfaces and instruments, to your ears"
        ]
        ( beatBand <> heads <> [ rule ] <> map link (filter (not <<< isQuant) laid.routes) <> map node laid.nodes <> beatMarks <> patchBay <> dock <> empty )
  where
  -- the X-ray and the storage lens share the skeleton and its layout
  xray = live.lens /= Flowing
  store = live.lens == Storage
  -- In Atlantis, link-spike stands above the flow: the beat, broadcast to
  -- everything the rig times, rather than one more hop in it.
  beat = onTheBeat f
  band = if Array.null beat then 0.0 else 58.0
  -- A machine fed from the left wears its label above its bar: room for it
  -- under the column heads.
  fedRoom = if Array.any (\l -> l.signal == Quantise) f.links then 84.0 else 0.0
  -- the X-ray's lines need room under each node, and on the right
  h = heightOf f + band + fedRoom + (if xray then 90.0 else 0.0)
  rightEdge = if xray then right - 90.0 else right
  byId = Map.fromFoldable (map (\x -> x.id /\ x) f.nodes)
  ours sn = Map.lookup sn.name byId
  -- A long line from the machines' end (Feeds to Page, a machine to its
  -- loops) gets a lane: a waypoint in each column it passes, so it is not
  -- drawn through the nodes there (hylograph's Sankey.Lanes). A lane sorts
  -- with the machine its line comes from.
  wants i _ = maybe false fromEnd (f.links !! i)
  fromEnd l = maybe false (\nd -> nd.column == Feeders || nd.column == Machines) (Map.lookup l.from byId)
  rankOf sn = case laneOf sn.name of
    Just w -> do
      l <- f.links !! w.link
      from <- Map.lookup l.from byId
      c <- columns !! w.layer
      pure (Tuple c (snd (nodeRank from)))
    Nothing -> map (rankIn order) (ours sn)
  -- the columns after the engine follow their wiring (Flow.Order)
  order = orderOf f
  laid = computeLayoutWithLanes wants
    (map (\l -> { s: l.from, t: l.to, v: widthOf l }) f.links)
    (defaultSankeyConfig width h)
      { nodeWidth = 5.0
      , nodePadding = if xray then 46.0 else 20.0
      , extent = { x0: left, y0: 48.0 + band + fedRoom, x1: rightEdge, y1: h - 40.0 }
      , nodeLayer = layerOf f
      , nodeSort = Just (comparing rankOf)
      }

  columns = Array.sort (nub (map _.column f.nodes))
  heads = mapMaybe colHead columns
  colHead c = do
    x <- if Just c == Array.head columns then Just 20.0 else
      Array.head (mapMaybe (\sn -> ours sn >>= \nd -> if nd.column == c then Just sn.x0 else Nothing) laid.nodes)
    pure $ svg "text" [ attr "class" "colhead", attr "x" (n x), attr "y" (n (24.0 + band)) ] [ HH.text (columnTitle c) ]
  rule = svg "line" [ attr "class" "colrule", attr "x1" "20", attr "x2" (n (width - 20.0)), attr "y1" (n (32.0 + band)), attr "y2" (n (32.0 + band)) ] []

  -- The lanternfish between broadcast arcs, and a gold mark pulsing on every
  -- node it times.
  beatBand
    | Array.null beat = []
    | otherwise =
        let cx = (left + right) / 2.0
        in
          [ svg "g" [ attr "class" "beat" ]
              ( arcs cx (-1.0) <> arcs cx 1.0 <>
                  -- facing right, towards the audio, while it keeps the beat
                  [ svg "use"
                      ( [ attr "href" "#ic-lantern", attr "x" (n (cx - 30.0)), attr "y" "14", attr "width" "60", attr "height" "18" ]
                          <> (if live.locked then [ attr "transform" ("translate(" <> n (2.0 * cx) <> " 0) scale(-1 1)") ] else [])
                      ) []
                  , label "beatlabel" cx 52.0 "middle" "Diaphus · the beat"
                  ] <> (if Array.any (\x -> x.id == "diaphus") f.nodes then [] else lampAt "diaphus" (cx + 58.0) 48.5)
              )
          ]
  arcs cx dir = [ 1.0, 2.0, 3.0 ] <#> \k ->
    let
      r = 30.0 + 9.0 * k
      x0 = cx + dir * (24.0 + 8.0 * k)
      x1 = x0
    in
      svg "path"
        [ attr "class" ("arc a" <> show (round' k))
        , attr "d" ("M" <> n x0 <> "," <> n (23.0 - r * 0.42) <> " A" <> n r <> "," <> n r <> " 0 0 " <> (if dir > 0.0 then "1" else "0") <> " " <> n x1 <> "," <> n (23.0 + r * 0.42))
        ] []
  beatMarks = laid.nodes # mapMaybe \sn ->
    if sn.name `Array.elem` beat
      then Just $ svg "circle" [ attr "class" "beatmark", attr "cx" (n (sn.x0 + 2.5)), attr "cy" (n (sn.y0 - 6.0)), attr "r" "3.4" ]
        [ svg "title" [] [ HH.text "on the beat: timed by Diaphus" ] ]
      else Nothing

  linkCls l | l.bone = "bone"
  linkCls l = (if storeLink l && not store then "stored " else "") <> sigClass l.signal <> (if l.control then " control" else "") <> (if live.keyHot == Just (kindOf l) then " khot" else "") <> (if Just l.machine == hot then " hot" else "") <> (if l.broken > 0 then " broken" else "") <> (if waits l then " waiting" else if sounding l then "" else " idle")
  -- A bone is a line, not a ribbon: it carries nothing, so it has no width
  -- to show (and alone on the chart a ribbon would fill the height).
  link route | Just l <- f.links !! route.index, l.bone =
    case Array.head route.segments, nodeAt l.from, nodeAt l.to of
      Just seg, Just a, Just b ->
        let xi = (a.x1 + b.x0) / 2.0
        in svg "path" [ attr "class" "bone", attr "d" ("M" <> n a.x1 <> "," <> n seg.y0 <> " C" <> n xi <> "," <> n seg.y0 <> " " <> n xi <> "," <> n seg.y1 <> " " <> n b.x0 <> "," <> n seg.y1) ]
             [ svg "title" [] [ HH.text (linkTitle l) ] ]
      _, _, _ -> svg "g" [] []
  link route =
    let
      ours' = f.links !! route.index
      cls = maybe "" linkCls ours'
    in
      svg "path" ([ attr "class" ("link " <> cls), attr "d" (generateRoutePath (laid.nodes <> laid.waypoints) route) ]
          <> maybe [] (\l -> [ HE.onClick \_ -> on.link l.machine l.to ]) ours')
        (maybe [] (\l -> [ svg "title" [] [ HH.text (linkTitle l) ] ]) ours')

  linkTitle l | l.bone = l.from <> " → " <> l.to <> " · " <> signalLabel l.signal <> " · the rig's wiring: nothing travels it now"
  linkTitle l =
    l.from <> " → " <> l.to <> " · " <> signalLabel l.signal
      <> (if l.signal == Quantise then " · quantisation: feeds " <> joinWith " and " (map (\w -> "odonus." <> w) l.wires) <> " (resolved on the rig)"
          else if l.signal == Recorded then " · its notes are kept in the rig's record buffer"
          else if l.control then " · control: the page tells the rig what to play; the rig makes the notes"
          else " · " <> plural l.streams "stream")
      <> (if l.broken > 0 then " · " <> show l.broken <> " not reaching " <> l.to else "")
      <> (if Array.null l.wires then "" else "\n" <> joinWith ", " l.wires)
      <> (if Array.null l.notes then "" else "\n" <> joinWith ", " l.notes)
      <> (if waits l then "\nplays only through the rig: switch to Atlantis to hear it" else "")

  waits l = l.waiting > 0 && l.waiting == l.streams
  -- A machine whose every stream waits for the rig says so under its name.
  needsAtlantis m =
    let ls = filter (\l -> l.machine == m) f.links
    in not (Array.null ls) && Array.all waits ls

  node sn = case ours sn of
    Nothing -> svg "g" [] []
    Just nd
      | not store && storedNode nd && not (annotated nd.id) -> svg "g" [ attr "class" "stored" ] [ node' sn nd ]
      | otherwise -> node' sn nd
  -- mostly storage: ghosted in place outside the storage lens, unless the
  -- X-ray has a process to show on it (Amphora, on the sample sets)
  storedNode nd = nd.id == "sets" || (isJust (loopOf nd.id) && nd.note /= "playing")
  node' sn nd = case nd.machine, loopOf nd.id of
      Just _, _ | isJust (String.stripPrefix (String.Pattern "src:") nd.id) -> voiceNode sn nd
      Just m, _ -> machineNode sn nd m
      Nothing, Just l -> loopNode sn nd l
      Nothing, Nothing
        | nd.column == Feeders -> feedNode sn nd
        | otherwise -> placeNode sn nd

  bar sn = svg "rect"
    [ attr "class" "bar", attr "x" (n sn.x0), attr "y" (n sn.y0)
    , attr "width" (n (sn.x1 - sn.x0)), attr "height" (n (max 2.0 (sn.y1 - sn.y0)))
    ] []

  mid sn = (sn.y0 + sn.y1) / 2.0

  machineNode sn nd m
    | Just nd.column /= Array.head columns = innerMachine sn nd m
    | otherwise = edgeMachine sn nd m

  -- A machine downstream of another (fed its harmony): its fish above its
  -- name, both just left of its bar, since the margin belongs to the first.
  innerMachine sn nd m
    | Array.any (\l -> l.to == nd.id) f.links = fedMachine sn nd m
    | otherwise = besideMachine sn nd m

  -- A machine fed from the left (its harmony): the lines come in where its
  -- label would be, so fish and name stand above its bar.
  fedMachine sn nd m =
    let
      top = sn.y0
      cx = (sn.x0 + sn.x1) / 2.0
    in
      svg "g"
        [ attr "class" ("node pick inner" <> procCls nd.id), attr "tabindex" "0", attr "role" "button"
        , attr "aria-label" (nd.name <> ", " <> plural (streamsOf m) "stream")
        , HE.onMouseEnter \_ -> on.hover (Just m)
        , HE.onMouseLeave \_ -> on.hover Nothing
        , HE.onFocus \_ -> on.hover (Just m)
        , HE.onBlur \_ -> on.hover Nothing
        ]
        [ svg "rect" [ attr "class" "hit", attr "x" (n (cx - 90.0)), attr "y" (n (top - 80.0)), attr "width" "180", attr "height" "80" ] []
        , bar sn
        , fishBtn m (cx - 27.0) (top - 80.0)
        , pickName m cx (top - 30.0) "middle" nd.name
        , subOr ("m:" <> m) "start" (cx - 70.0) (top - 16.0) (label "sub" cx (top - 16.0) "middle" (summary m))
        , svg "title" [] [ HH.text (tip nd m) ]
        ]

  -- --------------------------------------------------------------------
  -- The harmony patch bay: ports on the labels, cables between them.
  -- --------------------------------------------------------------------
  isQuant r = maybe false (\l -> l.signal == Quantise) (f.links !! r.index)
  quantShown = not (Array.elem "s-quant" live.hidden)
  nodeAt id = Array.find (\sn -> sn.name == id) laid.nodes
  pt x y = { x, y }

  -- Odonus's input ports stand on its node, at the top of its left edge,
  -- where the cables come in; its label stays centred over it.
  inputs = [ "grid", "out" ]
  inPort i = nodeAt "m:odonus" >>= \sn -> do
    k <- Array.elemIndex i inputs
    pure (pt (sn.x0 - 9.0) (sn.y0 + 8.0 + 17.0 * toNumber k))

  -- A source's output port: on its machine's label, in a row under it; on
  -- its own node in Feeds; else nowhere (a ghost, while an input is armed).
  qid s = "q:" <> maybe s.label (\m -> m <> " " <> s.label) s.machine
  srcPort s = case s.machine >>= \m -> nodeAt ("m:" <> m) of
    Just sn ->
      let
        mine = filter (\x -> x.machine == s.machine) live.patch.sources
        k = toNumber (Array.length mine)
        j = toNumber (fromMaybe 0 (Array.findIndex (\x -> x.id == s.id) mine))
      in Just (pt (sn.x0 - 16.0 - (k - 1.0 - j) * 17.0) (mid sn + 28.0))
    Nothing -> nodeAt (qid s) <#> \sn -> pt (sn.x1 + 9.0) (mid sn)

  armedInput = live.patch.armed >>= String.stripPrefix (String.Pattern "in:")
  armedSource = live.patch.armed >>= \a -> Array.find (\s -> s.id == a) live.patch.sources
  -- unrouted sources with nowhere to stand, shown beside an armed input
  ghosts = case armedInput >>= inPort of
    Just ip -> Array.mapWithIndex (\k s -> { s, p: pt (ip.x - 40.0) (ip.y + 26.0 + 20.0 * toNumber k) })
      (filter (\s -> srcPort s == Nothing && Array.elem (fromMaybe "" armedInput) s.allowed) live.patch.sources)
    Nothing -> []

  portState id compatible used =
    (if live.patch.armed == Just id then " armed" else "")
      <> (if used then " on" else "")
      <> case live.patch.armed of
          Just a | a /= id -> if compatible then " can" else " cannot"
          _ -> ""
  usedSrc s = Array.any (\r -> r.source == s.id) live.patch.routes
  usedIn i = Array.any (\r -> r.input == i) live.patch.routes

  portDot id cls' p short tip =
    svg "g" [ attr "class" ("port " <> cls'), attr "role" "button", attr "tabindex" "0", HE.onClick \_ -> on.port id ]
      [ svg "circle" [ attr "cx" (n p.x), attr "cy" (n p.y), attr "r" "6.5" ] []
      , label "pl" p.x (p.y + 3.0) "middle" short
      , svg "title" [] [ HH.text tip ]
      ]

  cable r = do
    s <- Array.find (\x -> x.id == r.source) live.patch.sources
    a <- srcPort s
    b <- inPort r.input
    let dx = max 40.0 (abs (b.x - a.x) / 2.0)
    pure $ svg "path"
      [ attr "class" "cable", attr "d" ("M" <> n a.x <> "," <> n a.y <> " C" <> n (a.x + dx) <> "," <> n a.y <> " " <> n (b.x - dx) <> "," <> n b.y <> " " <> n b.x <> "," <> n b.y)
      , HE.onClick \_ -> on.cable r.source r.input
      ]
      [ svg "title" [] [ HH.text ("odonus." <> r.input <> " ← " <> s.label <> " · click to unplug") ] ]

  patchBay
    | not quantShown || xray = []
    | otherwise =
        mapMaybe cable live.patch.routes
          <> mapMaybe (\s -> srcPort s <#> \p ->
               portDot s.id ("src" <> portState s.id (maybe false (\i -> Array.elem i s.allowed) armedInput) (usedSrc s)) p s.short
                 (s.label <> " · feeds " <> joinWith " or " (map ("odonus." <> _) s.allowed))) live.patch.sources
          <> mapMaybe (\i -> inPort i <#> \p ->
               svg "g" []
                 [ portDot ("in:" <> i) ("in" <> portState ("in:" <> i) (maybe false (\s -> Array.elem i s.allowed) armedSource) (usedIn i)) p "" ("odonus." <> i <> ": click, then a source")
                 , label "pin" (p.x - 10.0) (p.y + 3.0) "end" i
                 ]) inputs
          <> (ghosts <#> \g ->
               svg "g" [ attr "class" "ghost" ]
                 [ label "sub" (g.p.x - 11.0) (g.p.y + 3.5) "end" g.s.label
                 , portDot g.s.id "src can" g.p g.s.short (g.s.label <> " · plug into odonus." <> fromMaybe "" armedInput)
                 ])

  -- One voice of a machine opened into its voices: its name, no fish.
  voiceNode sn nd =
    svg "g" [ attr "class" "node voice" ]
      [ bar sn, label "sub" (sn.x0 - 8.0) (mid sn + 3.5) "end" nd.name ]

  -- A machine's one line: its streams and where it plays. The rest (the
  -- loaded preset, what it is) is in its hover.
  summary m
    | needsAtlantis m = "needs Atlantis"
    | otherwise = plural (streamsOf m) "stream" <> " · " <> playsWhere m
  tip nd m = nd.name <> (if nd.note == "" then "" else ", " <> nd.note) <> preset m
  nameWidth t = 7.8 * toNumber (String.length t)

  dockOf m = Array.find (\d -> d.slot == m) live.dock
  preset m = maybe "" (\a -> " · " <> a) (dockOf m >>= _.alias)
  -- A machine's fish says its state: ghosted when its page is not running
  -- (the dock), facing left while it runs, and turned to face right,
  -- towards the audio, while it plays. A playable machine's fish is also its
  -- play and stop, as on its own page.
  fishBtn m x y = case dockOf m of
    Just d | d.playable && (d.open || d.remote) ->
      svg "g"
        [ attr "class" ("fish fishbtn" <> if d.playing then " playing" else ""), attr "role" "button", attr "tabindex" "0"
        , attr "aria-label" ((if d.playing then "Stop " else "Play ") <> d.name)
        , HE.onClick \e -> if modified e then on.peek m else on.play m (not d.playing)
        ]
        [ fishUse m x y d.playing, svg "title" [] [ HH.text ((if d.playing then "Stop " else "Play ") <> d.name) ] ]
    Just d | d.rig ->
      svg "g"
        [ attr "class" "fish fishbtn playing", attr "role" "button", attr "tabindex" "0"
        , attr "aria-label" ("Stop " <> d.name <> " on the rig")
        , HE.onClick \e -> if modified e then on.peek m else on.play m false
        ]
        [ fishUse m x y true, svg "title" [] [ HH.text ("Stop " <> d.name <> ": the rig plays it, with no page open") ] ]
    Just d ->
      svg "g" [ attr "class" ("fish" <> if d.playing then " playing" else "") ] [ fishUse m x y d.playing ]
    Nothing -> use ("sp-" <> m) x y 54.0 32.0
  -- A cmd- (ctrl-, shift-) click on a fish opens its page (the link around
  -- it, in the dock), so it does not also play or stop it there.
  modified e = ME.metaKey e || ME.ctrlKey e || ME.shiftKey e
  -- The fish, reflected in place about its own middle when facing right.
  fishUse m x y right =
    svg "use"
      ( [ attr "href" ("#sp-" <> m), attr "x" (n x), attr "y" (n y), attr "width" "54", attr "height" "32" ]
          <> (if right then [ attr "transform" ("translate(" <> n (2.0 * x + 54.0) <> " 0) scale(-1 1)") ] else [])
      ) []
  pickName _ x y anchor txt =
    svg "text" [ attr "class" "name", attr "x" (n x), attr "y" (n y), attr "text-anchor" anchor ] [ HH.text txt ]

  -- The dock: every machine not on the chart, under the ones that are, in
  -- the left margin. A closed one is ghosted and opens on a click; an open
  -- one with nothing routed says so.
  onChart m = Array.any (\nd -> nd.machine == Just m) f.nodes
  docked = filter (\d -> not (onChart d.slot)) live.dock
  dockTop =
    let bottoms = map _.y1 (filter (\sn -> sn.x0 < left + 300.0) laid.nodes)
    in (foldl max (48.0 + band + fedRoom) bottoms) + 44.0
  dockItem k d =
    let
      y = dockTop + 38.0 * toNumber k
    in
      svg "g" [ attr "class" ("dock" <> (if d.open then " open" else " closed") <> procCls ("m:" <> d.slot)) ]
        [ svg "rect" [ attr "class" "hit", attr "x" "10", attr "y" (n (y - 18.0)), attr "width" (n (left - 20.0)), attr "height" "36" ] []
        , if d.open then svg "g" [] [ fishBtn d.slot 23.0 (y - 16.0), label "name" (left - 10.0) (y - 1.0) "end" d.name ]
          -- playing on the rig with its page closed (a loop, Selene's banks):
          -- the fish plays and stops it there, and the name still opens the
          -- page, as a closed machine's does (AC, 2026-10-05: Selene could
          -- not be opened while its banks ran)
          else if d.rig || d.remote then
            svg "g" []
              -- the fish too, as before (AC cmd-clicks the fish): a plain click
              -- on it plays or stops, a cmd-click opens the page behind
              [ svg "a" [ attr "href" d.href, attr "target" d.target, attr "data-peek" "" ] [ fishBtn d.slot 23.0 (y - 16.0) ]
              , svg "a" [ attr "class" "ghostfish", attr "href" d.href, attr "target" d.target, attr "data-peek" "", HE.onClick \_ -> on.peek d.slot ]
                  [ label "name" (left - 10.0) (y - 1.0) "end" d.name, svg "title" [] [ HH.text ("Open " <> d.name) ] ]
              ]
          -- a closed machine: a plain click shows its 'open ↗'; the
          -- browser's own cmd-click opens it behind the dashboard
          else svg "a" [ attr "class" "ghostfish", attr "href" d.href, attr "target" d.target, attr "data-peek" "", HE.onClick \_ -> on.peek d.slot ]
                 [ use ("sp-" <> d.slot) 23.0 (y - 16.0) 54.0 32.0
                 , label "name" (left - 10.0) (y - 1.0) "end" d.name
                 , svg "title" [] [ HH.text d.name ] ]
        , if annotated ("m:" <> d.slot) then annotLines ("m:" <> d.slot) "end" (left - 10.0) (y + 12.0)
          else if live.peeked == Just d.slot && not d.open then
            svg "a" [ attr "class" "openlink", attr "href" d.href, attr "target" d.target ]
              [ label "sub" (left - 10.0) (y + 12.0) "end" "open ↗"
              , svg "title" [] [ HH.text ("Open " <> d.name) ] ]
          else if d.remote && not d.open then label "sub" (left - 10.0) (y + 12.0) "end" (if d.playing then "on the rig, no page" else "hushed on the rig")
          else if d.rig then label "sub" (left - 10.0) (y + 12.0) "end" "on the rig, no page"
          else if d.open then label "sub" (left - 10.0) (y + 12.0) "end" ("open · nothing routed" <> preset d.slot)
          else if live.peeked == Just d.slot then
            svg "a" [ attr "class" "openlink", attr "href" d.href, attr "target" d.target ]
              [ label "sub" (left - 10.0) (y + 12.0) "end" "open ↗"
              , svg "title" [] [ HH.text ("Open " <> d.name) ] ]
          else svg "g" [] []
        ]
  dock = Array.mapWithIndex dockItem docked
  -- with nothing routed anywhere, the chart is the dock and a line
  empty
    | Array.null f.links =
        [ label "flow-empty" (left + 40.0) (48.0 + band + 26.0) "start"
            if Array.null live.hidden then "Nothing is playing anywhere yet. Open a machine on the left and the chart shows where it goes."
            else "Every line drawn is of a kind hidden in the key. Click the key to show them again." ]
    | otherwise = []
  dockBottom = dockTop + 38.0 * toNumber (Array.length docked)

  besideMachine sn nd m =
    let cy = mid sn
    in
      svg "g"
        [ attr "class" ("node pick inner" <> procCls nd.id), attr "tabindex" "0", attr "role" "button"
        , attr "aria-label" (nd.name <> ", " <> plural (streamsOf m) "stream")
        , HE.onMouseEnter \_ -> on.hover (Just m)
        , HE.onMouseLeave \_ -> on.hover Nothing
        , HE.onFocus \_ -> on.hover (Just m)
        , HE.onBlur \_ -> on.hover Nothing
        ]
        [ svg "rect" [ attr "class" "hit", attr "x" (n (sn.x0 - 8.0 - nameWidth nd.name - 64.0)), attr "y" (n (cy - 20.0)), attr "width" (n (nameWidth nd.name + 72.0)), attr "height" "40" ] []
        , bar sn
        , fishBtn m (sn.x0 - 8.0 - nameWidth nd.name - 62.0) (cy - 18.0)
        , pickName m (sn.x0 - 8.0) (cy - 2.0) "end" nd.name
        , subOr ("m:" <> m) "end" (sn.x0 - 8.0) (cy + 11.0) (label "sub" (sn.x0 - 8.0) (cy + 11.0) "end" (summary m))
        , engineChoice m (sn.x0 - 8.0) (cy + 25.0)
        , svg "title" [] [ HH.text (tip nd m) ]
        ]

  -- A source of quantisation of its own (a scale, a pattern, a machine not
  -- drawn whole): named to the left of its bar, like a machine.
  feedNode sn nd =
    let cy = mid sn
    in
      svg "g" [ attr "class" "node feed" ]
        [ bar sn
        , label "name" (sn.x0 - 10.0) (cy - 2.0) "end" nd.name
        , label "sub" (sn.x0 - 10.0) (cy + 11.0) "end" nd.note
        ]

  edgeMachine sn nd m =
    let
      cy = mid sn
      reach = max 10.0 (min 22.0 ((sn.y1 - sn.y0) / 2.0 + 6.0))
    in
      svg "g"
        [ attr "class" ("node pick" <> procCls nd.id), attr "tabindex" "0", attr "role" "button"
        , attr "aria-label" (nd.name <> ", " <> plural (round' sn.value) "stream")
        , HE.onMouseEnter \_ -> on.hover (Just m)
        , HE.onMouseLeave \_ -> on.hover Nothing
        , HE.onFocus \_ -> on.hover (Just m)
        , HE.onBlur \_ -> on.hover Nothing
        ]
        -- A group catches the pointer only over what it paints, so the gap
        -- between the fish and the name needs something to land on.
        [ svg "rect" [ attr "class" "hit", attr "x" "10", attr "y" (n (cy - reach)), attr "width" (n (sn.x0 - 10.0)), attr "height" (n (2.0 * reach)) ] []
        , bar sn
        , fishBtn m 23.0 (cy - 16.0)
        , pickName m (sn.x0 - 10.0) (cy - 2.0) "end" nd.name
        , subOr ("m:" <> m) "end" (sn.x0 - 10.0) (cy + 11.0) (label "sub" (sn.x0 - 10.0) (cy + 11.0) "end" (summary m))
        , engineChoice m (sn.x0 - 10.0) (cy + 25.0)
        , svg "title" [] [ HH.text (tip nd m) ]
        ]

  -- A loop: a starfish in its machine's colour with the mark's number (AC,
  -- 2026-10-08: a feather star for Odonus, a sea star for Vetula), filled
  -- and pulsing while the rig plays it. A click starts or stops it.
  loopNode sn nd l =
    let
      cy = mid sn
      playing = nd.note == "playing"
      cx = sn.x1 + 15.0
      body cls = starfish l.machine cls cx cy
    in
      svg "g" [ attr "class" ("node loopnode pick m-" <> l.machine), attr "role" "button", attr "tabindex" "0"
              , attr "aria-label" (l.machine <> " loop " <> nd.name <> (if playing then ", playing: stop it" else ", kept: start it"))
              , HE.onClick \_ -> on.loop l.machine l.n playing ] $
        [ bar sn
        , svg "g" [ attr "class" ("bubble" <> if playing then " playing" else "") ]
            -- the pulse is a halo behind the starfish, so the number stays solid
            ( (if playing then [ body "halo" ] else []) <>
            [ body "disc"
            , label "bn" cx (cy + 3.0) "middle" nd.name
            ] )
        , svg "title" [] [ HH.text (l.machine <> " loop " <> nd.name <> (if playing then ": playing on the rig · click to stop it" else ": kept on the rig · click to play it")
            <> (if silenced l.machine then "\nIt cannot be heard: every path out of the rig for " <> l.machine <> " is broken." else "")) ]
        ] <> (if playing && silenced l.machine then [ stopSign (cx + 18.0) (cy - 9.0) ] else [])

  -- The two starfish, about 26 units across. Odonus's is a feather star: a
  -- small disc and ten slender arms that curl. Vetula's is a sea star: five
  -- wide arms with rounded tips. Each is one closed shape, so the loop's
  -- fill and stroke (and the halo) apply to it whole.
  starfish m cls cx cy =
    let
      pol r a = { x: cx + r * Number.cos a, y: cy + r * Number.sin a }
      p q = n q.x <> "," <> n q.y
      top = -Number.pi / 2.0
      shape d = svg "path" [ attr "class" cls, attr "d" d, attr "stroke-linejoin" "round" ] []
    in
      if m == "odonus" then
        let
          step = 2.0 * Number.pi / 10.0
          -- an arm leaves the disc, curls sunwise and comes back on its
          -- other side; the disc's rim joins one arm to the next
          arm k =
            let a = top + toNumber k * step
            in "L" <> p (pol 5.6 (a - 0.16))
                 <> "Q" <> p (pol 10.0 (a - 0.10)) <> " " <> p (pol 13.6 (a + 0.30))
                 <> "Q" <> p (pol 9.6 (a + 0.20)) <> " " <> p (pol 5.6 (a + 0.16))
        in shape ("M" <> p (pol 5.6 (top - 0.16)) <> joinWith "" (map arm (Array.range 0 9)) <> "Z")
      else
        let
          step = 2.0 * Number.pi / 5.0
          -- valley, up the arm's side, round the tip, down the other side
          arm k =
            let a = top + toNumber k * step
            in "L" <> p (pol 8.6 (a - 0.20))
                 <> "Q" <> p (pol 13.6 a) <> " " <> p (pol 8.6 (a + 0.20))
                 <> "L" <> p (pol 5.4 (a + step / 2.0))
        in shape ("M" <> p (pol 5.4 (top - step / 2.0)) <> joinWith "" (map arm (Array.range 0 4)) <> "Z")

  -- Whether everything the rig sends for a machine is broken: its loops
  -- play, and nobody hears them.
  silenced m =
    let out = filter (\x -> x.machine == m && x.from == "engine" && x.signal /= Recorded) f.links
    in not (Array.null out) && Array.all (\x -> x.broken > 0) out
  -- A stop sign: a red octagon with a white bar.
  stopSign x y =
    let
      r = 7.0
      pts = joinWith " " (map (\k -> let a = (toNumber k + 0.5) * Number.pi / 4.0 in n (x + r + r * Number.cos a) <> "," <> n (y + r + r * Number.sin a)) (Array.range 0 7))
    in
      svg "g" [ attr "class" "stopsign" ]
        [ svg "polygon" [ attr "points" pts ] []
        , svg "rect" [ attr "x" (n (x + 3.0)), attr "y" (n (y + r - 1.2)), attr "width" (n (2.0 * r - 6.0)), attr "height" "2.4" ] []
        ]

  placeNode sn nd =
    let
      cy = mid sn
      tx = sn.x1 + 8.0
      icon = case nd.id of
        "engine" -> [ use "ic-kraken" tx (cy - 34.0) 64.0 64.0 ]
        "ears" -> [ use "ic-ears" tx (cy - 18.0) 34.0 34.0 ]
        "sets" -> [ cylinder tx (cy - 11.0) ]
        _ -> []
      -- The kraken is drawn large, beside its label rather than above it (so
      -- it never climbs into the column heads). It says "the rig's engine"
      -- itself, so that line goes, and the label stays clear of the next
      -- column's even with all seven columns showing.
      lx = case nd.id of
        "ears" -> tx + 40.0
        "sets" -> tx + 24.0
        "engine" -> tx + 68.0
        _ -> tx
      -- with loops beside it, the page's line is short, so it clears them
      sub
        | annotated nd.id =
            [ annotLines nd.id "start" lx (cy + (if nd.id == "engine" then 27.0 else 11.0)) ]
        | nd.id == "engine" = []
        | nd.id == "browser" && Array.any (\x -> x.column == Loops) f.nodes =
            [ label "sub" lx (cy + 11.0) "start" (show (round' sn.value)) ]
        | otherwise = [ label "sub" lx (cy + 11.0) "start" (nd.note <> " · " <> show (round' sn.value)) ]
    in
      svg "g" [ attr "class" ("node" <> procCls nd.id) ]
        ( [ bar sn ] <> icon <> daemonLamp nd.id sn <>
            [ label "name" lx (cy - 2.0) "start" nd.name ]
            <> sub
            <> rigLamp nd.id lx cy
        )

  -- The store's sign: a database cylinder, 16 by 22.
  cylinder x y =
    svg "g" [ attr "class" "dbicon" ]
      [ svg "path" [ attr "d" ("M" <> n x <> "," <> n (y + 4.0) <> " v14 a8,4 0 0 0 16,0 v-14") ] []
      , svg "ellipse" [ attr "cx" (n (x + 8.0)), attr "cy" (n (y + 4.0)), attr "rx" "8", attr "ry" "4" ] []
      , svg "path" [ attr "class" "band", attr "d" ("M" <> n x <> "," <> n (y + 11.0) <> " a8,4 0 0 0 16,0") ] []
      ]

  -- Bosun's word on the daemons behind a node: a lamp each under its bar
  -- (in the X-ray they are lines beside its name instead).
  lampsOf id = filter (\l -> l.node == id) live.lamps
  -- in the X-ray, a node with a process behind it is drawn as itself; the
  -- rest are greyed
  -- (its name in the colour of its worst process's state)
  procCls id
    | store = if Array.null (keepsOf id) then "" else " proc kept"
    | otherwise = case lampsOf id of
        [] -> ""
        ls -> " proc st-" <> lampClass (worst (map _.lamp ls))
  worst ls
    | Array.elem Down ls = Down
    | Array.elem Coming ls = Coming
    | otherwise = Up
  daemonLamp id sn
    | xray = []
    | otherwise = lampAt id (sn.x0 + 2.5) (sn.y1 + 8.0)
  lampAt id x y = lampsOf id # Array.mapWithIndex \k l ->
      -- a link to the X-ray, where it can be restarted
      svg "a" [ attr "href" "#atlantis" ]
        [ svg "circle" [ attr "class" ("dlamp " <> lampClass l.lamp), attr "cx" (n (x + 10.0 * toNumber k)), attr "cy" (n y), attr "r" "4" ]
            [ svg "title" [] [ HH.text (l.title <> " · X-ray: restart it there") ] ]
        ]

  -- Limulus's engine, chosen on its node: where its Tidal goes is a fact
  -- about its path, so the choice is made where the path is drawn. One of
  -- two, right-aligned under its caption.
  engineChoice m x y
    | m /= "limulus" || xray = svg "g" [] []
    | otherwise =
        let w = 6.0 * toNumber (String.length "→ Haskell Tidal")
        in svg "g" [ attr "class" "engines", attr "role" "group", attr "aria-label" "Where Limulus sends Tidal" ]
             [ choice "ghci" "Haskell Tidal" x "Haskell Tidal (GHCi), to compare. Machine lines still go to the rig."
             , label "sub" (x - w - 5.0) y "middle" "·"
             , choice "architeuthis" "Architeuthis" (x - w - 10.0) "Architeuthis: the rig plays it, machines and all."
             ]
    where
    choice key name cx hint =
      let picked = live.limulusEngine == key
      in svg "g"
           ( [ attr "class" ("engine" <> if picked then " on" else ""), attr "role" "button", attr "tabindex" "0"
             , attr "aria-pressed" (if picked then "true" else "false") ]
               <> (if picked then [] else [ HE.onClick \_ -> on.engine key ])
           )
           [ label "sub" cx y "end" ("→ " <> name), svg "title" [] [ HH.text hint ] ]

  -- In the X-ray, a machine's caption is its page server's line, when it
  -- has one.
  subOr id anchor x y plain
    | annotated id = annotLines id anchor x y
    | otherwise = plain

  -- What a lens says under a node: its processes in the X-ray, what it
  -- keeps in the storage lens.
  keepsOf id = filter (\k -> k.node == id) keeps
  annotated id = case live.lens of
    XRay -> not (Array.null (lampsOf id))
    Storage -> not (Array.null (keepsOf id))
    Flowing -> false
  annotLines id
    | store = keepLines id
    | otherwise = lampLines id
  -- A line for each thing kept, coloured by how long it lasts; where it
  -- is kept is in its hover, which keeps the lines short.
  keepLines id anchor x y =
    svg "g" [ attr "class" "xlamps" ] $ keepsOf id # Array.mapWithIndex \k kp ->
      svg "g" [ attr "class" "xlamp" ]
        [ label ("sub " <> lastsClass kp.lasts) x (y + 14.0 * toNumber k) anchor kp.what
        , svg "title" [] [ HH.text (kp.what <> " · " <> kp.at <> "\n" <> lastsText kp.lasts) ]
        ]
  lastsClass = case _ of
    Lost -> "k-lost"
    OnDisk -> "k-disk"
    InBrowser -> "k-browser"
    Versioned -> "k-versioned"
  lastsText = case _ of
    Lost -> "in memory, gone when the process stops"
    OnDisk -> "a file on this machine, kept across restarts"
    InBrowser -> "this browser's storage for this address; another port or machine sees none of it"
    Versioned -> "in Amphora, content-addressed; old versions stay reachable"

  -- The X-ray's word on a node's processes: a line each, with its lamp and
  -- (for a daemon) its ↻, reading from the node outwards.
  lampLines id anchor x y =
    svg "g" [ attr "class" "xlamps" ] $ lampsOf id # Array.mapWithIndex \k l ->
      let
        yk = y + 14.0 * toNumber k
        dir = if anchor == "end" then -1.0 else 1.0
        txt = if l.label == "" then l.caption else l.label <> " · " <> l.caption
      in
        svg "g" [ attr "class" "xlamp" ]
          ( (case l.service of
              Just sv ->
                [ svg "g"
                    ( [ attr "class" ("restart" <> if l.canRestart then "" else " off"), attr "role" "button" ]
                        <> (if l.canRestart then [ attr "tabindex" "0", HE.onClick \_ -> on.restart sv ] else [])
                    )
                    [ svg "rect" [ attr "class" "hit", attr "x" (n (x + dir * 6.0 - 7.0)), attr "y" (n (yk - 10.0)), attr "width" "14", attr "height" "13" ] []
                    , label "rs" (x + dir * 6.0) (yk + 0.5) "middle" "↻"
                    , svg "title" [] [ HH.text (if l.canRestart then "Restart " <> sv <> ". " <> l.tip else l.title) ]
                    ]
                ]
              Nothing -> [])
            <>
              [ label "sub" (x + dir * 16.0) yk anchor txt
              , svg "title" [] [ HH.text l.title ]
              ]
          )
  lampClass = case _ of
    Up -> "up"
    Coming -> "coming"
    Down -> "down"
  -- The rig's link, on the rig: a lamp under purerl-tidal's label.
  rigLamp id x cy
    | id == "engine" && xray =
        [ label ("sub st-" <> if live.rigUp then "up" else "down") x (cy + 13.0) "start" (if live.rigUp then "connected" else "not connected") ]
    | id == "engine" =
        [ svg "circle" [ attr "class" (if live.rigUp then "lamp-on" else "lamp-off"), attr "cx" (n (x + 4.0)), attr "cy" (n (cy + 10.0)), attr "r" "4" ] []
        , label "sub" (x + 13.0) (cy + 13.0) "start" (if live.rigUp then "connected" else "not connected")
        ]
    | otherwise = []
  -- A machine's streams, counted where they are heard: its links into the
  -- page may be control, whose width says nothing.
  streamsOf m = foldl (+) 0 (map _.streams (filter (\l -> l.machine == m && l.to == "ears") f.links))
  -- Who makes the notes: the page, or the rig it tells.
  playsWhere m
    | Array.any (\l -> l.machine == m && l.control && l.signal /= Recorded) f.links = "on the rig"
    | maybe false _.rig (dockOf m) = "on the rig, no page"
    | Array.any (\l -> l.machine == m && l.from == "engine") f.links = "loops on the rig"
    | otherwise = "here"

  -- The sample sets sound whenever anything does.
  sounding l = l.machine `Array.elem` live.playing || (l.machine == "sets" && not (Array.null live.playing))

-- | The signals, as a key under the chart.
-- | The key under the chart, and a filter: a kind clicked is hidden (or
-- | shown again), a kind hovered is lit on the chart.
type KeyHandlers i = { toggle :: String -> i, hover :: Maybe String -> i, all :: i }

key :: forall w i. KeyHandlers i -> { hidden :: Array String, keyHot :: Maybe String } -> HH.HTML w i
key on st =
  HH.div [ HP.class_ (HH.ClassName "flow-key"), HP.attr (AttrName "role") "group", HP.attr (AttrName "aria-label") "Kinds of line: click to hide or show" ]
    ( map item kinds
        <> (if Array.null st.hidden then [] else [ HH.button [ HP.class_ (HH.ClassName "sig showall"), HE.onClick \_ -> on.all ] [ HH.text "show all" ] ])
    )
  where
  kinds =
    [ { k: "s-notes", label: signalLabel Notes }
    , { k: "control", label: "control" }
    , { k: "s-socket", label: signalLabel Socket }
    , { k: "s-midi", label: signalLabel Midi }
    , { k: "s-osc", label: signalLabel Osc }
    , { k: "s-http", label: signalLabel Http }
    , { k: "s-cv", label: signalLabel Cv }
    , { k: "s-audio", label: signalLabel Audio }
    , { k: "s-samples", label: signalLabel Samples }
    , { k: "s-record", label: signalLabel Recorded }
    , { k: "s-quant", label: signalLabel Quantise }
    ]
  item { k, label: name } =
    let off = Array.elem k st.hidden
    in
      HH.button
        [ HP.class_ (HH.ClassName ("sig " <> k <> (if off then " off" else "")))
        , HP.attr (AttrName "aria-pressed") (if off then "false" else "true")
        , HP.title (if off then "Show " <> name <> " lines" else "Hide " <> name <> " lines")
        , HE.onClick \_ -> on.toggle k
        , HE.onMouseEnter \_ -> on.hover (Just k)
        , HE.onMouseLeave \_ -> on.hover Nothing
        ]
        [ HH.i_ [], HH.text name ]

sigClass :: Signal -> String
sigClass = case _ of
  Notes -> "s-notes"
  Socket -> "s-socket"
  Midi -> "s-midi"
  Osc -> "s-osc"
  Http -> "s-http"
  Cv -> "s-cv"
  Audio -> "s-audio"
  Samples -> "s-samples"
  Recorded -> "s-record"
  Quantise -> "s-quant"

-- ---------------------------------------------------------------------------
-- SVG
-- ---------------------------------------------------------------------------

svg :: forall r w i. String -> Array (HP.IProp r i) -> Array (HH.HTML w i) -> HH.HTML w i
svg name = HH.elementNS (Namespace "http://www.w3.org/2000/svg") (ElemName name)

attr :: forall r i. String -> String -> HP.IProp r i
attr k = HP.attr (AttrName k)

use :: forall w i. String -> Number -> Number -> Number -> Number -> HH.HTML w i
use id x y w h' = svg "use" [ attr "href" ("#" <> id), attr "x" (n x), attr "y" (n y), attr "width" (n w), attr "height" (n h') ] []

label :: forall w i. String -> Number -> Number -> String -> String -> HH.HTML w i
label cls x y anchor s = svg "text" [ attr "class" cls, attr "x" (n x), attr "y" (n y), attr "text-anchor" anchor ] [ HH.text s ]

n :: Number -> String
n = toStringWith (fixed 1)

round' :: Number -> Int
round' x = fromMaybe 0 (fromNumber (Number.round x))

plural :: Int -> String -> String
plural k word = show k <> " " <> word <> (if k == 1 then "" else "s")
