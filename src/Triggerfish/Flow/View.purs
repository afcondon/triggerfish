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
  ) where

import Prelude

import Data.Array (filter, foldl, mapMaybe, nub, (!!))
import Data.Array as Array
import Data.Int (fromNumber, toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number as Number
import Data.String (joinWith)
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Data.Number.Format (fixed, toStringWith)
import DataViz.Layout.Sankey.Lanes (computeLayoutWithLanes, generateRoutePath, laneOf)
import DataViz.Layout.Sankey.Types (defaultSankeyConfig)
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..), ElemName(..), Namespace(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Triggerfish.Bosun (Lamp(..))
import Triggerfish.Flow (Column(..), Flow, Link, Signal(..), columnTitle, layerOf, loopOf, nodeRank, onTheBeat, signalLabel)

-- | What the chart reports: a machine hovered (or left), and a machine picked.
-- | `link`: a link clicked, with its machine and the node it runs into.
type Handlers i = { hover :: Maybe String -> i, pick :: String -> i, link :: String -> String -> i }

width :: Number
width = 1500.0

-- | A link's drawn width, in streams. Signal is its streams. Control is drawn
-- | at half its machine's streams (at least one): wide enough to read as the
-- | way in, grey so it does not read as notes (AC, 2026-10-03: pretty over
-- | strict here). A recorded line is thin.
widthOf :: Link -> Number
widthOf l
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

-- | What the chart shows of the moment. `playing` names the machines
-- | sounding now; the rest are drawn ghosted rather than dropped, since an
-- | open page that is stopped is still part of the picture, and when nothing
-- | plays the whole chart rests. `rigUp` is whether the page reaches
-- | purerl-tidal, shown on the kraken. `tempo` sets the beat's pulse.
-- | `lamps`: what Bosun says of the daemon behind a node, when it was reached.
type Live =
  { playing :: Array String, rigUp :: Boolean, tempo :: Number
  , lamps :: Array { node :: String, lamp :: Lamp, title :: String }
  -- | the key as a filter: kinds of line hidden (the chart is laid out
  -- | without them), and the kind hovered in the key (lit, the rest dimmed)
  , hidden :: Array String
  , keyHot :: Maybe String
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
chart on hot live = chartOf on hot live <<< hide live.hidden

chartOf :: forall w i. Handlers i -> Maybe String -> Live -> Flow -> HH.HTML w i
chartOf on hot live f
  | Array.null f.links =
      HH.p [ HP.class_ (HH.ClassName "flow-empty") ]
        [ HH.text if Array.null live.hidden then "Nothing is playing anywhere yet. Open a machine and the chart shows where it goes."
                  else "Every line drawn is of a kind hidden in the key. Click the key to show them again." ]
  | otherwise =
      svg "svg"
        [ attr "viewBox" ("0 0 " <> n width <> " " <> n h)
        , attr "class" ("flows" <> (if hot == Nothing then "" else " hovering") <> (if live.keyHot == Nothing then "" else " keying") <> (if Array.null live.playing then " resting" else ""))
        , attr "style" ("--beat: " <> n (60.0 / max 20.0 live.tempo) <> "s")
        , attr "role" "img"
        , attr "aria-label" "Where each machine's output goes: through the browser or the rig, through interfaces and instruments, to your ears"
        ]
        ( beatBand <> heads <> [ rule ] <> map link laid.routes <> map node laid.nodes <> beatMarks )
  where
  -- In Atlantis, link-spike stands above the flow: the beat, broadcast to
  -- everything the rig times, rather than one more hop in it.
  beat = onTheBeat f
  band = if Array.null beat then 0.0 else 58.0
  -- A machine fed from the left wears its label above its bar: room for it
  -- under the column heads.
  fedRoom = if Array.any (\l -> l.signal == Quantise) f.links then 46.0 else 0.0
  h = heightOf f + band + fedRoom
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
    Nothing -> map nodeRank (ours sn)
  laid = computeLayoutWithLanes wants
    (map (\l -> { s: l.from, t: l.to, v: widthOf l }) f.links)
    (defaultSankeyConfig width h)
      { nodeWidth = 5.0
      , nodePadding = 20.0
      , extent = { x0: left, y0: 48.0 + band + fedRoom, x1: right, y1: h - 40.0 }
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
                  [ use "ic-lantern" (cx - 30.0) 14.0 60.0 18.0
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

  linkCls l = sigClass l.signal <> (if l.control then " control" else "") <> (if live.keyHot == Just (kindOf l) then " khot" else "") <> (if Just l.machine == hot then " hot" else "") <> (if l.broken > 0 then " broken" else "") <> (if waits l then " waiting" else if sounding l then "" else " idle")
  link route =
    let
      ours' = f.links !! route.index
      cls = maybe "" linkCls ours'
    in
      svg "path" ([ attr "class" ("link " <> cls), attr "d" (generateRoutePath (laid.nodes <> laid.waypoints) route) ]
          <> maybe [] (\l -> [ HE.onClick \_ -> on.link l.machine l.to ]) ours')
        (maybe [] (\l -> [ svg "title" [] [ HH.text (linkTitle l) ] ]) ours')

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
    Just nd -> case nd.machine, loopOf nd.id of
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
    let top = sn.y0
    in
      svg "g"
        [ attr "class" "node pick inner", attr "tabindex" "0", attr "role" "button"
        , attr "aria-label" (nd.name <> ", " <> plural (streamsOf m) "stream")
        , HE.onMouseEnter \_ -> on.hover (Just m)
        , HE.onMouseLeave \_ -> on.hover Nothing
        , HE.onFocus \_ -> on.hover (Just m)
        , HE.onBlur \_ -> on.hover Nothing
        , HE.onClick \_ -> on.pick m
        ]
        [ svg "rect" [ attr "class" "hit", attr "x" (n (sn.x0 - 150.0)), attr "y" (n (top - 46.0)), attr "width" "160", attr "height" "46" ] []
        , bar sn
        , use ("sp-" <> m) (sn.x0 - 140.0) (top - 44.0) 54.0 32.0
        , label "name" (sn.x0 + 4.0) (top - 24.0) "end" nd.name
        , label "sub" (sn.x0 + 4.0) (top - 10.0) "end" (plural (streamsOf m) "stream" <> " · " <> (if needsAtlantis m then "needs Atlantis" else playsWhere m))
        ]

  besideMachine sn nd m =
    let cy = mid sn
    in
      svg "g"
        [ attr "class" "node pick inner", attr "tabindex" "0", attr "role" "button"
        , attr "aria-label" (nd.name <> ", " <> plural (streamsOf m) "stream")
        , HE.onMouseEnter \_ -> on.hover (Just m)
        , HE.onMouseLeave \_ -> on.hover Nothing
        , HE.onFocus \_ -> on.hover (Just m)
        , HE.onBlur \_ -> on.hover Nothing
        , HE.onClick \_ -> on.pick m
        ]
        [ svg "rect" [ attr "class" "hit", attr "x" (n (sn.x0 - 120.0)), attr "y" (n (cy - 44.0)), attr "width" "120", attr "height" "84" ] []
        , bar sn
        , use ("sp-" <> m) (sn.x0 - 62.0) (cy - 42.0) 54.0 32.0
        , label "name" (sn.x0 - 8.0) (cy + 2.0) "end" nd.name
        , label "sub" (sn.x0 - 8.0) (cy + 15.0) "end" (plural (streamsOf m) "stream")
        , label "sub where" (sn.x0 - 8.0) (cy + 27.0) "end" (if needsAtlantis m then "needs Atlantis" else playsWhere m)
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
        [ attr "class" "node pick", attr "tabindex" "0", attr "role" "button"
        , attr "aria-label" (nd.name <> ", " <> plural (round' sn.value) "stream")
        , HE.onMouseEnter \_ -> on.hover (Just m)
        , HE.onMouseLeave \_ -> on.hover Nothing
        , HE.onFocus \_ -> on.hover (Just m)
        , HE.onBlur \_ -> on.hover Nothing
        , HE.onClick \_ -> on.pick m
        ]
        -- A group catches the pointer only over what it paints, so the gap
        -- between the fish and the name needs something to land on.
        [ svg "rect" [ attr "class" "hit", attr "x" "10", attr "y" (n (cy - reach)), attr "width" (n (sn.x0 - 10.0)), attr "height" (n (2.0 * reach)) ] []
        , bar sn
        , use ("sp-" <> m) 23.0 (cy - 16.0) 54.0 32.0
        , label "name" (sn.x0 - 10.0) (cy - 2.0) "end" nd.name
        , label "sub" (sn.x0 - 10.0) (cy + 11.0) "end" (if needsAtlantis m then "needs Atlantis" else plural (streamsOf m) "stream")
        , label "sub where" (sn.x0 - 10.0) (cy + 23.0) "end" (if needsAtlantis m then "" else playsWhere m)
        ]

  -- A loop: a bubble in its machine's colour with the mark's number, filled
  -- and pulsing while the rig plays it.
  loopNode sn nd l =
    let
      cy = mid sn
      playing = nd.note == "playing"
      cx = sn.x1 + 14.0
    in
      svg "g" [ attr "class" ("node loopnode m-" <> l.machine) ] $
        [ bar sn
        , svg "g" [ attr "class" ("bubble" <> if playing then " playing" else "") ]
            -- the pulse is a halo behind the bubble, so the number stays solid
            ( (if playing then [ svg "circle" [ attr "class" "halo", attr "cx" (n cx), attr "cy" (n cy), attr "r" "9" ] [] ] else []) <>
            [ svg "circle" [ attr "class" "disc", attr "cx" (n cx), attr "cy" (n cy), attr "r" "9" ] []
            , label "bn" cx (cy + 3.5) "middle" nd.name
            ] )
        , svg "title" [] [ HH.text (l.machine <> " mark " <> nd.name <> (if playing then ": looping on the rig" else ": kept on the rig, not playing")
            <> (if silenced l.machine then "\nIt cannot be heard: every path out of the rig for " <> l.machine <> " is broken." else "")) ]
        ] <> (if playing && silenced l.machine then [ stopSign (cx + 18.0) (cy - 9.0) ] else [])

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
        _ -> []
      -- The kraken is drawn large, beside its label rather than above it (so
      -- it never climbs into the column heads). It says "the rig's engine"
      -- itself, so that line goes, and the label stays clear of the next
      -- column's even with all seven columns showing.
      lx = case nd.id of
        "ears" -> tx + 40.0
        "engine" -> tx + 68.0
        _ -> tx
      -- with loops beside it, the page's line is short, so it clears them
      sub
        | nd.id == "engine" = []
        | nd.id == "browser" && Array.any (\x -> x.column == Loops) f.nodes =
            [ label "sub" lx (cy + 11.0) "start" (show (round' sn.value)) ]
        | otherwise = [ label "sub" lx (cy + 11.0) "start" (nd.note <> " · " <> show (round' sn.value)) ]
    in
      svg "g" [ attr "class" "node" ]
        ( [ bar sn ] <> icon <> daemonLamp nd.id sn <>
            [ label "name" lx (cy - 2.0) "start" nd.name ]
            <> sub
            <> rigLamp nd.id lx cy
        )

  -- Bosun's word on the daemon behind a node: a lamp under its bar.
  daemonLamp id sn = lampAt id (sn.x0 + 2.5) (sn.y1 + 8.0)
  lampAt id x y = case Array.find (\l -> l.node == id) live.lamps of
    Nothing -> []
    Just l ->
      -- a link to the Atlantis page, where it can be restarted
      [ svg "a" [ attr "href" "#atlantis" ]
          [ svg "circle" [ attr "class" ("dlamp " <> lampClass l.lamp), attr "cx" (n x), attr "cy" (n y), attr "r" "4" ]
              [ svg "title" [] [ HH.text ("Bosun · " <> l.title <> " · open the Atlantis page") ] ]
          ]
      ]
  lampClass = case _ of
    Up -> "up"
    Coming -> "coming"
    Down -> "down"
  -- The rig's link, on the rig: a lamp under purerl-tidal's label.
  rigLamp id x cy
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
    | Array.any (\l -> l.machine == m && l.control && l.signal /= Recorded) f.links = "plays on the rig"
    | Array.any (\l -> l.machine == m && l.from == "engine") f.links = "loops on the rig"
    | otherwise = "plays here"

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
