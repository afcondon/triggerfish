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
  ) where

import Prelude

import Data.Array (filter, foldl, mapMaybe, nub, (!!))
import Data.Array as Array
import Data.Int (fromNumber, toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number as Number
import Data.String (joinWith)
import Data.Tuple.Nested ((/\))
import Data.Number.Format (fixed, toStringWith)
import DataViz.Layout.Sankey.Compute (computeLayoutWithConfig)
import DataViz.Layout.Sankey.Path (generateLinkPath)
import DataViz.Layout.Sankey.Types (LinkID(..), defaultSankeyConfig)
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..), ElemName(..), Namespace(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Triggerfish.Flow (Column(..), Flow, Signal(..), columnTitle, layerOf, nodeRank, onTheBeat, signalLabel)

-- | What the chart reports: a machine hovered (or left), and a machine picked.
-- | `link`: a link clicked, with its machine and the node it runs into.
type Handlers i = { hover :: Maybe String -> i, pick :: String -> i, link :: String -> String -> i }

width :: Number
width = 1500.0

-- | A control line's width, in streams: thin whatever it carries.
controlWidth :: Number
controlWidth = 0.3

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
type Live = { playing :: Array String, rigUp :: Boolean, tempo :: Number }

chart :: forall w i. Handlers i -> Maybe String -> Live -> Flow -> HH.HTML w i
chart on hot live f
  | Array.null f.links =
      HH.p [ HP.class_ (HH.ClassName "flow-empty") ]
        [ HH.text "Nothing is playing anywhere yet. Open a machine and the chart shows where it goes." ]
  | otherwise =
      svg "svg"
        [ attr "viewBox" ("0 0 " <> n width <> " " <> n h)
        , attr "class" ("flows" <> (if hot == Nothing then "" else " hovering") <> (if Array.null live.playing then " resting" else ""))
        , attr "style" ("--beat: " <> n (60.0 / max 20.0 live.tempo) <> "s")
        , attr "role" "img"
        , attr "aria-label" "Where each machine's output goes: through the browser or the rig, through interfaces and instruments, to your ears"
        ]
        ( beatBand <> heads <> [ rule ] <> map link laid.links <> map node laid.nodes <> beatMarks <> bubbles )
  where
  -- In Atlantis, link-spike stands above the flow: the beat, broadcast to
  -- everything the rig times, rather than one more hop in it.
  beat = onTheBeat f
  band = if Array.null beat then 0.0 else 58.0
  h = heightOf f + band
  byId = Map.fromFoldable (map (\x -> x.id /\ x) f.nodes)
  ours sn = Map.lookup sn.name byId
  rankOf sn = map nodeRank (ours sn)
  laid = computeLayoutWithConfig
    (map (\l -> { s: l.from, t: l.to, v: if l.control then controlWidth else toNumber l.streams }) f.links)
    (defaultSankeyConfig width h)
      { nodeWidth = 5.0
      , nodePadding = 20.0
      , extent = { x0: left, y0: 48.0 + band, x1: right, y1: h - 40.0 }
      , nodeLayer = layerOf f
      , nodeSort = Just (comparing rankOf)
      }

  heads = mapMaybe colHead (nub (map _.column f.nodes))
  colHead c = do
    x <- if c == Machines then Just 20.0 else
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
                  ]
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

  link sl =
    let
      ours' = f.links !! (unwrap' sl.index)
      cls = maybe "" (\l -> sigClass l.signal <> (if l.control then " control" else "") <> (if Just l.machine == hot then " hot" else "") <> (if l.broken > 0 then " broken" else "") <> (if waits l then " waiting" else if sounding l then "" else " idle")) ours'
    in
      svg "path" ([ attr "class" ("link " <> cls), attr "d" (generateLinkPath laid.nodes sl) ]
          <> maybe [] (\l -> [ HE.onClick \_ -> on.link l.machine l.to ]) ours')
        (maybe [] (\l -> [ svg "title" [] [ HH.text (linkTitle l) ] ]) ours')

  linkTitle l =
    l.from <> " → " <> l.to <> " · " <> signalLabel l.signal
      <> (if l.control then " · control: the page tells the rig what to play; the rig makes the notes" else " · " <> plural l.streams "stream")
      <> (if l.broken > 0 then " · " <> show l.broken <> " with no port" else "")
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
    Just nd -> case nd.machine of
      Just m -> machineNode sn nd m
      Nothing -> placeNode sn nd

  bar sn = svg "rect"
    [ attr "class" "bar", attr "x" (n sn.x0), attr "y" (n sn.y0)
    , attr "width" (n (sn.x1 - sn.x0)), attr "height" (n (max 2.0 (sn.y1 - sn.y0)))
    ] []

  mid sn = (sn.y0 + sn.y1) / 2.0

  machineNode sn nd m =
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
        , label "sub" (sn.x0 - 10.0) (cy + 11.0) "end" (if needsAtlantis m then "needs Atlantis" else plural (streamsOf m) "stream" <> " · " <> playsWhere m)
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
      sub
        | nd.id == "engine" = []
        | otherwise = [ label "sub" lx (cy + 11.0) "start" (nd.note <> " · " <> show (round' sn.value)) ]
    in
      svg "g" [ attr "class" "node" ]
        ( [ bar sn ] <> icon <>
            [ label "name" lx (cy - 2.0) "start" nd.name ]
            <> sub
            <> rigLamp nd.id lx cy
        )

  unwrap' (LinkID i) = i
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
    | Array.any (\l -> l.machine == m && l.control) f.links = "plays on the rig"
    | Array.any (\l -> l.machine == m && l.from == "engine") f.links = "loops on the rig"
    | otherwise = "plays here"

  -- The rig's loops, as bubbles under the engine: a row per machine, one
  -- per mark, numbered as Limulus numbers them, filled while a loop plays it.
  bubbles = case Array.find (\sn -> sn.name == "engine") laid.nodes of
    Nothing -> []
    Just sn ->
      let
        rows = nub (map _.machine f.loops)
        x0 = sn.x1 + 76.0
        row i m =
          let
            y = sn.y1 + 18.0 + toNumber i * 20.0
            marks = filter (\l -> l.machine == m) f.loops
          in
            svg "g" [ attr "class" ("loops m-" <> m) ]
              ( [ label "sub" (x0 - 8.0) (y + 3.5) "end" (m <> " loops") ]
                  <> Array.mapWithIndex (\j l -> bubble (x0 + toNumber j * 18.0) y l) marks
              )
        bubble x y l =
          svg "g" [ attr "class" ("bubble" <> (if l.playing then " playing" else "")) ]
            [ svg "circle" [ attr "cx" (n (x + 7.0)), attr "cy" (n y), attr "r" "7.5" ] []
            , label "bn" (x + 7.0) (y + 3.5) "middle" (show l.n)
            , svg "title" [] [ HH.text (m' l <> " mark " <> show l.n <> (if l.playing then ": looping on the rig" else ": kept, not playing") ) ]
            ]
        m' l = l.machine
      in Array.mapWithIndex row rows

  -- The sample sets sound whenever anything does.
  sounding l = l.machine `Array.elem` live.playing || (l.machine == "sets" && not (Array.null live.playing))

-- | The signals, as a key under the chart.
key :: forall w i. HH.HTML w i
key =
  HH.div [ HP.class_ (HH.ClassName "flow-key") ]
    ( [ Notes, Socket, Midi, Osc, Http, Cv, Audio, Samples ] <#> \s ->
        HH.span [ HP.class_ (HH.ClassName ("sig " <> sigClass s)) ] [ HH.i_ [], HH.text (signalLabel s) ]
    )

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
