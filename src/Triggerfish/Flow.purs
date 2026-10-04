-- | `Triggerfish.Flow` — the signal-flow chart's data: every way a machine's
-- | output travels to the ears, read from the routing table rather than drawn.
-- |
-- | The chart is the landing page, the rig's dashboard and the explanation at
-- | once (`docs/kb/plans/dashboard.md`, "One chart, three jobs"), and that only
-- | works if **what you are not using is not drawn**. Nothing here is chosen:
-- | the machines are the ones whose pages are open, the destinations are the
-- | table's live legs, and the mode decides which path each leg takes. One
-- | sequencer playing Ableton in Solo comes out as five nodes in a line.
-- |
-- | ## The path is the architecture
-- |
-- | Every machine is a page, so every stream starts in the browser. What
-- | happens next depends on who plays:
-- |
-- | - **The page plays** (Solo, and the machines the rig does not play): MIDI
-- |   leaves the browser through Web MIDI, straight to its port.
-- | - **The rig plays** (Atlantis, for Odonus, Vetula and Balistes): the page
-- |   tells Architeuthis what to play (its patch, edits and cues) over the rig
-- |   socket, and the rig makes the notes. Those links are **control**, drawn
-- |   thin whatever they carry; the streams start at the engine (AC,
-- |   2026-10-03: "make the Sankey honest about data flows").
-- | - **Rig loops** play on the rig whether or not their page is open, so a
-- |   machine with a loop playing and its page closed is drawn from the engine.
-- |   The marks themselves are bubbles under the engine (the view's).
-- |
-- | Diaphus carries the rig's MIDI (Architeuthis hands it `/midi/note/at` over
-- | OSC for timestamped CoreMIDI delivery), but what it means to a reader is
-- | the beat: Link, broadcast to everything the rig times. So the chart draws
-- | it above the flow, reaching the nodes in `onTheBeat`, and puts it on the
-- | MIDI path only when asked (`relays`), for the documentary view.
-- |
-- | Samples and the ES-9 are always reached through the rig. In Solo such a
-- | leg cannot sound, but it is still drawn, on the path it would take, and
-- | marked as needing the rig (`waiting`): an open Conspicillum that left the
-- | chart unchanged read as broken rather than as "switch to Atlantis".
-- |
-- | ## Width is streams, not legs
-- |
-- | A link's value is the number of distinct WIRES it carries, not of table
-- | rows. Sixteen drum lanes all sending notes to one port on channel 10 are
-- | one stream; four Odonus heads on channels 1–4 are four. So a machine is as
-- | wide as the number of things it actually drives, which is what a reader
-- | wants the width to mean.
module Triggerfish.Flow
  ( Column(..)
  , columnTitle
  , Signal(..)
  , signalLabel
  , Node
  , Link
  , Flow
  , Inputs
  , Extra
  , RigLoop
  , Quant
  , flow
  , layerOf
  , onTheBeat
  , nodeRank
  , machineOf
  , loopOf
  ) where

import Prelude

import Data.Array (all, catMaybes, concatMap, elem, filter, findIndex, foldl, length, mapMaybe, nub, nubByEq, null, sort, sortWith, uncons, (!!))
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Int as Int
import Data.String (Pattern(..), contains, joinWith, split, stripPrefix)
import Data.Tuple (Tuple(..))
import Triggerfish.Routing.Model (Destination(..), Ports, Reach(..), Source(..), Table, instrumentLabel, reachOf, sourceKey, sourceLabel)
import Triggerfish.Transport (Mode(..))
import Reef.Balistes.Kit (canonKit)

-- ---------------------------------------------------------------------------
-- Columns and signals
-- ---------------------------------------------------------------------------

-- | Where a node stands, left to right. Fixed, so the chart reads the same in
-- | every configuration; columns with nothing in them are packed away by
-- | `layerOf`.
data Column = Feeders | Machines | Page | Loops | Engine | RigOut | Interface | Instrument | Heard

derive instance Eq Column
derive instance Ord Column

columnTitle :: Column -> String
columnTitle = case _ of
  Feeders -> "Feeds"
  Machines -> "Machine"
  Page -> "Page"
  Loops -> "Loop"
  Engine -> "Engine"
  RigOut -> "Rig out"
  Interface -> "Interface"
  Instrument -> "Instrument"
  Heard -> "Heard"

-- | What is travelling on a link. The colour changes where the signal changes
-- | form, which is the architecture made visible.
data Signal = Notes | Socket | Midi | Osc | Http | Cv | Audio | Samples | Recorded | Quantise

derive instance Eq Signal

signalLabel :: Signal -> String
signalLabel = case _ of
  Notes -> "note events"
  Socket -> "rig socket"
  Midi -> "MIDI"
  Osc -> "OSC"
  Http -> "HTTP"
  Cv -> "CV / gate"
  Audio -> "audio"
  Samples -> "samples"
  Recorded -> "recorded"
  Quantise -> "quantisation"

-- ---------------------------------------------------------------------------
-- The chart's data
-- ---------------------------------------------------------------------------

-- | `machine` is set on a machine's own node (or a voice's, when opened), so
-- | the view can draw its fish and link to its page.
type Node = { id :: String, column :: Column, name :: String, note :: String, machine :: Maybe String }

-- | One hop of one machine's streams, merged across that machine's sources.
-- | `broken` counts the streams on it whose port is missing: they are drawn,
-- | because the table says they should sound, and the view marks them.
-- | `wires` says which (`ch 10`, `gate 3`, a sample set) and `notes` which
-- | drum lanes ride them (`BD 36`), for the hover and the port labels.
type Link =
  { from :: String, to :: String, signal :: Signal, machine :: String, streams :: Int, broken :: Int
  , wires :: Array String, notes :: Array String
  -- | how many of `streams` would sound only with the rig (in Solo)
  , waiting :: Int
  -- | the page telling the rig what to play (its patch, edits and cues), not
  -- | the notes themselves: drawn thin, whatever it carries
  , control :: Boolean
  }

type Flow = { nodes :: Array Node, links :: Array Link }

-- | What feeds one of a machine's harmony inputs (`routing/harmony`): today
-- | Odonus's grid and out. `machine` when another machine feeds it (Vetula's
-- | key, or a voice's chords), drawn from that machine, which then stands
-- | upstream; else a source of its own (a scale, a harmony pattern).
type Quant = { target :: String, input :: String, machine :: Maybe String, label :: String }

-- | A mark the rig keeps for a machine (`rig_loops`), and whether a loop is
-- | playing it. A loop plays on the rig whether or not its page is open.
type RigLoop = { machine :: String, n :: Int, playing :: Boolean }

-- | A route that is not in the routing table: Conspicillum and Quadrat drive
-- | their destinations themselves. `via` is set when the page reaches it
-- | through a server of its own rather than through the rig (Quadrat's CV goes
-- | through the Friends server's `/api/cv`).
type Extra = { machine :: String, dest :: Destination, via :: Maybe String }

type Inputs =
  { mode :: Mode
  , table :: Table
  , ports :: Ports
  , machines :: Array String   -- whose pages are open
  , open :: Array String       -- machines drawn as their separate voices
  , extras :: Array Extra
  -- | draw the relays a reader rarely needs: Diaphus, which delivers every
  -- | MIDI note the rig sends, timestamped
  , relays :: Boolean
  , loops :: Array RigLoop
  , rigUp :: Boolean
  -- | nodes whose daemon Bosun says is down: a stream through one is broken
  -- | there. "diaphus" breaks every hop of the rig's MIDI, drawn or not.
  , down :: Array String
  , quantise :: Array Quant
  -- | machines the rig says it is playing (the stage) whose page is closed:
  -- | drawn from the engine, as a closed page's loops are
  , rigOnly :: Array String
  }

-- | The machine a source belongs to, by its slot name.
machineOf :: Source -> String
machineOf = case _ of
  SOdonusHead _ -> "odonus"
  SDrumLane _ -> "balistes"
  SVetulaVoice _ -> "vetula"
  SSeleneBank _ -> "selene"

-- | The machines purerl-tidal plays in Atlantis. Selene and Quadrat stay in
-- | the browser; Conspicillum's samples go through the rig regardless.
rigPlays :: String -> Boolean
rigPlays m = m `elem` [ "odonus", "vetula", "balistes", "limulus" ]

-- | Limulus always plays through the rig, whatever the mode: it sends its
-- | lines to purerl-tidal even when the machines' pages play themselves.
modeFor :: Mode -> String -> Mode
modeFor mode m = if m == "limulus" then Atlantis else mode

-- ---------------------------------------------------------------------------
-- From a leg to a path
-- ---------------------------------------------------------------------------

type Hop = { from :: String, to :: String, signal :: Signal, control :: Boolean }

-- | One stream: who emits it, which wire it is, and the path it takes.
-- | `brokenAt` names the node it cannot reach (a port that is not there), so
-- | only the hop into it is marked, not the whole way from the machine.
-- | `detail` is the wire as a reader names it, `notes` the drum lanes on it.
type Stream =
  { machine :: String, unit :: String, wire :: String, hops :: Array Hop, brokenAt :: Maybe String
  , detail :: String, notes :: Array String
  , needsRig :: Boolean
  }

-- | The wire a destination drives. Two legs on one wire are one stream.
wireOf :: Destination -> String
wireOf = case _ of
  DMidi d -> "midi:" <> d.port <> ":" <> show d.channel
  DFh2Env d -> "fh2env:" <> show d.slot
  DFh2Gate d -> "fh2gate:" <> show d.jack
  DEs9Gate d -> "es9gate:" <> show d.block <> ":" <> show d.jack
  DEs9Cv d -> "es9cv:" <> show d.bus
  DContinuo d -> "continuo:" <> show d.channel
  DRample d -> "rample:" <> d.port <> ":" <> show d.channel <> ":" <> show d.voice
  DRamplePoly d -> "rample:" <> d.port <> ":" <> show d.channel
  DPoly d -> "poly:" <> instrumentLabel d.inst
  DSample _ -> "dirt"

-- | The wire as a reader names it, beside its port or interface.
detailOf :: Destination -> String
detailOf = case _ of
  DMidi d -> "ch " <> show d.channel
  DFh2Env d -> "env " <> show d.slot
  DFh2Gate d -> "gate " <> show d.jack
  DEs9Gate d -> "gate " <> show (d.block + 1) <> "." <> show d.jack
  DEs9Cv d -> "bus " <> show d.bus
  DContinuo d -> "ch " <> show d.channel
  DRample d -> "ch " <> show d.channel <> " voice " <> show d.voice
  DRamplePoly d -> "ch " <> show d.channel
  DPoly d -> instrumentLabel d.inst
  DSample d -> if d.set == "" then "samples" else d.set

-- | A drum lane as its name and note (`BD 36`); nothing for other sources.
notesOf :: Source -> Array String
notesOf = case _ of
  SDrumLane i -> maybe [] (\k -> [ k.name <> " " <> show k.note ]) (canonKit !! i)
  _ -> []

-- | Numbered wires as runs per kind (`ch 1–4, 10 · gate 1–4`), in the order
-- | the kinds first appear; any other wire (a sample set) as it is.
compactWires :: Array String -> String
compactWires ws = joinWith " · " (map one kinds)
  where
  numbered w = case split (Pattern " ") w of
    [ k, v ] | Just n <- Int.fromString v -> Just { k, n }
    _ -> Nothing
  kinds = nub (map (\w -> maybe w _.k (numbered w)) ws)
  one k = case mapMaybe (\w -> numbered w >>= \x -> if x.k == k then Just x.n else Nothing) ws of
    [] -> k
    ns -> k <> " " <> joinWith ", " (runs (sort (nub ns)))
  runs xs = case uncons xs of
    Nothing -> []
    Just { head, tail } ->
      let
        go lo hi rest = case uncons rest of
          Just { head: x, tail: more } | x == hi + 1 -> go lo x more
          _ -> [ if lo == hi then show lo else show lo <> "–" <> show hi ] <> runs rest
      in go head head tail

-- | The interface a MIDI-borne destination leaves by, and the instrument at
-- | the far end of it. A port nobody has named an instrument for is its own
-- | instrument, by name, rather than a guess.
type Ends = { iface :: String, inst :: String, last :: Signal }

midiEnds :: Destination -> Maybe Ends
midiEnds = case _ of
  DMidi d -> Just (portEnds d.port)
  DFh2Env _ -> Just { iface: "fh2", inst: "modular", last: Cv }
  DFh2Gate _ -> Just { iface: "fh2", inst: "modular", last: Cv }
  DContinuo _ -> Just { iface: "continuo", inst: "piano", last: Midi }
  DRample d -> Just { iface: "port:" <> d.port, inst: "rample", last: Midi }
  DRamplePoly d -> Just { iface: "port:" <> d.port, inst: "rample", last: Midi }
  _ -> Nothing
  where
  portEnds p
    | contains (Pattern "IAC") p = { iface: "port:" <> p, inst: "ableton", last: Midi }
    | otherwise = { iface: "port:" <> p, inst: "inst:" <> p, last: Midi }

-- | The path one leg takes, or `Nothing` when it cannot sound in this mode
-- | (or when nothing sends to it yet).
pathOf :: Boolean -> Mode -> String -> Maybe String -> Destination -> Maybe (Array Hop)
pathOf relays mode m via dest = case midiEnds dest of
  Just e ->
    Just $ head <> [ hop e.iface e.inst e.last, hop e.inst "ears" Audio ]
    where
    head
      | atlantis && rigPlays m =
          [ ctl "browser" "engine" Socket ] <> rigMidi e.iface
      | otherwise = [ hop "browser" e.iface Midi ]
  Nothing -> case dest of
    DSample _ | atlantis ->
      Just [ ctl "browser" "engine" Socket, hop "engine" "d-dirt" Osc, hop "d-dirt" "ears" Audio ]
    DPoly _ | atlantis -> Just (toEs9 relay)
    -- Nothing sends a plain ES-9 leg from the table yet (`reachOf` says
    -- `NotBuilt`); Quadrat's own CV, through the Friends server, is built.
    DEs9Cv _ | atlantis, Just _ <- via -> Just (toEs9 relay)
    DEs9Gate _ | atlantis, Just _ <- via -> Just (toEs9 relay)
    _ -> Nothing
  where
  atlantis = mode == Atlantis
  hop from to signal = { from, to, signal, control: false }
  -- The page tells the rig what to play; the rig makes the notes.
  ctl from to signal = { from, to, signal, control: true }
  -- The rig's MIDI: handed to Diaphus as OSC for timestamped delivery.
  rigMidi iface
    | relays = [ hop "engine" "diaphus" Osc, hop "diaphus" iface Midi ]
    | otherwise = [ hop "engine" iface Midi ]
  relay = case via of
    Just v -> [ hop "browser" v Http, hop v "d-es9" Osc ]
    -- a machine the rig plays (Odonus's poly legs) is told; a page that
    -- times its own CV (Binnacle) sends it, and the rig only relays
    Nothing
      | rigPlays m -> [ ctl "browser" "engine" Socket, hop "engine" "d-es9" Osc ]
      | otherwise -> [ hop "browser" "engine" Socket, hop "engine" "d-es9" Osc ]
  toEs9 r = r <> [ hop "d-es9" "es9" Cv, hop "es9" "modular" Cv, hop "modular" "ears" Audio ]

-- ---------------------------------------------------------------------------
-- The flow
-- ---------------------------------------------------------------------------

flow :: Inputs -> Flow
flow inp = { nodes, links: links <> loopLinks <> quantLinks <> makesLinks }
  where
  pageOpen m = m `elem` inp.machines
  -- The rig's marks, while the rig is there to keep them.
  marks m = if inp.rigUp then filter (\l -> l.machine == m) inp.loops else []
  loopsPlay m = Array.any _.playing (marks m) || (inp.rigUp && m `elem` inp.rigOnly && not (pageOpen m))
  -- Whether the page's own streams already run through the engine.
  rigCarries m = pageOpen m && modeFor inp.mode m == Atlantis && rigPlays m
  unitOf m src = if m `elem` inp.open then "src:" <> sourceKey src else "m:" <> m

  tableStreams = inp.table # concatMap \r ->
    let
      m = machineOf r.source
      legs = filter _.on r.legs
      page = if pageOpen m then mapMaybe (\leg -> stream m (unitOf m r.source) Nothing (notesOf r.source) leg.dest) legs else []
      -- A loop plays on the rig whatever the page does (or with it closed):
      -- down the rig's paths, from the engine.
      fromLoops =
        if loopsPlay m && not (rigCarries m) then mapMaybe (\leg -> loopStream m (notesOf r.source) leg.dest) legs else []
    in page <> fromLoops

  extraStreams = inp.extras # mapMaybe \e ->
    if pageOpen e.machine then stream e.machine ("m:" <> e.machine) e.via [] e.dest
    else if loopsPlay e.machine && e.via == Nothing then loopStream e.machine [] e.dest
    else Nothing

  loopStream m notes dest =
    (\hops -> mk m ("rig:" <> m) notes dest (filter (\h -> h.from /= "browser") hops) false) <$> pathOf inp.relays Atlantis m Nothing dest

  -- A leg with no path in Solo that has one in Atlantis needs the rig: drawn
  -- on that path, marked waiting.
  stream m unit via notes dest =
    case pathOf inp.relays (modeFor inp.mode m) m via dest of
      Just hops -> Just (mk m unit notes dest hops false)
      Nothing | inp.mode /= Atlantis -> (\hops -> mk m unit notes dest hops true) <$> pathOf inp.relays Atlantis m via dest
      Nothing -> Nothing

  mk m unit notes dest hops needsRig =
      { machine: m, unit, wire: wireOf dest, hops
      , brokenAt: case Array.find dead (if needsRig then [] else hops) of
          Just h -> Just h.to
          Nothing -> if isNoPort (reachOf inp.ports dest) then _.iface <$> midiEnds dest else Nothing
      , detail: detailOf dest, notes, needsRig
      }

  -- A hop into a daemon that is down, or one of the rig's MIDI hops while
  -- Diaphus, which delivers them, is down.
  dead h = h.to `elem` inp.down
    -- the page does not reach the rig at all
    || (h.to == "engine" && not inp.rigUp)
    || ("diaphus" `elem` inp.down && h.from == "engine" && h.signal == Midi)

  -- Two legs on one wire from one unit are one stream; the drum lanes riding
  -- it are gathered, so the hover can say which.
  streams :: Array Stream
  streams = foldl gather [] (tableStreams <> extraStreams)
    where
    gather acc x = case findIndex (\a -> a.unit == x.unit && a.wire == x.wire) acc of
      Just i -> fromMaybe acc (Array.modifyAt i (\a -> a { notes = a.notes <> x.notes }) acc)
      Nothing -> Array.snoc acc x

  -- The first hop of every stream is its unit into the page: control too
  -- when the page only tells the rig. A stream the rig plays from its loops,
  -- with the page closed, starts at the engine.
  hopsOf s
    | Array.any (\h -> h.from == "browser") s.hops =
        [ { from: s.unit, to: "browser", signal: Notes, control: Array.any _.control s.hops } ] <> s.hops
    | otherwise = s.hops

  -- Sample sets feed SuperDirt whenever anything plays a sample: the material
  -- is part of the path even though no machine sends it.
  sampleUsers = filter (\s -> s.wire == "dirt") streams
  sampleStreams = length sampleUsers
  setsLinks
    | sampleStreams > 0 =
        [ { from: "sets", to: "d-dirt", signal: Samples, machine: "sets", streams: sampleStreams, broken: 0, wires: [], notes: [], waiting: if all _.needsRig sampleUsers then sampleStreams else 0, control: false } ]
    | otherwise = []

  links = merge (concatMap (\s -> hopsOf s <#> \h -> { hop: h, machine: s.machine, broken: s.brokenAt == Just h.to, detail: s.detail, notes: s.notes, waiting: s.needsRig }) streams) <> setsLinks

  merge = foldl add []
    where
    add acc x = case findIndex (same x) acc of
      Just i -> fromMaybe acc (Array.modifyAt i (\l -> l { streams = l.streams + 1, broken = l.broken + fromBool x.broken, wires = nub (Array.snoc l.wires x.detail), notes = nub (l.notes <> x.notes), waiting = l.waiting + fromBool x.waiting }) acc)
      Nothing -> Array.snoc acc { from: x.hop.from, to: x.hop.to, signal: x.hop.signal, machine: x.machine, streams: 1, broken: fromBool x.broken, wires: [ x.detail ], notes: x.notes, waiting: fromBool x.waiting, control: x.hop.control }
    same x l = l.from == x.hop.from && l.to == x.hop.to && l.signal == x.hop.signal && l.machine == x.machine && l.control == x.hop.control
    fromBool b = if b then 1 else 0

  -- The loops, between the pages and the engine: each mark fed, thin, by
  -- what recorded it (the machine, or each of its voices when opened), and a
  -- playing one feeding the engine with its notes.
  loopLinks = nub (map _.machine inp.loops) # concatMap \m ->
    let
      feeders
        | not (pageOpen m) = []
        | m `elem` inp.open = filter (\u -> isJust (stripPrefix (Pattern "src:") u)) (map _.unit (filter (\s -> s.machine == m) streams))
        | otherwise = [ "m:" <> m ]
    in
      marks m # concatMap \l ->
        let id = loopId m l.n
        in map (\f -> plain f id Recorded m true) feeders
             <> (if l.playing then [ plain id "engine" Notes m false ] else [])
  -- Quantisation: what feeds a machine's harmony inputs, into that machine
  -- (or each of its voices, opened). A machine feeding it is drawn upstream,
  -- in Feeds; a source with no page drawn (a scale, a pattern, or a machine
  -- closed or opened into voices) is a node of its own there.
  quantFrom q = case q.machine of
    Just m | pageOpen m && not (m `elem` inp.open) -> "m:" <> m
    Just m -> "q:" <> m <> " " <> q.label
    Nothing -> "q:" <> q.label
  quantTo t
    | t `elem` inp.open = filter (\u -> isJust (stripPrefix (Pattern "src:") u)) (map _.unit (filter (\x -> x.machine == t) streams))
    | Array.any (\x -> x.machine == t) streams = [ "m:" <> t ]
    | otherwise = []
  quantLinks = foldl addQ [] (inp.quantise # concatMap \q ->
    quantTo q.target <#> \to -> { from: quantFrom q, to, machine: fromMaybe q.target q.machine, input: q.input <> " ← " <> q.label })
    where
    addQ acc x = case findIndex (\l -> l.from == x.from && l.to == x.to) acc of
      Just i -> fromMaybe acc (Array.modifyAt i (\l -> l { streams = l.streams + 1, wires = nub (Array.snoc l.wires x.input) }) acc)
      Nothing -> Array.snoc acc ((plain x.from x.to Quantise x.machine false) { wires = [ x.input ] })
  feeders = nub (mapMaybe (\l -> stripPrefix (Pattern "m:") l.from) quantLinks)

  -- Quadrat makes sample sets: while its page is open, a line into them,
  -- whatever plays them.
  makesLinks
    | pageOpen "quadrat" = [ (plain "m:quadrat" "sets" Samples "quadrat" false) { wires = [ "makes sample sets" ] } ]
    | otherwise = []

  plain from to signal machine control =
    { from, to, signal, machine, streams: 1, broken: 0, wires: [], notes: [], waiting: 0, control }

  ids = nub (concatMap (\l -> [ l.from, l.to ]) (links <> loopLinks <> quantLinks <> makesLinks))
  units = nubByEq (\a b -> a.unit == b.unit) streams
  nodes = sortWith nodeRank (catMaybes (map (nodeOf units inp.table inp.loops feeders links) ids))

-- | A loop's node: one per mark, by machine and number.
loopId :: String -> Int -> String
loopId m n = "loop:" <> m <> ":" <> show n

loopOf :: String -> Maybe { machine :: String, n :: Int }
loopOf id = do
  rest <- stripPrefix (Pattern "loop:") id
  case split (Pattern ":") rest of
    [ m, k ] -> Int.fromString k <#> \n -> { machine: m, n }
    _ -> Nothing

isNoPort :: Reach -> Boolean
isNoPort = case _ of
  NoPort _ -> true
  _ -> false

-- | The fixed nodes, in their reading order within each column.
fixed :: Array Node
fixed =
  [ n "browser" Page "Browser" "every machine is a page"
  , n "engine" Engine "Architeuthis" "the rig's engine"
  , n "foi" Engine "Friends server" "Quadrat's CV relay"
  , n "sets" Engine "Sample sets" "Quadrat · Amphora"
  , n "diaphus" RigOut "Diaphus" "MIDI, on the beat"
  , n "d-es9" RigOut "es9-daemon" "CV over audio"
  , n "d-dirt" RigOut "SuperDirt" "plays samples"
  , n "continuo" Interface "continuo" "a MIDI port, hosted"
  , n "fh2" Interface "FH-2" "MIDI to CV and gates"
  , n "es9" Interface "ES-9" "audio to CV"
  , n "ableton" Instrument "Ableton" "instruments and effects"
  , n "piano" Instrument "Piano" "in Continuo"
  , n "rample" Instrument "Rample" "four sample voices"
  , n "modular" Instrument "The modular" "CV and gates"
  , n "ears" Heard "Your ears" "headphones · monitors"
  ]
  where
  n id column name note = { id, column, name, note, machine: Nothing }

machineNames :: Array { slot :: String, name :: String, note :: String }
machineNames =
  [ { slot: "odonus", name: "Odonus", note: "harmelodic ideas" }
  , { slot: "vetula", name: "Vetula", note: "progressions" }
  , { slot: "balistes", name: "Balistes", note: "drums" }
  , { slot: "selene", name: "Selene", note: "polysignals" }
  , { slot: "conspicillum", name: "Conspicillum", note: "sample loupe" }
  , { slot: "quadrat", name: "Quadrat", note: "sampling" }
  , { slot: "limulus", name: "Limulus", note: "Tidal, live-coded" }
  ]

nodeOf :: Array Stream -> Table -> Array RigLoop -> Array String -> Array Link -> String -> Maybe Node
nodeOf units table loops feeders links id = case Array.find (\x -> x.id == id) fixed of
  Just f -> Just (withWires f)
  Nothing
    | Just q <- strip "q:" -> Just { id, column: Feeders, name: q, note: "kept on the rig", machine: Nothing }
    | Just l <- loopOf id ->
        let playing = Array.any (\x -> x.machine == l.machine && x.n == l.n && x.playing) loops
        in Just { id, column: Loops, name: show l.n, note: if playing then "playing" else "kept", machine: Nothing }
    | Just p <- strip "port:" -> Just (withWires { id, column: Interface, name: p, note: "MIDI port", machine: Nothing })
    | Just p <- strip "inst:" -> Just { id, column: Instrument, name: p, note: "on its port", machine: Nothing }
    | Just m <- strip "m:" -> Just (machineNode m)
    | Just k <- strip "src:" -> (Array.find (\u -> u.unit == id) units) <#> \u ->
        { id, column: Machines, name: maybe k sourceLabel (sourceOf k), note: "", machine: Just u.machine }
    | otherwise -> Nothing
  where
  strip pre = stripPrefix (Pattern pre) id
  -- An interface says which of its wires are in use: a port its channels,
  -- the FH-2 its gates and envelopes.
  withWires nd
    | nd.column == Interface =
        let ws = concatMap _.wires (filter (\l -> l.to == id) links)
        in if null ws then nd else nd { note = compactWires ws }
    | otherwise = nd
  sourceOf k = _.source <$> Array.find (\r -> sourceKey r.source == k) table
  -- a machine that feeds another's harmony stands upstream of it
  machineNode m =
    let column = if m `elem` feeders then Feeders else Machines
    in case Array.find (\x -> x.slot == m) machineNames of
      Just x -> { id, column, name: x.name, note: x.note, machine: Just m }
      Nothing -> { id, column, name: m, note: "", machine: Just m }

-- | A node's place in reading order: machines in the dashboard's order,
-- | everything else in `fixed`'s, named ports and instruments after the fixed
-- | ones in their column. For the layout's `nodeSort`, so the chart holds
-- | still as nodes come and go.
nodeRank :: Node -> Tuple Column (Tuple Int String)
nodeRank nd = Tuple nd.column (Tuple at nd.name)
  where
  at = case nd.machine of
    Just m -> fromMaybe 99 (findIndex (\x -> x.slot == m) machineNames)
    -- loops by machine, then number
    Nothing | Just l <- loopOf nd.id -> 100 * fromMaybe 99 (findIndex (\x -> x.slot == l.machine) machineNames) + l.n
    -- A scale or pattern heads Feeds, above any machine there, whose
    -- ports hang under its label.
    Nothing | isJust (stripPrefix (Pattern "q:") nd.id) -> -2
    -- A named port heads its column: it is where most streams go.
    Nothing | isJust (stripPrefix (Pattern "port:") nd.id) -> -1
    Nothing -> fromMaybe 99 (findIndex (\x -> x.id == nd.id) fixed)

-- | Packed column numbers: the columns in use, numbered left to right. In
-- | Solo there is no rig, so its columns close up rather than leave a gap.
layerOf :: Flow -> String -> Maybe Int
layerOf f id = do
  nd <- Array.find (\x -> x.id == id) f.nodes
  findIndex (_ == nd.column) present
  where
  present = Array.sort (nub (map _.column f.nodes))

-- | The nodes the rig times to Link's beat: purerl-tidal, its daemons, and
-- | every port it sends MIDI to (through link-spike, timestamped). Empty in
-- | Solo, where each page keeps its own time.
onTheBeat :: Flow -> Array String
onTheBeat f = nub (concatMap rig f.links)
  where
  rig l
    | l.from == "engine" = [ "engine", l.to ]
    | l.from == "d-es9" || l.from == "d-dirt" || l.from == "diaphus" = [ l.from ]
    | otherwise = []
