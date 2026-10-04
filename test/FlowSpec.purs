-- | **The signal-flow chart's data, checked without drawing it.**
-- |
-- | The chart only explains anything if it is true, and the ways it can be
-- | false are quiet: a path through the wrong column, a stream counted twice,
-- | a leg drawn in Solo that only the rig can play. Each case here is one
-- | configuration and the shape it must produce.
module Test.FlowSpec (runFlowTests) where

import Prelude

import Data.Array (all, elem, filter, find, length, null)
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert')
import Triggerfish.Bosun as Bosun
import Triggerfish.Flow (Inputs, Signal(..), flow, layerOf, onTheBeat, skeleton)
import Triggerfish.Routing.Model (Destination(..), Source(..), Table, defaultTableFor)
import Triggerfish.Transport (Mode(..))

ports :: Array String
ports = [ "IAC Driver Tidal", "FH-2", "continuo" ]

base :: Inputs
base =
  { mode: Solo
  , table: defaultTableFor ports
  , ports: { found: ports, rigUp: true }
  , machines: [ "odonus" ]
  , open: []
  , extras: []
  , relays: false
  , loops: []
  , rigUp: true
  , down: []
  , quantise: []
  , rigOnly: []
  , polysignals: []
  }

-- | Odonus on the IAC bus alone: the base case of the reveal.
odonusToAbleton :: Table
odonusToAbleton = [ 0, 1, 2, 3 ] <#> \h ->
  { source: SOdonusHead h, legs: [ { dest: DMidi { port: "IAC Driver Tidal", channel: h + 1 }, offsetMs: 0.0, on: true } ] }

check :: String -> Boolean -> Effect Unit
check name ok = do
  assert' ("flow: " <> name) ok
  log ("  ✓ " <> name)

runFlowTests :: Effect Unit
runFlowTests = do
  log "\n--- Triggerfish.Flow — the chart's data ---"

  let one = flow base { table = odonusToAbleton }
  check "the base case is five nodes in a line"
    (map _.id one.nodes == [ "m:odonus", "browser", "port:IAC Driver Tidal", "ableton", "ears" ])
  check "four heads on four channels are four streams"
    (all (\l -> l.streams == 4) one.links && length one.links == 4)
  check "Solo sends MIDI straight from the page"
    ((find (\l -> l.from == "browser") one.links <#> _.signal) == Just Midi)

  let drums = flow base { machines = [ "balistes" ] }
  check "sixteen lanes on channel 10 are one stream, plus four FH-2 gates"
    ((find (\l -> l.to == "browser") drums.links <#> _.streams) == Just 5)

  let rig = flow base { mode = Atlantis, table = odonusToAbleton }
  check "in Atlantis the page tells the rig what to play"
    (map _.id rig.nodes == [ "m:odonus", "browser", "engine", "port:IAC Driver Tidal", "ableton", "ears" ])
  check "the rig times purerl-tidal and the ports it sends to; Solo has no beat"
    (onTheBeat rig == [ "engine", "port:IAC Driver Tidal" ] && onTheBeat one == [])
  check "purerl-tidal stands one column deeper than the page"
    (layerOf rig "browser" == Just 1 && layerOf rig "engine" == Just 2)
  check "in Atlantis the page's links are control; the notes start at the engine"
    ( all _.control (filter (\l -> l.to == "browser" || l.to == "engine") rig.links)
        && all (not <<< _.control) (filter (\l -> l.from == "engine") rig.links)
        && all (not <<< _.control) one.links
    )

  let relayed = flow base { mode = Atlantis, table = odonusToAbleton, relays = true }
  check "with relays drawn, the rig's MIDI goes through Diaphus"
    ( (find (\l -> l.from == "engine") relayed.links <#> _.to) == Just "diaphus"
        && (find (\l -> l.from == "diaphus") relayed.links <#> _.signal) == Just Midi
    )

  let looping = flow base { machines = [], table = odonusToAbleton, loops = [ { machine: "odonus", n: 1, playing: true } ] }
  check "a loop on the rig sounds with its page closed, from the engine"
    ( isJust (find (\l -> l.from == "engine" && l.machine == "odonus") looping.links)
        && isNothing (find (\l -> l.from == "browser" || l.to == "browser") looping.links)
    )
  let looped = flow base { mode = Atlantis, table = odonusToAbleton
                          , loops = [ { machine: "odonus", n: 1, playing: false }, { machine: "odonus", n: 2, playing: true } ] }
  check "a loop is a node between the page and the engine"
    ( layerOf looped "loop:odonus:2" == Just 2 && layerOf looped "engine" == Just 3
        && layerOf looped "browser" == Just 1
    )
  check "every mark is recorded from its machine; only a playing one feeds the engine"
    ( isJust (find (\l -> l.from == "m:odonus" && l.to == "loop:odonus:1" && l.signal == Recorded) looped.links)
        && isJust (find (\l -> l.from == "loop:odonus:2" && l.to == "engine") looped.links)
        && isNothing (find (\l -> l.from == "loop:odonus:1" && l.to == "engine") looped.links)
    )
  let soloLoop = flow base { table = odonusToAbleton, loops = [ { machine: "odonus", n: 1, playing: true } ] }
  check "in Solo the page plays here and a playing loop plays on the rig, both drawn"
    ( isJust (find (\l -> l.from == "browser" && l.signal == Midi) soloLoop.links)
        && isJust (find (\l -> l.from == "engine" && l.machine == "odonus") soloLoop.links)
    )

  check "a kept mark that is not playing draws no streams"
    (null (flow base { machines = [], table = odonusToAbleton, loops = [ { machine: "odonus", n: 1, playing: false } ] }).links)
  check "with the rig down a loop draws nothing"
    (null (flow base { machines = [], table = odonusToAbleton, rigUp = false, loops = [ { machine: "odonus", n: 1, playing: true } ] }).links)

  let noDiaphus = flow base { mode = Atlantis, table = odonusToAbleton, down = [ "diaphus" ] }
  check "with Diaphus down, the rig's MIDI is broken where it leaves the engine"
    ( (find (\l -> l.from == "engine") noDiaphus.links <#> _.broken) == Just 4
        && all (\l -> l.broken == 0) (filter (\l -> l.from /= "engine") noDiaphus.links)
    )
  check "in Solo, Diaphus down breaks nothing"
    (all (\l -> l.broken == 0) (flow base { table = odonusToAbleton, down = [ "diaphus" ] }).links)
  let noDirt = flow base { mode = Atlantis, machines = [ "balistes" ], down = [ "d-dirt" ]
                         , table = [ { source: SDrumLane 0, legs: [ { dest: DSample { set: "kit", n: 0, begin: 0, end: 100, reverse: false, gain: 100, chop: 1 }, offsetMs: 0.0, on: true } ] } ] }
  check "with SuperDirt down, a sample stream is broken going into it"
    ((find (\l -> l.to == "d-dirt" && l.from == "engine") noDirt.links <#> _.broken) == Just 1)

  let lonely = flow base
        { machines = [ "limulus" ], table = [], rigUp = false
        , extras = [ { machine: "limulus", dest: DMidi { port: "IAC Driver Tidal", channel: 10 }, via: Nothing } ] }
  check "with no rig, Limulus is broken where it reaches for the engine"
    ((find (\l -> l.to == "engine") lonely.links <#> _.broken) == Just 1)
  check "with no rig, a stream that waits for Atlantis is waiting, not broken"
    (all (\l -> l.broken == 0) (flow base { machines = [ "balistes" ], rigUp = false
        , table = [ { source: SDrumLane 0, legs: [ { dest: DSample { set: "kit", n: 0, begin: 0, end: 100, reverse: false, gain: 100, chop: 1 }, offsetMs: 0.0, on: true } ] } ] }).links)

  let
    vetulaToOdonus = [ { source: SVetulaVoice "", legs: [ { dest: DMidi { port: "IAC Driver Tidal", channel: 5 }, offsetMs: 0.0, on: true } ] } ] <> odonusToAbleton
    fed = flow base { machines = [ "odonus", "vetula" ], table = vetulaToOdonus
                    , quantise = [ { target: "odonus", input: "grid", machine: Just "vetula", label: "key" }
                                 , { target: "odonus", input: "out", machine: Just "vetula", label: "voice 3" } ] }
  check "Vetula feeding Odonus's harmony stands upstream of it, one ribbon for both inputs"
    ( layerOf fed "m:vetula" == Just 0 && layerOf fed "m:odonus" == Just 1
        && ((find (\l -> l.signal == Quantise) fed.links <#> \l -> [ l.from, l.to, show l.streams ]) == Just [ "m:vetula", "m:odonus", "2" ])
    )
  let scaled = flow base { table = odonusToAbleton, quantise = [ { target: "odonus", input: "grid", machine: Nothing, label: "scale dorian" } ] }
  check "a scale feeding Odonus is a node of its own, upstream"
    ( layerOf scaled "q:scale dorian" == Just 0
        && isJust (find (\l -> l.from == "q:scale dorian" && l.to == "m:odonus") scaled.links)
    )
  check "nothing is quantised when its machine is not drawn"
    (null (flow base { machines = [], table = odonusToAbleton, quantise = [ { target: "odonus", input: "grid", machine: Nothing, label: "scale dorian" } ] }).links)

  let quadratOpen = flow base { machines = [ "quadrat" ], table = [] }
  check "Quadrat, open, makes sample sets: a line into them"
    ((find (\l -> l.from == "m:quadrat") quadratOpen.links <#> \l -> [ l.to, l.machine ]) == Just [ "sets", "quadrat" ])

  let conspOnRig = flow base { machines = [], table = [], rigOnly = [ "conspicillum" ]
        , extras = [ { machine: "conspicillum", dest: DSample { set: "", n: 0, begin: 0, end: 100, reverse: false, gain: 100, chop: 1 }, via: Nothing } ] }
  check "a machine the rig plays with its page closed is drawn from the engine"
    ( isJust (find (\l -> l.from == "engine" && l.to == "d-dirt" && l.machine == "conspicillum") conspOnRig.links)
        && isNothing (find (\l -> l.to == "browser" || l.from == "browser") conspOnRig.links)
    )

  check "Solo closes up the rig's columns"
    (layerOf one "port:IAC Driver Tidal" == Just 2)

  let
    sampled = [ { source: SDrumLane 0, legs: [ { dest: DSample { set: "kit", n: 0, begin: 0, end: 100, reverse: false, gain: 100, chop: 1 }, offsetMs: 0.0, on: true } ] } ]
    soloSamples = flow base { machines = [ "balistes" ], table = sampled }
    rigSamples = flow base { mode = Atlantis, machines = [ "balistes" ], table = sampled }
  check "a sample cannot sound in Solo: drawn on the rig's path, every hop waiting"
    ( isJust (find (\l -> l.to == "d-dirt") soloSamples.links)
        && all (\l -> l.waiting == l.streams) soloSamples.links
    )
  check "in Atlantis nothing waits" (all (\l -> l.waiting == 0) rigSamples.links)

  let
    limulus = flow base
      { machines = [ "limulus" ], table = []
      , extras = [ { machine: "limulus", dest: DMidi { port: "IAC Driver Tidal", channel: 10 }, via: Nothing } ]
      }
  check "Limulus plays through the rig even in Solo, and nothing waits"
    ( isJust (find (\l -> l.from == "browser" && l.to == "engine") limulus.links)
        && all (\l -> l.waiting == 0) limulus.links
    )
  check "a port names its channels as runs"
    (map _.note (find (\nd -> nd.id == "port:IAC Driver Tidal") rig.nodes) == Just "ch 1–4")
  check "in Atlantis a sample goes through SuperDirt, fed by the sample sets"
    ( (find (\l -> l.from == "sets") rigSamples.links <#> _.signal) == Just Samples
        && (find (\l -> l.from == "d-dirt") rigSamples.links <#> _.to) == Just "ears"
    )

  let noFh2 = flow base { ports = { found: [ "IAC Driver Tidal" ], rigUp: false } }
  check "a missing port is drawn, and only the hop into it is broken"
    ( (find (\l -> l.to == "fh2") noFh2.links <#> _.broken) == Just 4
        && all (\l -> l.broken == 0) (filter (\l -> l.to /= "fh2") noFh2.links)
    )

  check "a machine whose page is closed is not drawn"
    (length (flow base { machines = [] }).nodes == 0)

  let opened = flow base { open = [ "odonus" ] }
  check "an open machine shows its voices"
    (length (filter (\n -> n.machine == Just "odonus") opened.nodes) == 4)

  let quadrat = flow base { mode = Atlantis, machines = [ "quadrat" ], extras = [ { machine: "quadrat", dest: DEs9Cv { bus: 1 }, via: Just "foi" } ] }
  check "Quadrat's CV goes through the Friends server"
    ((find (\l -> l.from == "browser") quadrat.links <#> _.to) == Just "foi")

  -- The X-ray (2026-10-04): every daemon has its node, whatever plays.
  let bare = skeleton (flow base { machines = [] })
  check "the X-ray's skeleton gives every daemon a node with nothing playing"
    (all (\id -> isJust (find (\nd -> nd.id == id) bare.nodes)) [ "browser", "engine", "foi", "sets", "diaphus", "d-es9", "d-dirt", "continuo", "fh2", "es9" ])
  let rigX = skeleton rigSamples
  check "a bone is added only where no line already runs"
    ( length (filter (\l -> l.from == "engine" && l.to == "d-dirt") rigX.links) == 1
        && all (\l -> not l.bone) (filter (\l -> l.from == "engine" && l.to == "d-dirt") rigX.links)
    )
  check "a bone is not on the beat"
    (null (onTheBeat bare))
  -- the Atlantis group as composed (bosun/fixtures/atlantis/compose.yml):
  -- a service added there needs a place here, or the X-ray lists it apart
  check "every Atlantis daemon has a place on the X-ray, or watches it"
    (all (\id -> not (null (Bosun.placesOf id)) || id `elem` Bosun.watchers)
      [ "es9-daemon", "diaphus", "architeuthis", "triggerfish-frontend", "conspicillum-frontend", "superdirt", "amphora", "friends-of-itajara", "limulus", "deepstar", "fh2-daemon", "fh2-drumkit", "continuo" ])
  check "every place a daemon stands is a node the skeleton draws, or a machine"
    (all (\id -> all (\pl -> isJust (find (\nd -> nd.id == pl) bare.nodes) || pl `elem` [ "m:quadrat", "m:conspicillum", "m:limulus", "ghci" ]) (Bosun.placesOf id))
      [ "es9-daemon", "diaphus", "architeuthis", "triggerfish-frontend", "superdirt", "amphora", "friends-of-itajara", "fh2-daemon", "fh2-drumkit", "continuo" ])

  -- Limulus sent to Haskell Tidal (2026-10-04): GHCi plays SuperDirt itself
  let ghci = flow base { machines = [ "limulus" ], table = []
        , extras = [ { machine: "limulus", dest: DSample { set: "d1–d16", n: 0, begin: 0, end: 100, reverse: false, gain: 100, chop: 1 }, via: Just "ghci" } ] }
  check "Limulus on Haskell Tidal goes through GHCi to SuperDirt, not the rig"
    ( isJust (find (\l -> l.from == "browser" && l.to == "ghci") ghci.links)
        && isJust (find (\l -> l.from == "ghci" && l.to == "d-dirt") ghci.links)
        && isNothing (find (\l -> l.to == "engine") ghci.links)
    )

  -- Selene's banks, as the rig keeps them (2026-10-04)
  let sel = flow base { mode = Atlantis, machines = [ "selene" ], table = []
        , polysignals = [ { socket: "es9", bank: "main", family: "polyeuclid", slots: 8 }, { socket: "fh2", bank: "main", family: "envelope", slots: 8 } ] }
  check "Selene's banks reach the modular through es9-daemon and the FH-2, one stream a bank"
    ( (find (\l -> l.from == "d-es9" && l.to == "es9") sel.links <#> _.streams) == Just 1
        && (find (\l -> l.from == "fh2" && l.to == "modular") sel.links <#> _.streams) == Just 1
        && (find (\l -> l.from == "d-es9") sel.links <#> _.wires) == Just [ "polyeuclid ×8" ]
        && all _.control (filter (\l -> l.to == "engine" || l.from == "engine") sel.links)
    )
  let selClosed = flow base { machines = [], table = [], polysignals = [ { socket: "fh2", bank: "main", family: "envelope", slots: 4 } ] }
  check "with Selene's page closed the rig keeps its banks: drawn from the engine"
    ( isNothing (find (\l -> l.to == "engine") selClosed.links)
        && maybe false _.control (find (\l -> l.from == "engine" && l.to == "fh2") selClosed.links)
    )
