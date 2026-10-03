-- | **The signal-flow chart's data, checked without drawing it.**
-- |
-- | The chart only explains anything if it is true, and the ways it can be
-- | false are quiet: a path through the wrong column, a stream counted twice,
-- | a leg drawn in Solo that only the rig can play. Each case here is one
-- | configuration and the shape it must produce.
module Test.FlowSpec (runFlowTests) where

import Prelude

import Data.Array (all, filter, find, length, null)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert')
import Triggerfish.Flow (Inputs, Signal(..), flow, layerOf, onTheBeat)
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
