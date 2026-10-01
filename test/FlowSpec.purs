-- | **The signal-flow chart's data, checked without drawing it.**
-- |
-- | The chart only explains anything if it is true, and the ways it can be
-- | false are quiet: a path through the wrong column, a stream counted twice,
-- | a leg drawn in Solo that only the rig can play. Each case here is one
-- | configuration and the shape it must produce.
module Test.FlowSpec (runFlowTests) where

import Prelude

import Data.Array (all, filter, find, length)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert')
import Triggerfish.Flow (Inputs, Signal(..), flow, layerOf)
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
  check "in Atlantis the page hands the rig its notes"
    (map _.id rig.nodes == [ "m:odonus", "browser", "engine", "linkspike", "port:IAC Driver Tidal", "ableton", "ears" ])
  check "purerl-tidal stands one column deeper than the page"
    (layerOf rig "browser" == Just 1 && layerOf rig "engine" == Just 2)
  check "Solo closes up the rig's columns"
    (layerOf one "port:IAC Driver Tidal" == Just 2)

  let
    sampled = [ { source: SDrumLane 0, legs: [ { dest: DSample { set: "kit", n: 0, begin: 0, end: 100, reverse: false, gain: 100, chop: 1 }, offsetMs: 0.0, on: true } ] } ]
    soloSamples = flow base { machines = [ "balistes" ], table = sampled }
    rigSamples = flow base { mode = Atlantis, machines = [ "balistes" ], table = sampled }
  check "a sample cannot sound in Solo, so it is not drawn" (length soloSamples.nodes == 0)
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
