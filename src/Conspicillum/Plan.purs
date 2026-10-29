-- | **One cycle, as the panel draws it.** The engine's own plan for the cycle
-- | (reef's `cycleOf`, the function the rig runs), with each grain placed on
-- | all three scales: the ring (when), the strip (where in the material) and
-- | the lanes (its read span, packed so none hides another).
module Conspicillum.Plan
  ( Grain
  , Plan
  , emptyPlan
  , sceneOf
  , cycleSeconds
  , planCycle
  , stripWidth
  ) where

import Prelude

import Data.Array (length, zipWith)
import Data.Foldable (maximum)
import Data.Maybe (fromMaybe, maybe)
import Conspicillum.Corpora (SampleSet, corpusFor)
import Reef.Conspicillum.Cloud (Emit, cycleOf)
import Reef.Conspicillum.Corpus (Query)
import Reef.Conspicillum.Display (Location, Material, colourAt, colourCss, lanes, locate, wedges)
import Reef.Conspicillum.Notation (Line)
import Reef.Conspicillum.Protocol (Scene)

type Grain =
  { emit :: Emit
  -- | On the ring, as fractions of the cycle: the onset, the end of the wedge
  -- | (the next onset at most), and how long it sounds.
  , from :: Number
  , to :: Number
  , sounding :: Number
  , colour :: String
  , reads :: { from :: Location, to :: Location }
  , lane :: Int
  }

type Plan = { cycle :: Int, grains :: Array Grain, lanes :: Int }

emptyPlan :: Plan
emptyPlan = { cycle: -1, grains: [], lanes: 1 }

-- | The strip's drawing width, in its own units. A read span is at least two
-- | of them wide, so a grain is never invisible.
stripWidth :: Number
stripWidth = 784.0

-- | A bar at the rig's tempo.
cycleSeconds :: Number -> Number
cycleSeconds tempo = 240.0 / tempo

-- | The scene as the rig is sent it. The warp ratio is the rig's tempo against
-- | the tape's, so a new tempo is a new scene.
sceneOf :: { set :: SampleSet, line :: Line, query :: Query, tempo :: Number } -> Scene
sceneOf { set, line, query, tempo } =
  { corpus: corpusFor set line
  , query
  , spec: line.spec { warp { ratio = tempo / maybe 120.0 _.bpm set.tape } }
  , seed: line.seed
  }

planCycle :: { scene :: Scene, material :: Material, tempo :: Number } -> Int -> Plan
planCycle { scene, material, tempo } cycle =
  let
    emits = cycleOf scene.corpus scene.query scene.spec scene.seed cycle
    seconds = cycleSeconds tempo
    placed = wedges { cycleSeconds: seconds, orbit: scene.spec.chain.orbit } emits <#> \w ->
      let
        start = locate material w.emit.n w.emit.begin
      in
        { emit: w.emit
        , from: w.from
        , to: w.to
        , sounding: max 0.004 (w.emit.sustain / seconds)
        , colour: colourCss (colourAt material start)
        , reads: { from: start, to: locate material w.emit.n w.emit.end }
        }
    laneOf = lanes (2.0 / stripWidth) (placed <#> \g -> { from: g.reads.from.along, to: g.reads.to.along })
    grains = zipWith (\g lane -> { emit: g.emit, from: g.from, to: g.to, sounding: g.sounding, colour: g.colour, reads: g.reads, lane }) placed laneOf
  in
    { cycle
    , grains
    , lanes: if length grains == 0 then 1 else 1 + fromMaybe 0 (maximum (grains <#> _.lane))
    }
