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
  , reach
  , planCycle
  , stripWidth
  ) where

import Prelude

import Data.Array (filter, findMap, last, (!!), length, range, sortBy, zipWith)
import Data.Foldable (maximum)
import Data.Int (ceil, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Ord (abs)
import Conspicillum.Corpora (SampleSet, corpusFor)
import Conspicillum.Take as Take
import Reef.Conspicillum.Cloud (Emit, Play, cycleOfPlays)
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
-- | the tape's, so a new tempo is a new scene. A take (`Conspicillum.Take`,
-- | as notation) is stacked with the preset's own pattern here and only here,
-- | so it plays and is drawn without ever being part of the preset.
sceneOf :: { set :: SampleSet, line :: Line, query :: Query, tempo :: Number, gate :: Array Boolean, take :: String } -> Scene
sceneOf { set, line, query, tempo, gate, take } =
  { corpus: corpusFor set line
  , query
  , spec: line.spec
      { warp { ratio = tempo / maybe 120.0 _.bpm set.tape }
      , steps { gate = gate }
      , samplePattern = Take.stack line.spec.samplePattern take
      }
  , seed: line.seed
  }

-- | How many bars back a sample still sounding now may have begun: the
-- | longest sample at the scene's speed, in bars. None for a tape or a cloud,
-- | whose grains end within their bar. At most 48: the rig answers 64 cycles
-- | of plays at a time, and the page asks for this many behind and 8 ahead.
reach :: Scene -> Number -> Int
reach scene tempo
  | scene.spec.samplePattern == "" = 0
  | otherwise =
      let
        longest = fromMaybe 0.0 (maximum (scene.corpus.samples <#> _.secs))
        rate = if scene.spec.speed == 0.0 then 1.0 else abs scene.spec.speed
      in
        min 48 (ceil (longest / rate / cycleSeconds tempo))

planCycle :: { scene :: Scene, material :: Material, tempo :: Number, plays :: Int -> Array Play } -> Int -> Plan
planCycle { scene, material, tempo, plays } cycle =
  let
    orbit = scene.spec.chain.orbit
    emitsOf c = cycleOfPlays (plays c) scene.corpus scene.query scene.spec scene.seed c
    emits = emitsOf cycle
    seconds = cycleSeconds tempo
    place span =
      let
        start = locate material span.emit.n span.emit.begin
      in
        { emit: span.emit
        , from: span.from
        , to: span.to
        , sounding: max 0.004 (span.emit.sustain / seconds)
        , colour: colourCss (colourAt material start)
        , reads: { from: start, to: locate material span.emit.n span.emit.end }
        }
    within = wedges { cycleSeconds: seconds, orbit } emits
    -- A sample that began in an earlier bar and has not ended: the last one
    -- to begin before this bar, drawn from the bar's start until it ends or
    -- the next one begins, as a wedge is. Its `from` is before zero, so the
    -- strip's read head is as far through it as the sound is.
    back = min cycle (reach scene tempo)
    earlier k = last (sortBy (comparing _.at) (filter (\e -> e.chain.orbit == orbit) (emitsOf (cycle - k)))) <#> \e ->
      { emit: e, from: e.at - toNumber k }
    carried = do
      c <- if back < 1 then Nothing else findMap earlier (range 1 back)
      let ends = c.from + c.emit.sustain / seconds
      if ends <= 0.0 then Nothing
      else Just { emit: c.emit, from: c.from, to: min ends (maybe 1.0 _.from (within !! 0)) }
    placed = map place (maybe within (\c -> [ c ] <> within) carried)
    laneOf = lanes (2.0 / stripWidth) (placed <#> \g -> { from: g.reads.from.along, to: g.reads.to.along })
    grains = zipWith (\g lane -> { emit: g.emit, from: g.from, to: g.to, sounding: g.sounding, colour: g.colour, reads: g.reads, lane }) placed laneOf
  in
    { cycle
    , grains
    , lanes: if length grains == 0 then 1 else 1 + fromMaybe 0 (maximum (grains <#> _.lane))
    }
