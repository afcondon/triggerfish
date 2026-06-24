-- | Triggerfish.Selene.Model — the state of the Selene polysignal rack, the
-- | fourth virtual module in the Triggerfish bench (sibling of Odonus +
-- | Balistes). Selene is a stack of *polysignal generators*: each generator is
-- | an "octo" bank of eight related slots whose outputs fan to eight CV/gate
-- | jacks (ES-9 buses 8–15 / ESX-8CV / ES-5 gates), the relation between the
-- | eight being the musical idea — phase-spread LFOs, divided clocks,
-- | Euclidean gates, a held chord.
-- |
-- | The field names here are copied verbatim from `Tidal.Selene` (in
-- | purerl-tidal) so this model ports straight onto the BEAM cell + the
-- | `apply-polysignal` wire shape the es9-daemon already speaks. This module is
-- | the faithful core; the component is the design surface that grows past it.
-- |
-- | Skeleton scope: the four generators the rig ships today — POLYLFO,
-- | POLYCLOCK, POLYEUCLID, POLYPRESETNOTE. (`Tidal.Selene` also has a fifth,
-- | the ADSR `EnvSlot`/polyenv — left for later.) No output/scheduling yet:
-- | this lays out the editable state + its eight-slot shape.
module Triggerfish.Selene.Model
  ( Selene
  , defaultSelene
  , Gen(..)
  , genLabel
  , allGens
  , slotCount
  , ModSlot
  , ClockSlot
  , EuclidSlot
  , PresetNoteSlot
  , OutputRange(..)
  , rangeLabel
  , rangeToWire
  , allRanges
  , ClockBase(..)
  , clockBaseToWire
  , clockBaseLabel
  , clockBaseBeats
  , modLfo
  , modClock
  , modEuclid
  , modNote
  , setGen
  , setRange
  , euclidBits
  , noteName
  , clampI
  ) where

import Prelude

import Data.Array (modifyAt, range, (!!))
import Data.Int as Int
import Data.Maybe (fromMaybe)

-- ---------------------------------------------------------------------------
-- Which generator the surface is editing
-- ---------------------------------------------------------------------------

-- | The four polysignal families. Each is an eight-slot bank; the surface
-- | edits one family at a time (they would, in the rig, claim different jacks).
data Gen = GenLfo | GenClock | GenEuclid | GenNote

derive instance Eq Gen

genLabel :: Gen -> String
genLabel = case _ of
  GenLfo -> "POLYLFO"
  GenClock -> "POLYCLOCK"
  GenEuclid -> "POLYEUCLID"
  GenNote -> "POLYNOTE"

allGens :: Array Gen
allGens = [ GenLfo, GenClock, GenEuclid, GenNote ]

-- | The eight slots of an octo bank.
slotCount :: Int
slotCount = 8

-- ---------------------------------------------------------------------------
-- Output range — how a normalised slot value maps to volts at the jack
-- ---------------------------------------------------------------------------

data OutputRange
  = Unipolar10V
  | Bipolar5V
  | Unipolar1V
  | Unipolar5V
  | Unipolar8V

derive instance Eq OutputRange

rangeToWire :: OutputRange -> String
rangeToWire = case _ of
  Unipolar10V -> "unipolar10v"
  Bipolar5V -> "bipolar5v"
  Unipolar1V -> "unipolar1v"
  Unipolar5V -> "unipolar5v"
  Unipolar8V -> "unipolar8v"

rangeLabel :: OutputRange -> String
rangeLabel = case _ of
  Unipolar10V -> "0–10V"
  Bipolar5V -> "±5V"
  Unipolar1V -> "0–1V"
  Unipolar5V -> "0–5V"
  Unipolar8V -> "0–8V"

allRanges :: Array OutputRange
allRanges = [ Bipolar5V, Unipolar10V, Unipolar8V, Unipolar5V, Unipolar1V ]

-- ---------------------------------------------------------------------------
-- Slot record types (field names match Tidal.Selene exactly)
-- ---------------------------------------------------------------------------

-- | POLYLFO: one continuous waveform per slot = a DC `level` plus a mix of six
-- | shapes, free-running at `rate` Hz from initial `phase`. All amps normalised.
type ModSlot =
  { rate :: Number
  , phase :: Number
  , level :: Number
  , sin :: Number
  , sqr :: Number
  , tri :: Number
  , saw :: Number
  , rnd :: Number
  , nse :: Number
  }

-- | POLYCLOCK: a tempo-locked gate train. `base` / `multiplier` set the
-- | division (beats-per-pulse = base-beats / multiplier), `pulseWidth` the
-- | duty %, `phase` a degrees offset. Rate rides the Link tempo at playback.
type ClockSlot =
  { base :: ClockBase
  , multiplier :: Int
  , pulseWidth :: Int
  , phase :: Int
  }

-- | POLYEUCLID: a Euclidean gate — `beats` pulses spread over `steps`, clocked
-- | at `rate` subdivisions/beat. `accentRate` is an FH-2 feature the ES-9 drops.
type EuclidSlot =
  { beats :: Int
  , steps :: Int
  , rate :: Int
  , accentRate :: Int
  }

-- | POLYPRESETNOTE: a static V/oct pitch, one held MIDI note per slot.
type PresetNoteSlot =
  { note :: Int
  }

-- ---------------------------------------------------------------------------
-- Clock base durations
-- ---------------------------------------------------------------------------

data ClockBase
  = ClockWhole
  | ClockHalf
  | ClockQuarter
  | ClockQuarterT
  | ClockEighth
  | ClockEighthT
  | ClockSixteenth
  | ClockSixteenthT
  | ClockThirtySecond
  | ClockThirtySecondT
  | ClockSixtyFourthT

derive instance Eq ClockBase

clockBaseToWire :: ClockBase -> String
clockBaseToWire = case _ of
  ClockWhole -> "whole"
  ClockHalf -> "half"
  ClockQuarter -> "quarter"
  ClockQuarterT -> "qt"
  ClockEighth -> "8th"
  ClockEighthT -> "8t"
  ClockSixteenth -> "16th"
  ClockSixteenthT -> "16t"
  ClockThirtySecond -> "32nd"
  ClockThirtySecondT -> "32t"
  ClockSixtyFourthT -> "64t"

-- | A short readable label for the panel.
clockBaseLabel :: ClockBase -> String
clockBaseLabel = case _ of
  ClockWhole -> "1/1"
  ClockHalf -> "1/2"
  ClockQuarter -> "1/4"
  ClockQuarterT -> "1/4T"
  ClockEighth -> "1/8"
  ClockEighthT -> "1/8T"
  ClockSixteenth -> "1/16"
  ClockSixteenthT -> "1/16T"
  ClockThirtySecond -> "1/32"
  ClockThirtySecondT -> "1/32T"
  ClockSixtyFourthT -> "1/64T"

-- | The base's length in beats (a quarter = 1.0; triplets are 2/3 of the next
-- | size up). Used by the panel's Hz/period readout.
clockBaseBeats :: ClockBase -> Number
clockBaseBeats = case _ of
  ClockWhole -> 4.0
  ClockHalf -> 2.0
  ClockQuarter -> 1.0
  ClockQuarterT -> 2.0 / 3.0
  ClockEighth -> 0.5
  ClockEighthT -> 1.0 / 3.0
  ClockSixteenth -> 0.25
  ClockSixteenthT -> 1.0 / 6.0
  ClockThirtySecond -> 0.125
  ClockThirtySecondT -> 1.0 / 12.0
  ClockSixtyFourthT -> 1.0 / 24.0

-- ---------------------------------------------------------------------------
-- The whole rack state
-- ---------------------------------------------------------------------------

-- | The rack: which generator is in focus, plus an eight-slot bank for each of
-- | the four families and the shared output range. (In the rig each family
-- | claims its own jacks; here they coexist as editable state.)
type Selene =
  { gen :: Gen
  , lfo :: Array ModSlot
  , clock :: Array ClockSlot
  , euclid :: Array EuclidSlot
  , note :: Array PresetNoteSlot
  , range :: OutputRange
  }

-- | A tasteful starting bank for each generator — eight *related* slots, so the
-- | "poly" idea is legible the moment the panel opens.
defaultSelene :: Selene
defaultSelene =
  { gen: GenLfo
  , lfo: map lfoSlot ixs        -- a phase-fanned sine choir
  , clock: map clockSlot ixs    -- a fan of divisions, fastest at the top
  , euclid: map euclidSlot ixs  -- a spread of classic Euclidean patterns
  , note: map noteSlot ixs      -- a C-major-add9 spread chord
  , range: Bipolar5V
  }
  where
  ixs = range 0 (slotCount - 1)

  -- eight sines, evenly phase-spread over the cycle (a travelling wave)
  lfoSlot i =
    { rate: 0.5, phase: Int.toNumber i / Int.toNumber slotCount
    , level: 0.0, sin: 0.8, sqr: 0.0, tri: 0.0, saw: 0.0, rnd: 0.0, nse: 0.0
    }

  -- divisions 1/4 ÷ (i+1): a metric fan
  clockSlot i = { base: ClockQuarter, multiplier: i + 1, pulseWidth: 50, phase: 0 }

  -- a spread of well-known Euclidean rhythms over 8 or 16 steps
  euclidSlot i =
    let steps = if i < 4 then 8 else 16
        beats = clampI 1 steps (3 + (i `mod` 5))
    in { beats, steps, rate: 4, accentRate: 0 }

  -- C2 G2 C3 D3 E3 G3 C4 E4 — a Cadd9 voicing climbing the bank
  noteSlot i = { note: fromMaybe 60 (chord !! i) }
  chord = [ 36, 43, 48, 50, 52, 55, 60, 64 ]

-- ---------------------------------------------------------------------------
-- Editing — one modifier per family, applied to slot i
-- ---------------------------------------------------------------------------

modLfo :: Int -> (ModSlot -> ModSlot) -> Selene -> Selene
modLfo i f s = s { lfo = fromMaybe s.lfo (modifyAt i f s.lfo) }

modClock :: Int -> (ClockSlot -> ClockSlot) -> Selene -> Selene
modClock i f s = s { clock = fromMaybe s.clock (modifyAt i f s.clock) }

modEuclid :: Int -> (EuclidSlot -> EuclidSlot) -> Selene -> Selene
modEuclid i f s = s { euclid = fromMaybe s.euclid (modifyAt i f s.euclid) }

modNote :: Int -> (PresetNoteSlot -> PresetNoteSlot) -> Selene -> Selene
modNote i f s = s { note = fromMaybe s.note (modifyAt i f s.note) }

setGen :: Gen -> Selene -> Selene
setGen g s = s { gen = g }

setRange :: OutputRange -> Selene -> Selene
setRange r s = s { range = r }

-- ---------------------------------------------------------------------------
-- Derived
-- ---------------------------------------------------------------------------

-- | The Euclidean step pattern (Bresenham distribution): step k is a pulse iff
-- | `(k * beats) mod steps < beats`. The same rule the daemon uses.
euclidBits :: EuclidSlot -> Array Boolean
euclidBits sl =
  map (\k -> (k * sl.beats) `mod` sl.steps < sl.beats) (range 0 (sl.steps - 1))

-- | A MIDI note as a pitch-class name + octave (middle C = 60 = "C4").
noteName :: Int -> String
noteName n =
  let
    pcs = [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ]
    pc = fromMaybe "?" (pcs !! (n `mod` 12))
    oct = n / 12 - 1
  in
    pc <> show oct

clampI :: Int -> Int -> Int -> Int
clampI lo hi v = if v < lo then lo else if v > hi then hi else v
