-- | Triggerfish.Balistes.Pattern — the fixed-rhythm half of the Pattern family.
-- |
-- | A Balistes panel plays a *Pattern*. Grids (the X/Y morph engine in
-- | `Model`) is one — special, generative, mutatable. The other kind is a
-- | **fixed rhythm**: a literal `lane × step` velocity grid, the kind you get
-- | from transcribing a starter beat (the ~30 from *The Secrets of Dance Music
-- | Production*). It doesn't morph; it's a labelled loop you recall instantly
-- | and tweak.
-- |
-- | Every fixed rhythm is laid against ONE canonical 16-lane kit (`canonKit`),
-- | so patterns stack, swap samples, and sequence against the same rows. Import
-- | maps each source MIDI note onto a lane by GM number (`laneFromNote`); a
-- | pattern that only uses six voices leaves the other ten lanes empty (they
-- | fold away in the compact view).
module Triggerfish.Balistes.Pattern
  ( KitLane
  , canonKit
  , kitSize
  , laneName
  , laneNote
  , laneFromNote
  , FixedPattern
  , velAt
  , firesAt
  , usedLanes
  , noteOf
  , setNoteAt
  , defaultNotes
  , emptyGrid
  , buildGrid
  , bundledPatterns
  , houseLoTempo110
  ) where

import Prelude

import Data.Array (any, filter, findIndex, range, replicate, updateAt, (!!))
import Data.Foldable (foldl)
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple(..))

-- | One row of the shared kit: a clear name plus the GM note it sends.
type KitLane = { name :: String, note :: Int }

-- | The canonical 16-lane kit — the shared coordinate system every fixed
-- | rhythm is laid against. Ordered kit-classically top-to-bottom (kick,
-- | snares/claps, hats, toms, cymbals, percussion); notes are GM percussion so
-- | imported MIDI lands on the right lane by number. Samples are swapped freely
-- | downstream (Ableton / SuperDirt / modular) — these notes are just the wire.
canonKit :: Array KitLane
canonKit =
  [ { name: "BD", note: 36 } --  0  bass drum
  , { name: "SD", note: 38 } --  1  snare
  , { name: "CP", note: 39 } --  2  hand clap
  , { name: "RS", note: 37 } --  3  rim / side stick
  , { name: "CH", note: 42 } --  4  closed hat
  , { name: "PH", note: 44 } --  5  pedal hat
  , { name: "OH", note: 46 } --  6  open hat
  , { name: "LT", note: 41 } --  7  low tom
  , { name: "MT", note: 47 } --  8  mid tom
  , { name: "HT", note: 50 } --  9  high tom
  , { name: "RD", note: 51 } -- 10  ride
  , { name: "RB", note: 53 } -- 11  ride bell
  , { name: "CR", note: 49 } -- 12  crash
  , { name: "CW", note: 56 } -- 13  cowbell
  , { name: "TB", note: 54 } -- 14  tambourine
  , { name: "SH", note: 70 } -- 15  shaker / maracas
  ]

kitSize :: Int
kitSize = 16

laneName :: Int -> String
laneName i = fromMaybe "?" (map _.name (canonKit !! i))

laneNote :: Int -> Int
laneNote i = fromMaybe (36 + i) (map _.note (canonKit !! i))

-- | The kit lane a GM note belongs to (Nothing if the note isn't in the kit —
-- | the importer then asks the user where it goes).
laneFromNote :: Int -> Maybe Int
laneFromNote n = findIndex (\k -> k.note == n) canonKit

-- | A literal rhythm: a name, a step count (16 or 32), a dense `kitSize × steps`
-- | grid of velocities (0 = no hit, 1..127 = hit), and a per-lane MIDI note
-- | (`notes`, length `kitSize`). The note is editable per pattern, so the same
-- | grid can drive a different kick / snare / etc — another axis of variation.
-- | Lane index aligns with `canonKit`.
type FixedPattern =
  { name :: String
  , steps :: Int
  , grid :: Array (Array Int)
  , notes :: Array Int
  }

velAt :: FixedPattern -> Int -> Int -> Int
velAt p lane step = fromMaybe 0 ((p.grid !! lane) >>= (_ !! step))

firesAt :: FixedPattern -> Int -> Int -> Boolean
firesAt p lane step = velAt p lane step > 0

-- | This pattern's MIDI note for a lane (falls back to the kit default).
noteOf :: FixedPattern -> Int -> Int
noteOf p lane = fromMaybe (laneNote lane) (p.notes !! lane)

-- | Set a lane's MIDI note (clamped to the 0..127 MIDI range).
setNoteAt :: Int -> Int -> FixedPattern -> FixedPattern
setNoteAt lane n p =
  p { notes = fromMaybe p.notes (updateAt lane (clampNote n) p.notes) }
  where
  clampNote v = if v < 0 then 0 else if v > 127 then 127 else v

-- | The kit's default notes — every lane at its `canonKit` GM note.
defaultNotes :: Array Int
defaultNotes = map _.note canonKit

-- | Which lanes carry any hit — the rows worth showing in the compact view.
usedLanes :: FixedPattern -> Array Int
usedLanes p = filter (\l -> any (_ > 0) (fromMaybe [] (p.grid !! l))) (range 0 (kitSize - 1))

-- | An all-silent `kitSize × steps` grid — the blank canvas.
emptyGrid :: Int -> Array (Array Int)
emptyGrid steps = replicate kitSize (replicate steps 0)

-- | Build a grid from sparse `(lane, velocityRow)` pairs over a blank canvas.
buildGrid :: Int -> Array (Tuple Int (Array Int)) -> Array (Array Int)
buildGrid steps rows = foldl place (emptyGrid steps) rows
  where
  place g (Tuple lane row) = fromMaybe g (updateAt lane row g)

-- | The starter rhythms shipped with the app (extensible: editing/import add
-- | more). First proof: `lo tempo house 110`, transcribed from the book and
-- | exported as MIDI, decoded onto the canonical kit.
bundledPatterns :: Array FixedPattern
bundledPatterns = [ houseLoTempo110 ]

-- | "Lo-tempo house, 110 bpm" — 2 bars (32 sixteenths), six voices: four-on-the-
-- | floor kick with ghost pushes, clap backbeat, off-beat open hats, a driving
-- | accented ride, one tom and a few crashes as the book's arrangement garnish.
houseLoTempo110 :: FixedPattern
houseLoTempo110 =
  { name: "lo house 110"
  , steps: 32
  , notes: defaultNotes
  , grid: buildGrid 32
      [ Tuple 0 [ 98, 0, 0, 0, 98, 0, 0, 0, 98, 0, 0, 0, 98, 0, 0, 98, 98, 0, 0, 0, 98, 0, 0, 0, 98, 0, 0, 0, 98, 0, 98, 0 ] -- BD
      , Tuple 2 [ 0, 0, 0, 0, 98, 0, 0, 0, 0, 0, 0, 0, 98, 0, 0, 0, 0, 0, 0, 0, 98, 0, 0, 0, 0, 0, 0, 0, 98, 0, 0, 0 ] -- CP
      , Tuple 6 [ 0, 0, 98, 0, 0, 0, 98, 0, 0, 0, 98, 0, 0, 0, 98, 0, 0, 0, 98, 0, 0, 0, 98, 0, 0, 0, 98, 0, 0, 0, 98, 0 ] -- OH
      , Tuple 9 [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 98, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ] -- HT
      , Tuple 10 [ 127, 100, 127, 0, 127, 0, 127, 82, 127, 127, 127, 0, 127, 0, 127, 127, 127, 127, 127, 0, 127, 0, 127, 127, 127, 127, 127, 0, 127, 0, 127, 127 ] -- RD
      , Tuple 12 [ 0, 0, 0, 98, 0, 0, 0, 0, 0, 98, 0, 0, 0, 0, 0, 0, 0, 0, 0, 98, 0, 0, 0, 0, 0, 98, 0, 0, 0, 0, 0, 0 ] -- CR
      ]
  }
