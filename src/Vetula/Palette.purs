-- | `Vetula.Palette` — curated chord palettes, ported verbatim from the
-- | lylepmills Plaits alt-firmware chord banks (`plaits/dsp/chords/chord_bank`).
-- |
-- | These are **curated voicings**, not theory — so they live here in the app,
-- | not in harmonia. Each entry is a literal interval-voicing in semitones from
-- | a root (the firmware's `Freq=Root, Harmonics=Chord` banks), so the specific
-- | octave placements the tables bake in (e.g. the `10th` spread-maj7) are
-- | preserved rather than flattened through `realize`.
-- |
-- | Two banks:
-- |   * `butlerPalette` — Jon Butler's 17-chord table (the superset).
-- |   * `stockPalette`  — Émilie Gillet's original 11, which the firmware defines
-- |     as an index map into Butler's table (`originalChordMapping`).
-- |
-- | Joe McMullen's table is the third bank, but it is *scale-relative*
-- | (`Freq=KEY, Harmonics=SCALE POSITION`) — so it lives in harmonia as
-- | `mcmullenYellow` (real `DegreeChord`s), not here.
-- |
-- | Rooted on a note, each entry becomes a `ChordNode` for the pool; `anchor` is
-- | `Free` for now (a root+quality voicing has no fixed scale reading — grade is
-- | a per-view, on-placement concern we colour later).
module Vetula.Palette
  ( PaletteEntry
  , butlerPalette
  , stockPalette
  , paletteNode
  , butlerChords
  , stockChords
  ) where

import Prelude

import Data.Array (mapMaybe, mapWithIndex, nub, (!!))
import Data.Maybe (Maybe(..))
import Harmonia.Anchor (Anchor(..))
import Harmonia.Chord (Key)
import Vetula.Harmony (ChordNode, Kind(..), noteName)

type PaletteEntry = { name :: String, intervals :: Array Int }

-- | Jon Butler's 17 chords — semitone offsets from the root, in the firmware's
-- | order (so `stockMapping` indexes are correct). The near-unison beating notes
-- | the organ voice used (0.01 / 7.01 / 11.99) are dropped; only the harmonic
-- | content remains.
butlerPalette :: Array PaletteEntry
butlerPalette =
  [ { name: "oct",  intervals: [ 0, 12 ] }
  , { name: "5",    intervals: [ 0, 7, 12 ] }
  , { name: "m",    intervals: [ 0, 3, 7, 12 ] }
  , { name: "m7",   intervals: [ 0, 3, 7, 10 ] }
  , { name: "m9",   intervals: [ 0, 3, 10, 14 ] }
  , { name: "m11",  intervals: [ 0, 3, 10, 17 ] }
  , { name: "M",    intervals: [ 0, 4, 7, 12 ] }
  , { name: "M7",   intervals: [ 0, 4, 7, 11 ] }
  , { name: "M9",   intervals: [ 0, 4, 11, 14 ] }
  , { name: "sus4", intervals: [ 0, 5, 7, 12 ] }
  , { name: "69",   intervals: [ 0, 2, 9, 16 ] }
  , { name: "6",    intervals: [ 0, 4, 7, 9 ] }
  , { name: "10",   intervals: [ 0, 7, 16, 23 ] }   -- spread maj7
  , { name: "7",    intervals: [ 0, 4, 7, 10 ] }    -- dominant 7th
  , { name: "7b9",  intervals: [ 0, 7, 10, 13 ] }
  , { name: "hd",   intervals: [ 0, 3, 6, 10 ] }    -- half-diminished
  , { name: "fd",   intervals: [ 0, 3, 6, 9 ] }     -- fully-diminished
  ]

-- | The firmware's `originalChordMapping` — which Butler chords make up Émilie's
-- | original 11-chord bank, in the original order.
stockMapping :: Array Int
stockMapping = [ 0, 1, 9, 2, 3, 4, 5, 10, 8, 7, 6 ]

-- | Émilie Gillet's original 11-chord bank, as a curated subset of Butler.
stockPalette :: Array PaletteEntry
stockPalette = mapMaybe (\i -> butlerPalette !! i) stockMapping

-- | Root a palette entry on a pitch class, producing a pool `ChordNode`. The
-- | literal interval-voicing is placed from octave 4 (48 + root); `pcs` is the
-- | de-duplicated pitch-class content. The `id` is provisional — `DropSet`
-- | reassigns it when the set is dropped onto the surface.
paletteNode :: Int -> Int -> PaletteEntry -> ChordNode
paletteNode nid root entry =
  { id: nid
  , parentId: Nothing
  , root
  , bassPc: root
  , pcs: nub (map (\iv -> mod (root + iv) 12) entry.intervals)
  , voicing: map (\iv -> 48 + root + iv) entry.intervals
  , kind: Seed
  , label: noteName root <> " " <> entry.name
  , pinned: false
  , outside: 0
  , targetX: 0.0
  , targetY: 0.0
  , isCentre: false
  , anchor: Free
  }

-- | The whole Butler bank rooted on the key's tonic — an exterior populator.
butlerChords :: Key -> Array ChordNode
butlerChords key = mapWithIndex (\i e -> paletteNode i key.tonic e) butlerPalette

-- | Émilie's original bank rooted on the key's tonic.
stockChords :: Key -> Array ChordNode
stockChords key = mapWithIndex (\i e -> paletteNode i key.tonic e) stockPalette
