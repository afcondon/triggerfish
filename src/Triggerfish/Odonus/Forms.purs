-- | `Triggerfish.Odonus.Forms` — the FORM shelf: named arpeggio/figure shapes
-- | that STAMP the 16-cell note field, the way LOW / MID / MELODY already do.
-- |
-- | The motivation (AC, 2026-09-13) is the iPad Progressions "Arp" shelf: when
-- | the writing is fugue-like, or Reich/phase-like, what you want is not a
-- | Marbles roll but a *plain figure* — up, down, thirds, a triad — laid into
-- | the grid, so the interest comes from the heads (lengths, offsets, Euclid)
-- | rather than from the notes. MELODY is a random line; a FORM is a known one.
-- |
-- | Three design facts, each one deliberate:
-- |
-- |   • **A form is DEGREES, not pitches.** Entries are indices into the active
-- |     pitch set, so the same form is the same shape in any scale, and the
-- |     chord overlay still colours it at the end of `renderCell` — UP over a
-- |     progression is an arpeggio, UP with the overlay off is a run. The
-- |     degrees become raw NOTE knobs here (`knobFor`, the inverse of
-- |     `PitchSet.equalIndex`), because a knob is what a cell stores.
-- |
-- |   • **Short forms TILE.** A 3-entry TRIAD laid across 16 cells rotates each
-- |     time round (16 = 3·5 + 1), so the grid itself phases before a single
-- |     head offset is touched — the Reich case, gratis. The odd lengths in the
-- |     library below are chosen for that, not by accident.
-- |
-- |   • **Forms are written in READING order, not row-major.** Cell *k of head
-- |     I's pattern* gets entry k, so UP sounds like up even when head I is on
-- |     Serpentine or Spiral. The other heads then read that same material
-- |     through their own patterns — a permutation of the figure, which is the
-- |     canon-ish material this whole idea is after.
-- |
-- | Adding a form is one line of `formLibrary`. Writing or generating them from
-- | the surface (rather than picking off a shelf) is the obvious next move, and
-- | is why a form is a plain `Array Int` with nothing else in it.
module Triggerfish.Odonus.Forms
  ( Form
  , formLibrary
  , stampForm
  ) where

import Prelude

import Data.Array (findIndex, length, mapWithIndex, (!!), (..))
import Data.Maybe (Maybe(..), fromMaybe)
import Reef.PitchSet (cardinality)
import Triggerfish.Odonus.Model as M

-- | A named figure over scale DEGREES, relative to the form's own base (0 = the
-- | bottom note of the figure, not the bottom of the grid's range).
type Form = { name :: String, degrees :: Array Int }

-- | The shelf: classic arp shapes, two figured-bass idioms, and the Piano Phase
-- | cell (Reich's twelve notes as degrees of the minor scale).
formLibrary :: Array Form
formLibrary =
  [ { name: "UP", degrees: [ 0, 1, 2, 3, 4, 5, 6, 7 ] }
  , { name: "DOWN", degrees: [ 7, 6, 5, 4, 3, 2, 1, 0 ] }
  , { name: "↑↓", degrees: [ 0, 1, 2, 3, 4, 3, 2, 1 ] }
  , { name: "↓↑", degrees: [ 4, 3, 2, 1, 0, 1, 2, 3 ] }
  , { name: "TRIAD", degrees: [ 0, 2, 4 ] }
  , { name: "7TH", degrees: [ 0, 2, 4, 6 ] }
  , { name: "3RDS", degrees: [ 0, 2, 1, 3, 2, 4, 3, 5 ] }
  , { name: "ALBERTI", degrees: [ 0, 4, 2, 4 ] }
  , { name: "OCT", degrees: [ 0, 7 ] }
  , { name: "PEDAL", degrees: [ 0, 2, 0, 4, 0, 5, 0, 3 ] }
  , { name: "PHASE", degrees: [ 0, 1, 4, 5, 6, 1, 0, 5, 4, 1, 6, 5 ] }
  , { name: "STAIR", degrees: [ 0, 1, 2, 1, 2, 3, 2, 3, 4 ] }
  ]

-- | Lay form `ix` into the note field, returning the 16 raw NOTE knobs in
-- | row-major cell order — ready for `Reef.Input.SetNotes`, which is how the
-- | stamp reaches both runtimes as one absolute, replayable edit. An unknown
-- | index (or an empty form) leaves the cells as they are.
stampForm :: Int -> M.Odonus -> Array Int
stampForm ix o = case formLibrary !! ix of
  Nothing -> current
  Just f ->
    let
      n = length f.degrees
    in
      if n == 0 then current
      else
        let
          card = cardinality (M.effectivePitchSet o)
          slots = max 1 (o.span * card)
          -- Sit the figure in the MIDDLE period of the span, not on its floor:
          -- at the default span of 3 that is the middle octave, which is where a
          -- melody wants to be. LOW / MID remain the way to flatten to a floor.
          base = ((o.span - 1) `div` 2) * card
          -- Reading order of head I — identity for the default Rows pattern.
          order = M.orderOf (fromMaybe 0 (map _.patternIx (o.heads !! 0)))
          -- The figure, tiled to 16, in reading order.
          voice = map (\k -> knobFor slots (base + fromMaybe 0 (f.degrees !! (k `mod` n)))) (0 .. 15)
        in
          -- Scatter back to row-major: cell `order !! k` holds entry k.
          mapWithIndex
            (\i old -> fromMaybe old (findIndex (eq i) order >>= \k -> voice !! k))
            current
  where
  current = map _.note o.cells

-- | The raw NOTE knob that equal-maps to set index `i` — the inverse of
-- | `PitchSet.equalIndex`, rounded UP so the floor division lands back exactly
-- | on `i` (valid while `slots ≤ knobMax + 1`, which a span of periods is).
knobFor :: Int -> Int -> Int
knobFor slots i =
  let
    idx = clamp 0 (slots - 1) i
  in
    clamp 0 M.knobMax ((idx * (M.knobMax + 1) + slots - 1) `div` slots)
