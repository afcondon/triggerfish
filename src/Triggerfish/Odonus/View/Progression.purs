-- | The chord progression Odonus is following, as names (AC, 2026-10-05: "the
-- | user can follow the effect the chord progression is having on the
-- | generated notes, because it can be non-obvious").
-- |
-- | The rig samples Odonus's patterns ahead (`odonus-sample`, Odonus.Samples),
-- | the same samples it plays from, so the chords of the next few bars are
-- | already here as voiced notes: for the grid (a chord shaping it) and for the
-- | output (a chord colouring it). Consecutive repeats collapse to one; each is
-- | named by Harmonia's recogniser, its lowest note the bass. The first is the
-- | chord in force now.
module Triggerfish.Odonus.View.Progression
  ( Chord
  , gridChords
  , outChords
  , chordName
  ) where

import Prelude

import Data.Array (catMaybes, foldl, head, last, range, snoc, sort, nub)
import Data.Maybe (Maybe(..), maybe)
import Harmonia.Recognise (best, candidateName, observeWithBass)
import Reef.Input (Input(..))
import Triggerfish.Odonus.Grid.Types (State)
import Triggerfish.Odonus.Samples as Samples

type Chord = { name :: String, notes :: Array Int }

-- | The chords shaping the grid over the sampled window, now first.
gridChords :: State -> Array Chord
gridChords = chords (\i -> case i of
  SetSampled _ _ g -> g
  _ -> Nothing)

-- | The chords colouring the output over the sampled window, now first.
outChords :: State -> Array Chord
outChords = chords (\i -> case i of
  SetSampled c _ _ -> c
  _ -> Nothing)

chords :: (Input -> Maybe (Array Int)) -> State -> Array Chord
chords pick st =
  let
    -- the patterns as Odonus.Grid.patternsNow gives them (not imported: Grid
    -- imports this view)
    patterns = { harmony: st.odo.harmony, scale: st.odo.scalePattern, outScale: st.odo.outScale, gridHarmony: st.odo.gridHarmony }
    key = Samples.keyOf patterns st.stepDiv
    from = st.nextModelStep
    notes = catMaybes (map (\step -> Samples.sampleAt key step st.samples >>= pick) (range from (from + Samples.window.count - 1)))
    runs = foldl (\acc ns -> if map _.notes (last acc) == Just ns then acc else snoc acc { name: chordName ns, notes: ns }) [] notes
  in
    runs

-- | A chord's name from its notes as voiced: Harmonia's best reading, with
-- | the lowest note as the bass (so an inversion reads as a slash chord);
-- | the pitch classes when nothing fits.
chordName :: Array Int -> String
chordName ns = case head (sort ns) of
  Nothing -> ""
  Just low ->
    let
      pcs = nub (map pc ns)
    in
      maybe (show pcs) candidateName (best (observeWithBass (pc low) pcs))
  where
  pc n = ((n `mod` 12) + 12) `mod` 12
