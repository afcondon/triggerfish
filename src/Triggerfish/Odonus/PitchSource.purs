-- | Triggerfish.Odonus.PitchSource — what Odonus's output snaps to past its
-- | scale, as the Lepidoptera format speaks it: the scale alone, or a harmony,
-- | a Tidal note pattern (`odonus $ harmony "<c'maj7 a'min7>/2"`) that the host
-- | samples each step (`Reef.Odonus.followHarmony`).
-- |
-- | Until 2026-10-01 the second case was a fed chord progression with its own
-- | period clock (`chords pcs [...] every N`) or a followed Vetula voice
-- | (`vetula N`). Both are what a harmony pattern now says, so both retired;
-- | the format still reads them (`Lepidoptera`), as the pattern that means the
-- | same, and as the scale.
module Triggerfish.Odonus.PitchSource
  ( PitchSource(..)
  , pitchSourceFrom
  , applyPitchSource
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Triggerfish.Odonus.Model as M

data PitchSource
  = PScale            -- snap to the scale (the `scale:` line)
  | PHarmony String   -- snap to the chord a Tidal note pattern gives, step by step

derive instance eqPitchSource :: Eq PitchSource

pitchSourceFrom :: M.Odonus -> PitchSource
pitchSourceFrom o = case o.harmony of
  Nothing -> PScale
  Just h -> PHarmony h

-- | Install a source. The chord itself follows on the next step, when the
-- | host samples the pattern.
applyPitchSource :: PitchSource -> M.Odonus -> M.Odonus
applyPitchSource src o = case src of
  PScale -> o { harmony = Nothing, chord = Nothing }
  PHarmony h -> o { harmony = Just h }
