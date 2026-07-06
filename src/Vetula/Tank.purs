-- | `Vetula.Tank` — the durable store of caught chords.
-- |
-- | A `Specimen` is a **frozen, voiced chord** — the atom the tank trades in. It
-- | carries an absolute-MIDI voicing plus its own bass, a descriptive label, and a
-- | provenance note saying where it came from. Crucially it references NO lattice
-- | node id: a specimen is self-contained, so the volatile physics surface can
-- | reflow, re-key, and regenerate underneath it without ever disturbing the tank
-- | or a sequence built from it. (That decoupling is the whole point — see
-- | docs/DESIGN-vetula-tank-model.md.)
-- |
-- | This module is data-only (no Halogen); the tank UI + verbs live in `Vetula.App`.
module Vetula.Tank
  ( SpecimenId(..)
  , Provenance(..)
  , Specimen
  , specNotes
  , transposeSpec
  ) where

import Prelude

import Data.Array ((:))
import Harmonia.Anchor (Anchor)
import Harmonia.Graded (transpose)

-- | Opaque id minted on catch. NOT a lattice node id — a specimen outlives the
-- | node it was frozen from.
newtype SpecimenId = SpecimenId Int

derive instance eqSpecimenId :: Eq SpecimenId
derive instance ordSpecimenId :: Ord SpecimenId

-- | An honest record of origin — pure metadata, never a constraint on what a
-- | specimen can sit next to. `FromLens` keeps a display label of the harmonic
-- | context it was caught in (e.g. "C major"); `Transposed` back-points at the
-- | specimen it was shifted from.
data Provenance
  = FromLens String       -- caught from a lens, in harmonic context <label>
  | Transposed SpecimenId Int
  | Hand                  -- entered directly
  | Imported              -- restored from a saved sequence / library

-- | A frozen voiced chord. `voicing` is the sounding upper notes as absolute
-- | MIDI; `bass` is the foot on its own line. Absolute MIDI everywhere so
-- | transposition is arithmetic.
type Specimen =
  { id :: SpecimenId
  , voicing :: Array Int
  , bass :: Int
  , label :: String
  , provenance :: Provenance
  , anchor :: Anchor       -- the harmonic reading (from Harmonia): drives grade,
                           -- the affordance colour, and which verbs light up.
                           -- `Free` for chords caught without a scale context.
  }

-- | The full sounding note set — foot then uppers — for audition and realisation.
specNotes :: Specimen -> Array Int
specNotes s = s.bass : s.voicing

-- | Shift a specimen by `n` semitones, minting a fresh id and recording the
-- | capo/shift in its provenance. (Used from Slice E; here so the type is stable.)
transposeSpec :: SpecimenId -> Int -> Specimen -> Specimen
transposeSpec newId n s =
  s { id = newId
    , voicing = map (_ + n) s.voicing
    , bass = s.bass + n
    , provenance = Transposed s.id n
    , anchor = transpose n s.anchor   -- move the reading with the pitches
    }
