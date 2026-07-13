-- | `Triggerfish.Balistes.TriSnapshot` — a snapshot that can hold the playing state
-- | of ANY of the three Balistes brains, so one snapshot bank sequences Mutable,
-- | Grids and Tidal states intermingled (the macro-tidal surface). See
-- | `docs/DESIGN-tri-snapshot.md`.
-- |
-- | Today's snapshot is Mutable-only (`Triggerfish.Balistes.Model.Snapshot`, the
-- | Grids control point). This generalises it: one constructor per brain, each
-- | carrying the whole restorable artefact — not a reference — so a snapshot
-- | survives library edits and can be pushed to the rig verbatim (the same
-- | discipline the reef handoffs use: send the whole FixedPattern / TrigKit).
-- |
-- | This module is the pure MODEL foundation (slice 1): the type + brain tags +
-- | pure describe/badge helpers. Capture and recall touch component `State` /
-- | `HalogenM`, so they live in `Triggerfish.Balistes.Component` (slices 2–3).
module Triggerfish.Balistes.TriSnapshot
  ( TriSnapshot(..)
  , Brain(..)
  , brainOf
  , brainBadge
  , brainLabel
  , describeTri
  ) where

import Prelude

import Data.Array (length)
import Triggerfish.Balistes.Model (Snapshot, TrigBank)
import Triggerfish.Balistes.Pattern (FixedPattern, usedLanes)

-- | A captured playing-state, tagged by which brain took it.
-- |
-- |   • `TSGrids` — the Mutable control point (the existing `Snapshot`: X/Y +
-- |     densities + randomness + open + push). The Grids engine regenerates its
-- |     pattern from this, so it's the natural restorable unit.
-- |   • `TSFixed` — a whole GRIDS rhythm (the rich `FixedPattern`, self-contained,
-- |     NOT a `library !! i` index — it must survive library reordering).
-- |   • `TSTrig` — a whole TIDAL POLYTRIG rack (`TrigBank`: named jacks + routes).
data TriSnapshot
  = TSGrids Snapshot
  | TSFixed FixedPattern
  | TSTrig TrigBank

derive instance eqTriSnapshot :: Eq TriSnapshot

-- | The three brains, as a small closed tag (used for badges / tab-switching on
-- | recall). Mirrors `Component.Active` minus the AFixed library index — a
-- | snapshot carries its own pattern, so the index is irrelevant.
data Brain = BGrids | BFixed | BTrig

derive instance eqBrain :: Eq Brain

-- | Which brain a snapshot belongs to (recall switches the active tab to this).
brainOf :: TriSnapshot -> Brain
brainOf = case _ of
  TSGrids _ -> BGrids
  TSFixed _ -> BFixed
  TSTrig _ -> BTrig

-- | The one-letter engraved badge shown on a filled slot (Mutable / Grids / Tidal).
brainBadge :: Brain -> String
brainBadge = case _ of
  BGrids -> "M"
  BFixed -> "G"
  BTrig -> "T"

-- | The tab's display name, for tooltips / the sequence rail.
brainLabel :: Brain -> String
brainLabel = case _ of
  BGrids -> "MUTABLE"
  BFixed -> "GRIDS"
  BTrig -> "TIDAL"

-- | A compact human description of a snapshot's contents (for a slot's title
-- | attribute): the brain + a size cue (used-lane count for a rhythm, jack count
-- | for a rack, the X/Y cursor for a Mutable point).
describeTri :: TriSnapshot -> String
describeTri = case _ of
  TSGrids snap -> "MUTABLE · x" <> show snap.x <> " y" <> show snap.y
  TSFixed pat -> "GRIDS · " <> show (length (usedLanes pat)) <> " lanes"
  TSTrig rack -> "TIDAL · " <> show (length rack.jacks) <> " jacks"
