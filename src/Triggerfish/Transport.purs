-- | The transport/authority model for the whole control surface.
-- |
-- | The design claim (see docs/DESIGN-transport-misu.md): the entire
-- | user-facing transport reduces to TWO stored values in the shell —
-- |
-- |   mode  :: Mode        -- Solo | Atlantis
-- |   armed :: Set Which   -- which machines the user has armed
-- |
-- | — plus the STATIC fact `hasRigVoice`. Everything the old code scattered
-- | across `master` / `audible` / `playing` / `rigOn` booleans is a pure
-- | function of those, so the illegal states (master=false, rigOn in Solo,
-- | audible=true on a rig voice in Atlantis, …) can't be written down.
-- |
-- | `Sounding` is the derived per-machine verdict the shell pushes to each
-- | instrument (`SourceQuery.SetSounding`). An instrument stores just this one
-- | value in place of the old three-or-four booleans.
module Triggerfish.Transport
  ( Which(..)
  , Mode(..)
  , Sounding(..)
  , hasRigVoice
  , soundingOf
  , localAudible
  , anyArmed
  , allMachines
  ) where

import Prelude

import Data.Set (Set)
import Data.Set as Set

-- The tabs in the switcher. Odo/Bal/Sel/Vet are machines; Tid is the aggregate
-- source view, not a machine (never armed, no rig voice). Suf (Sufflamen) is the
-- rig-only SuperDirt instrument — present as a tab but not yet transport-wired
-- (the D1 visualizer prototype; arm/emit arrive with routing gates C & B), so it
-- behaves like Tid here: never armed, no local rig voice.
data Which = Odo | Bal | Sel | Vet | Tid | Suf

derive instance Eq Which
derive instance Ord Which

-- The SOLO⟷ATLANTIS authority. Solo — the FRONTEND is authoritative (engines run
-- locally, direct to a MIDI sink via Web MIDI). Atlantis — the RIG (backend) is
-- authoritative; the frontend is muted but keeps its schedulers running (lockstep
-- animation). See docs/PLAN-control-surface-solo-atlantis.md.
data Mode = Solo | Atlantis

derive instance Eq Mode

-- WHERE a machine's PERFORMANCE output is sounding right now.
--   Silent — not armed (transport stopped): no performance output anywhere.
--   Local  — armed, frontend-authoritative: local Web-MIDI plays.
--   Rig    — armed, rig-authoritative (Atlantis, has a rig voice): local muted,
--            the backend voice makes the sound.
data Sounding = Silent | Local | Rig

derive instance Eq Sounding

-- Does this machine have a BEAM voice that can take authority? Static, not state.
-- Selene is frontend-only (POLYTRIG drums) so it's never rig-authoritative — its
-- "Local in both modes" behaviour falls out of this, no special-casing needed.
hasRigVoice :: Which -> Boolean
hasRigVoice = case _ of
  Odo -> true
  Bal -> true
  Vet -> true
  Sel -> false
  Tid -> false
  Suf -> false   -- rig-only at heart, but not transport-wired in the D1 prototype

-- The whole authority model in one function.
soundingOf :: Mode -> Set Which -> Which -> Sounding
soundingOf mode armed w
  | not (Set.member w armed) = Silent
  | otherwise = case mode of
      Solo     -> Local
      Atlantis -> if hasRigVoice w then Rig else Local

-- Is the frontend allowed to make LOCAL sound for this machine? True unless the
-- rig is authoritative. This gates AUDITION (manual chord/pad preview): audition
-- must survive a STOPPED transport (Silent ⇒ still audible) but may fall silent
-- when MUTED (Rig). Performance emission is stricter — it gates on `== Local`.
localAudible :: Sounding -> Boolean
localAudible = case _ of
  Rig -> false
  _   -> true

anyArmed :: Set Which -> Boolean
anyArmed = not <<< Set.isEmpty

-- The four playable machines (Tid excluded) — the arm-all target.
allMachines :: Set Which
allMachines = Set.fromFoldable [ Odo, Bal, Sel, Vet ]
