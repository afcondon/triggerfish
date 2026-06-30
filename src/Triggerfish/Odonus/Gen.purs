-- | `Triggerfish.Odonus.Gen` — re-export shim. The randomisation engine now
-- | lives in the portable `reef` package (`Reef.Gen`, alongside the gen-source
-- | descriptor it consumes) so the *whole* Odonus module — engine + generation —
-- | is one source of truth across the Triggerfish JS frontend and the
-- | purerl-tidal Erlang engine. This is the lockstep foundation: the rig can now
-- | generate autonomously and a frontend co-simulation stays bit-identical
-- | (reef/docs/PLAN-lockstep-cosimulation.md). Edit it in reef; this re-exports
-- | the engine entry points under the historical name so Grid is unchanged.
-- |
-- | (The descriptor — `GenKind`, `GenSource`, `periodOf`, … — is re-exported by
-- | `Triggerfish.Odonus.Grid.Types`, its historical home, not here, so importers
-- | of both modules don't see a doubly-imported name.)
module Triggerfish.Odonus.Gen (module Reef.Gen) where

import Reef.Gen (GenInput, runGen, rollAllNotes, rollChords, seedMelody)
