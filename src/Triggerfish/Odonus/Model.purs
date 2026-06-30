-- | `Triggerfish.Odonus.Model` — re-export shim. The Odonus engine (the
-- | declarative model + the pure `step`/`stepEmit`/`renderCell` semantics) now
-- | lives in the portable `reef` package (`Reef.Odonus`) so it is one source of
-- | truth across the Triggerfish JS frontend and the purerl-tidal Erlang engine.
-- | Edit it in reef; this module re-exports it under the historical name so the
-- | rest of Triggerfish (Grid, views, Patch, Lepidoptera) is unchanged.
module Triggerfish.Odonus.Model (module Reef.Odonus) where

import Reef.Odonus
