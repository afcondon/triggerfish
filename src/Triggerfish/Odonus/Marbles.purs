-- | `Triggerfish.Odonus.Marbles` — re-export shim. The Marbles seeded-PRNG +
-- | Beta note generator now lives in the portable `reef` package
-- | (`Reef.Marbles`) so generation is one source of truth across the
-- | Triggerfish JS frontend and the purerl-tidal Erlang engine (the lockstep
-- | foundation). Edit it in reef; this re-exports it under the historical name
-- | so the rest of Triggerfish (Gen, View.Generate, Grid) is unchanged.
module Triggerfish.Odonus.Marbles (module Reef.Marbles) where

import Reef.Marbles
