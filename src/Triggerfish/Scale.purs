-- | `Triggerfish.Scale` — re-export shim. The scale primitives now live in the
-- | portable `reef` package (`Reef.Scale`) so the Odonus engine is one source of
-- | truth across the JS frontend and the purerl-tidal BEAM engine. Edit them in
-- | reef; this module just re-exports under the historical name.
module Triggerfish.Scale (module Reef.Scale) where

import Reef.Scale
