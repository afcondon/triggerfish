-- | Triggerfish.Odonus.Store — localStorage persistence for the live Odonus
-- | patch, serialised as **Lepidoptera eDSL text** (one `odonusPatch` rendering).
-- | The text is the canonical, transferable form — drop it into Calypso or ship
-- | it to purerl-tidal, which speak the same dialect. Unlike Selene/Balistes
-- | this stores the raw eDSL string directly (no JSON envelope): a single live
-- | patch, not yet a named library. The cross-instrument library manager (A5)
-- | is where the named collection lands.
module Triggerfish.Odonus.Store
  ( savePatch
  , loadPatch
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)

storeKey :: String
storeKey = "triggerfish.odonus.patch.v1"

foreign import _save :: String -> String -> Effect Unit
foreign import _load :: String -> Effect (Nullable String)

-- | Persist the live patch's eDSL text (best-effort — the FFI swallows
-- | quota / private-mode errors).
savePatch :: String -> Effect Unit
savePatch txt = _save storeKey txt

-- | Load the stored patch text, or `Nothing` if absent. The caller parses it
-- | back through `Lepidoptera.parsePatch` (which yields `Nothing` on malformed
-- | text, falling back to the default setup).
loadPatch :: Effect (Maybe String)
loadPatch = map toMaybe (_load storeKey)
