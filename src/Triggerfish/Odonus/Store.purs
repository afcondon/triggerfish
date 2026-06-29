-- | Triggerfish.Odonus.Store — localStorage persistence for Odonus. Persists
-- | the named **scene library** (the recallable presets) plus the current `live`
-- | working patch, each as Lepidoptera eDSL text — the canonical, transferable
-- | form (a scene's text drops into Calypso / ships to purerl-tidal). The JSON
-- | here is only the local envelope around those eDSL texts; mirrors Selene's
-- | Store. (v2: was a single raw patch string; scenes became the preset library.)
module Triggerfish.Odonus.Store
  ( Saved
  , saveAll
  , loadAll
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)

-- | What we persist: the live working patch + the named scene library (each
-- | scene's `text` is its full authored patch rendered to eDSL).
type Saved =
  { live :: String
  , scenes :: Array { name :: String, text :: String }
  }

storeKey :: String
storeKey = "triggerfish.odonus.patch.v2"

foreign import _save :: String -> String -> Effect Unit
foreign import _load :: String -> Effect (Nullable Saved)
foreign import _stringify :: Saved -> String

-- | Persist the live patch + scene library (best-effort — the FFI swallows
-- | quota / private-mode errors).
saveAll :: Saved -> Effect Unit
saveAll sv = _save storeKey (_stringify sv)

-- | Load the stored envelope, or `Nothing` if absent / unparseable. The caller
-- | parses each `text` back through `Lepidoptera.parsePatch`.
loadAll :: Effect (Maybe Saved)
loadAll = map toMaybe (_load storeKey)
