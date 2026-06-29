-- | Triggerfish.Balistes.Store — localStorage persistence for the fixed-rhythm
-- | library, now serialised as **Lepidoptera eDSL text** (one `balistesPattern`
-- | rendering per pattern), not bespoke JSON. The eDSL text is the canonical,
-- | transferable form — a single pattern's text drops straight into Calypso or
-- | ships to purerl-tidal, which speak the same dialect. The JSON here is only
-- | the local envelope holding the array of texts. Mirrors Selene's Store;
-- | converges the A2 step of the Lepidoptera plan.
module Triggerfish.Balistes.Store
  ( saveLibrary
  , loadLibrary
  ) where

import Prelude

import Data.Array (mapMaybe)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Triggerfish.Balistes.Lepidoptera (parsePattern, printPattern)
import Triggerfish.Balistes.Pattern (FixedPattern)

-- v2: format changed from bespoke JSON (v1) to eDSL text.
storeKey :: String
storeKey = "triggerfish.balistes.library.v2"

foreign import _save :: String -> String -> Effect Unit
foreign import _load :: String -> Effect (Nullable (Array String))
foreign import _stringify :: Array String -> String

-- | Persist the library: each pattern rendered to its eDSL text, the array of
-- | texts stored as a JSON envelope (best-effort — FFI swallows storage errors).
saveLibrary :: Array FixedPattern -> Effect Unit
saveLibrary lib = _save storeKey (_stringify (map printPattern lib))

-- | Load the library, parsing each eDSL text; unparseable entries are dropped,
-- | and an absent / corrupt store yields `Nothing` → bundled fallback.
loadLibrary :: Effect (Maybe (Array FixedPattern))
loadLibrary = do
  m <- _load storeKey
  pure (map (mapMaybe parsePattern) (toMaybe m))
