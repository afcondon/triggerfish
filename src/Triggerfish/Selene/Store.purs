-- | Triggerfish.Selene.Store — localStorage persistence for the Selene rack
-- | library. A rack's canonical form is its **eDSL doc text** (the SOURCE pane
-- | authority, `printRack`/`parseRack`); a saved rack is just `{name, doc}`, and
-- | the library is a named collection of them. The JSON here is only the *local
-- | envelope* around those eDSL-text docs — the transferable unit is a single
-- | rack's `doc` (copy it into Calypso, which speaks the same dialect). First
-- | instance of the Lepidoptera "save the rendering, not bespoke structure"
-- | rule; Balistes' Store converges onto this shape next.
module Triggerfish.Selene.Store
  ( Rack
  , Saved
  , saveLibrary
  , loadLibrary
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)

-- | A named rack: its eDSL doc is the whole rack, rendered.
type Rack = { name :: String, doc :: String }

-- | What we persist: the library + which rack was open.
type Saved = { active :: Int, library :: Array Rack }

storeKey :: String
storeKey = "triggerfish.selene.library.v1"

foreign import _save :: String -> String -> Effect Unit
foreign import _load :: String -> Effect (Nullable Saved)
foreign import _stringify :: Saved -> String

-- | Persist the library (best-effort — FFI swallows quota/private-mode errors).
saveLibrary :: Saved -> Effect Unit
saveLibrary s = _save storeKey (_stringify s)

-- | Load the stored library, or `Nothing` if absent / unparseable.
loadLibrary :: Effect (Maybe Saved)
loadLibrary = map toMaybe (_load storeKey)
