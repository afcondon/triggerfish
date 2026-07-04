-- | Vetula.Store — localStorage persistence for the progression library (the
-- | auto-capture stack). Following the "save the rendering, not bespoke
-- | structure" rule (cf. Triggerfish.Selene.Store): a library entry's canonical
-- | form is its **Tidal source text** — the transferable unit that parses back
-- | to note-lists. So the persisted envelope is all strings + a keeper flag; no
-- | ChordNode / Key / Mode codecs, and it round-trips through the same
-- | render/parse the app already uses for copy-paste and import.
module Vetula.Store
  ( Entry
  , Saved
  , saveLibrary
  , loadLibrary
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)

-- | One saved progression: its display label, its Tidal source (the chords), and
-- | whether it's a promoted keeper (★) or an ephemeral auto-capture (◦).
type Entry = { name :: String, keyLabel :: String, source :: String, kept :: Boolean }

-- | What we persist: the whole library, newest-appended.
type Saved = { library :: Array Entry }

storeKey :: String
storeKey = "triggerfish.vetula.library.v1"

foreign import _save :: String -> String -> Effect Unit
foreign import _load :: String -> Effect (Nullable Saved)
foreign import _stringify :: Saved -> String

-- | Persist the library (best-effort — FFI swallows quota / private-mode errors).
saveLibrary :: Saved -> Effect Unit
saveLibrary s = _save storeKey (_stringify s)

-- | Load the stored library, or `Nothing` if absent / unparseable.
loadLibrary :: Effect (Maybe Saved)
loadLibrary = map toMaybe (_load storeKey)
