-- | `Triggerfish.Clips.Store` — localStorage persistence for the SHARED MIDI clip
-- | library (recording axis #27). Machine-agnostic: every capturing machine
-- | (Odonus now, Vetula at #28, later others) reads/writes this one store, so a
-- | clip captured anywhere is pickable everywhere. Mirrors the Odonus/Selene store
-- | FFI pattern (best-effort JSON envelope; swallows quota / private-mode errors).
module Triggerfish.Clips.Store
  ( saveClips
  , loadClips
  ) where

import Prelude

import Data.Maybe (fromMaybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Triggerfish.Clips (MidiClip)

-- v1: the shared library. `MidiClip` is all-concrete, so it JSON round-trips
-- directly — the envelope is just a `{ clips }` wrapper for forward headroom.
storeKey :: String
storeKey = "triggerfish.clips.v1"

type Envelope = { clips :: Array MidiClip }

foreign import _save :: String -> String -> Effect Unit
foreign import _load :: forall a. String -> Effect (Nullable a)
foreign import _stringify :: forall a. a -> String

-- | Persist the whole library (best-effort).
saveClips :: Array MidiClip -> Effect Unit
saveClips cs = _save storeKey (_stringify ({ clips: cs } :: Envelope))

-- | Load the library; an absent / unparseable store yields an empty library.
loadClips :: Effect (Array MidiClip)
loadClips = do
  m <- _load storeKey
  pure (fromMaybe [] (map _.clips (toMaybe (m :: Nullable Envelope))))
