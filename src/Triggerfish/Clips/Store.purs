-- | `Triggerfish.Clips.Store` — localStorage persistence for the SHARED MIDI clip
-- | library (recording axis #27). Machine-agnostic: every capturing machine
-- | (Odonus now, Vetula at #28, later others) reads/writes this one store, so a
-- | clip captured anywhere is pickable everywhere.
-- |
-- | The `Maybe`-does-not-survive-JSON conversion lives in `Clips.Codec` now, so
-- | one definition serves this store and the Amphora publish that carries a clip
-- | off this origin entirely.
module Triggerfish.Clips.Store
  ( saveClips
  , loadClips
  ) where

import Prelude

import Data.Array (null)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Triggerfish.Clips (MidiClip, NoteEvent)
import Triggerfish.Clips.Codec (StoredClip, fromStored, toStored)

-- v2: optional fields stored as Nullable so they round-trip through JSON. (v1 stored
-- them as PureScript `Maybe`, which corrupts on read — migrated below.)
storeKey :: String
storeKey = "triggerfish.clips.v2"

v1Key :: String
v1Key = "triggerfish.clips.v1"

type Envelope = { clips :: Array StoredClip }

-- | The salvageable subset of a v1 clip: every JSON-SAFE field (all but the three
-- | corrupt `Maybe`s). Reading v1 through this drops the malformed metadata but keeps
-- | the notes + name intact — a clip's music survives the format fix.
type V1Clip =
  { id :: String
  , events :: Array NoteEvent
  , lenMicros :: Number
  , heads :: Int
  , capturedMicros :: Number
  , source :: String
  , name :: String
  , tags :: Array String
  , notes :: String
  }

type V1Envelope = { clips :: Array V1Clip }

foreign import _save :: String -> String -> Effect Unit
foreign import _load :: forall a. String -> Effect (Nullable a)
foreign import _stringify :: forall a. a -> String

-- | Salvage a v1 clip: keep its notes/name, reset the corrupt metadata to absent.
fromV1 :: V1Clip -> MidiClip
fromV1 v =
  { id: v.id, events: v.events, lenMicros: v.lenMicros, heads: v.heads
  , capturedMicros: v.capturedMicros, source: v.source, name: v.name
  , tags: v.tags, notes: v.notes
  , bpm: Nothing, key: Nothing, context: Nothing }

-- | Persist the whole library (best-effort — the FFI swallows quota/private-mode).
saveClips :: Array MidiClip -> Effect Unit
saveClips cs = _save storeKey (_stringify ({ clips: map toStored cs } :: Envelope))

-- | Load the library. Prefers v2; if absent, salvages a v1 store (dropping its
-- | corrupt metadata) and rewrites it as v2 so the migration happens once. An
-- | absent/unparseable store yields an empty library.
loadClips :: Effect (Array MidiClip)
loadClips = do
  m2 <- _load storeKey
  case toMaybe (m2 :: Nullable Envelope) of
    Just env -> pure (map fromStored env.clips)
    Nothing -> do
      m1 <- _load v1Key
      case toMaybe (m1 :: Nullable V1Envelope) of
        Just env1 -> do
          let salvaged = map fromV1 env1.clips
          _ <- if null salvaged then pure unit else saveClips salvaged
          pure salvaged
        Nothing -> pure []
