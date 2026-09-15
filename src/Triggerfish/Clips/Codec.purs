-- | `Triggerfish.Clips.Codec` — **one definition of a clip's serialised shape.**
-- |
-- | Extracted from `Clips.Store` when clips gained a second destination: they go
-- | to localStorage for the library, and to Amphora for anything outside this
-- | origin — Quadrat, chiefly, which is a different page on a different port and
-- | cannot see our localStorage at all.
-- |
-- | ⚠️ PureScript `Maybe` does NOT survive `JSON.stringify`/`parse`. `Just`/
-- | `Nothing` rely on constructor identity that a plain JSON round-trip
-- | destroys, so a decoded `Maybe` field fails its pattern match at read time.
-- | The serialised shape therefore uses `Nullable` for the optional fields —
-- | `null` ⇔ `Nothing`, a bare value ⇔ `Just` — converting at the boundary.
-- | That bug cost a store version once (`triggerfish.clips.v1` → `v2`, with a
-- | salvage path); having the conversion in ONE place is how it stays paid for.
module Triggerfish.Clips.Codec
  ( StoredClip
  , toStored
  , fromStored
  , encode
  , decode
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)

import Triggerfish.Clips (MidiClip, NoteEvent)

-- | The JSON-safe clip: `MidiClip` with its `Maybe` fields as `Nullable`.
type StoredClip =
  { id :: String
  , events :: Array NoteEvent
  , lenMicros :: Number
  , heads :: Int
  , capturedMicros :: Number
  , source :: String
  , name :: String
  , tags :: Array String
  , notes :: String
  , bpm :: Nullable Number
  , key :: Nullable String
  , context :: Nullable String
  }

toStored :: MidiClip -> StoredClip
toStored c =
  { id: c.id, events: c.events, lenMicros: c.lenMicros, heads: c.heads
  , capturedMicros: c.capturedMicros, source: c.source, name: c.name
  , tags: c.tags, notes: c.notes
  , bpm: toNullable c.bpm, key: toNullable c.key, context: toNullable c.context }

fromStored :: StoredClip -> MidiClip
fromStored s =
  { id: s.id, events: s.events, lenMicros: s.lenMicros, heads: s.heads
  , capturedMicros: s.capturedMicros, source: s.source, name: s.name
  , tags: s.tags, notes: s.notes
  , bpm: toMaybe s.bpm, key: toMaybe s.key, context: toMaybe s.context }

-- | A clip as canonical text. Amphora content-addresses the payload, so two
-- | publishes of the same clip must produce the same bytes — which they do,
-- | since the shape is a record with a fixed field order.
encode :: MidiClip -> String
encode = _stringify <<< toStored

-- | Read one back. `Nothing` when the text is not a clip at all.
decode :: String -> Maybe MidiClip
decode = map fromStored <<< toMaybe <<< _parse

foreign import _stringify :: StoredClip -> String
foreign import _parse :: String -> Nullable StoredClip
