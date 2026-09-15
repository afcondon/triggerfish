-- | `Triggerfish.Clips.Share` — **a clip, off this origin.**
-- |
-- | The library lives in localStorage, which is per-origin: Triggerfish is
-- | `:3023` and Quadrat is `:3029`, so Quadrat cannot see a single clip of it,
-- | ever. Amphora (`:3024`, permissive CORS, content-addressed) is the shared
-- | store both pages already reach, so a clip destined for anything outside this
-- | page goes there.
-- |
-- | ## Why a clip, and not a progression
-- |
-- | A settled path through a rehearsal and a phrase marked in Review are the
-- | same object at different fidelity: a note stream with known times. `MidiClip`
-- | already is that, already carries `source :: "odonus" | "vetula" | …`, and
-- | already keeps every event. So one format covers both, and Quadrat reads one
-- | collection rather than learning what a progression is.
-- |
-- | ## Why this beats being overheard
-- |
-- | Quadrat's other way of learning what was played is to listen to the MIDI
-- | wire and cluster notes within 50 ms — necessary for Progressions, which is a
-- | black box on an iPad. Triggerfish is not a black box: it knows the notes,
-- | their order and their times before one sounds. Declared beats inferred, and
-- | the same clustering applied to DECLARED times is exact rather than a
-- | detector's best guess.
-- |
-- | The metadata rides on the LABEL rather than the payload — Amphora's own
-- | convention, and the reason the payload can stay a plain clip.
module Triggerfish.Clips.Share
  ( shareCollection
  , shareSpec
  ) where

import Prelude

import Data.Array (catMaybes)
import Data.Maybe (Maybe(..))

import Triggerfish.Amphora (PublishSpec)
import Triggerfish.Clips (MidiClip)
import Triggerfish.Clips.Codec (encode)

-- | Machine-agnostic on purpose: Odonus's marked phrases and Vetula's settled
-- | paths land in the same place, and a reader picks by the `source:` tag rather
-- | than by knowing which app wrote it.
shareCollection :: String
shareCollection = "triggerfish-clips"

-- | What to publish for one clip. `kind` and `key` are what a sampler needs to
-- | know before it records: how the material should be divided, and what it is
-- | in. Both are declarations, not guesses — which is the whole point.
shareSpec :: MidiClip -> { kind :: String, glyph :: String } -> PublishSpec
shareSpec clip extra =
  { kind: "triggerfish-clip"
  , collection: shareCollection
  , name: if clip.name == "" then clip.id else clip.name
  , source: clip.source
  , payload: encode clip
  , tags:
      [ "source:" <> clip.source
      , "kind:" <> extra.kind
      , "rebus:" <> extra.glyph
      ]
        <> catMaybes
             [ map (\k -> "key:" <> k) clip.key
             , map (\b -> "bpm:" <> show b) clip.bpm
             ]
        <> clip.tags
  }
