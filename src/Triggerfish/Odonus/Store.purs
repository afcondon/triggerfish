-- | Triggerfish.Odonus.Store — localStorage persistence for Odonus. Persists
-- | the named **scene library** (the recallable presets), the current `live`
-- | working patch, the unified glyph-chip **preset bank**, and the captured
-- | **clip harvest** (#151 R2d — self-contained looped note-spans lifted out of
-- | the logbook). Scenes/presets/live are Lepidoptera eDSL text — the canonical,
-- | transferable form (a scene's text drops into Calypso / ships to purerl-tidal);
-- | a clip is a rebased note buffer, so it's stored as its own record. The JSON
-- | here is the local envelope around all of it; mirrors Selene's Store. (v2: was
-- | a single raw patch string; scenes became the preset library. v3: added the
-- | glyph-chip preset bank. v4: added the persisted clip harvest.)
module Triggerfish.Odonus.Store
  ( Saved
  , saveAll
  , loadAll
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Triggerfish.Preset (Preset)
import Triggerfish.Odonus.Grid.Types (Clip)

-- | What we persist: the live working patch, the named scene library (each
-- | scene's `text` is its full authored patch rendered to eDSL), the unified
-- | glyph-chip preset bank, and the captured clip harvest.
type Saved =
  { live :: String
  , scenes :: Array { name :: String, text :: String }
  , presets :: Array Preset
  , clips :: Array Clip
  }

-- | The on-disk shape (v4): presets as { content, name, starred } (empty `name` =
-- | anonymous), plus the clip harvest stored as its own records (`Clip` is already
-- | all-concrete — name/events/lenMicros/patch — so it JSON round-trips directly).
-- | `content` is the preset's canonical patch text verbatim.
type Envelope =
  { live :: String
  , scenes :: Array { name :: String, text :: String }
  , presets :: Array { content :: String, name :: String, starred :: Boolean }
  , clips :: Array Clip
  }

-- | v3's on-disk shape — no clip harvest. Migrated with an empty clip list.
type EnvelopeV3 =
  { live :: String
  , scenes :: Array { name :: String, text :: String }
  , presets :: Array { content :: String, name :: String, starred :: Boolean }
  }

-- | v2's on-disk shape — live + scenes, no preset bank. Migrated with empty bank + clips.
type EnvelopeV2 =
  { live :: String
  , scenes :: Array { name :: String, text :: String }
  }

-- v4: added the persisted clip harvest alongside scenes + preset bank.
storeKey :: String
storeKey = "triggerfish.odonus.patch.v4"

v3Key :: String
v3Key = "triggerfish.odonus.patch.v3"

legacyKey :: String
legacyKey = "triggerfish.odonus.patch.v2"

foreign import _save :: String -> String -> Effect Unit
foreign import _load :: forall a. String -> Effect (Nullable a)
foreign import _stringify :: forall a. a -> String

-- | Persist the live patch + scene library + preset bank + clip harvest
-- | (best-effort — the FFI swallows quota / private-mode errors).
saveAll :: Saved -> Effect Unit
saveAll sv = _save storeKey (_stringify env)
  where
  env :: Envelope
  env =
    { live: sv.live
    , scenes: sv.scenes
    , presets: map (\p -> { content: p.content, name: fromMaybe "" p.name, starred: p.starred }) sv.presets
    , clips: sv.clips
    }

-- | Load the stored envelope. Prefers v4; migrates a v3 (no clips) with an empty
-- | clip harvest, and a v2 (live + scenes) with empty bank + clips; `Nothing` if
-- | absent / unparseable. The caller parses each scene/preset `text` back through
-- | `Lepidoptera.parsePatch`; clips are already concrete records.
loadAll :: Effect (Maybe Saved)
loadAll = do
  mEnv <- _load storeKey
  case toMaybe (mEnv :: Nullable Envelope) of
    Just env -> pure (Just (decode env))
    Nothing -> do
      mV3 <- _load v3Key
      case toMaybe (mV3 :: Nullable EnvelopeV3) of
        Just v3 -> pure (Just (decode { live: v3.live, scenes: v3.scenes, presets: v3.presets, clips: [] }))
        Nothing -> do
          mV2 <- _load legacyKey
          pure $ toMaybe (mV2 :: Nullable EnvelopeV2) <#> \v2 ->
            { live: v2.live, scenes: v2.scenes, presets: [], clips: [] }

decode :: Envelope -> Saved
decode env =
  { live: env.live
  , scenes: env.scenes
  , presets: map (\e -> { content: e.content, name: if e.name == "" then Nothing else Just e.name, starred: e.starred }) env.presets
  , clips: env.clips
  }
