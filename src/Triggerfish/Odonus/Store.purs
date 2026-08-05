-- | Triggerfish.Odonus.Store — localStorage persistence for Odonus. Persists
-- | the named **scene library** (the recallable presets) plus the current `live`
-- | working patch AND the unified glyph-chip **preset bank**, each as Lepidoptera
-- | eDSL text — the canonical, transferable form (a scene's text drops into
-- | Calypso / ships to purerl-tidal). The JSON here is only the local envelope
-- | around those eDSL texts; mirrors Selene's Store. (v2: was a single raw patch
-- | string; scenes became the preset library. v3: added the glyph-chip preset bank.)
-- |
-- | Captured CLIPS do NOT live here — they moved to the machine-agnostic shared
-- | library `Triggerfish.Clips.Store` (recording axis #27), so a clip captured in
-- | Odonus is pickable in Vetula and beyond.
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

-- | What we persist: the live working patch, the named scene library (each
-- | scene's `text` is its full authored patch rendered to eDSL), and the unified
-- | glyph-chip preset bank.
type Saved =
  { live :: String
  , scenes :: Array { name :: String, text :: String }
  , presets :: Array Preset
  }

-- | The on-disk shape (v3): presets as { content, name, starred } (empty `name` =
-- | anonymous). `content` is the preset's canonical patch text verbatim.
type Envelope =
  { live :: String
  , scenes :: Array { name :: String, text :: String }
  , presets :: Array { content :: String, name :: String, starred :: Boolean }
  }

-- | v2's on-disk shape — live + scenes, no preset bank. Migrated with an empty bank.
type EnvelopeV2 =
  { live :: String
  , scenes :: Array { name :: String, text :: String }
  }

-- v3: added the unified glyph-chip preset bank alongside the scene library.
storeKey :: String
storeKey = "triggerfish.odonus.patch.v3"

legacyKey :: String
legacyKey = "triggerfish.odonus.patch.v2"

foreign import _save :: String -> String -> Effect Unit
foreign import _load :: forall a. String -> Effect (Nullable a)
foreign import _stringify :: forall a. a -> String

-- | Persist the live patch + scene library + preset bank (best-effort — the FFI
-- | swallows quota / private-mode errors).
saveAll :: Saved -> Effect Unit
saveAll sv = _save storeKey (_stringify env)
  where
  env :: Envelope
  env =
    { live: sv.live
    , scenes: sv.scenes
    , presets: map (\p -> { content: p.content, name: fromMaybe "" p.name, starred: p.starred }) sv.presets
    }

-- | Load the stored envelope. Prefers v3; migrates a v2 (live + scenes) store with
-- | an empty preset bank; `Nothing` if absent / unparseable. The caller parses each
-- | `text` back through `Lepidoptera.parsePatch`.
loadAll :: Effect (Maybe Saved)
loadAll = do
  mEnv <- _load storeKey
  case toMaybe (mEnv :: Nullable Envelope) of
    Just env -> pure (Just (decode env))
    Nothing -> do
      mV2 <- _load legacyKey
      pure $ toMaybe (mV2 :: Nullable EnvelopeV2) <#> \v2 ->
        { live: v2.live, scenes: v2.scenes, presets: [] }

decode :: Envelope -> Saved
decode env =
  { live: env.live
  , scenes: env.scenes
  , presets: map (\e -> { content: e.content, name: if e.name == "" then Nothing else Just e.name, starred: e.starred }) env.presets
  }
