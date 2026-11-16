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
  , Keys
  , odonusKeys
  , melichthysKeys
  , saveLive
  , loadAll
  , saveUnison
  , loadUnison
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Data.Array (filter, notElem)
import Triggerfish.Preset (Preset, Stored, fromStored, newPreset, withWords)

-- | What we persist: the live working patch and the preset bank. (The named
-- | scene library was folded into the bank as named presets, 2026-10-13: the
-- | two held the same thing. Stored scenes are folded in on load and the
-- | field is written empty.)
type Saved =
  { live :: String
  , presets :: Array Preset
  }

-- | The on-disk shape (v3): presets as { content, name, starred } (empty `name` =
-- | anonymous). `content` is the preset's canonical patch text verbatim.
type Envelope =
  { live :: String
  , scenes :: Array { name :: String, text :: String }
  , presets :: Array Stored
  }

-- | v2's on-disk shape — live + scenes, no preset bank. Migrated with an empty bank.
type EnvelopeV2 =
  { live :: String
  , scenes :: Array { name :: String, text :: String }
  }

-- | Where a machine keeps itself in this browser. Odonus and Melichthys are
-- | one component (`Odonus.Grid`) being two machines, each with its own.
-- | `legacy` and `legacyUnison` are Odonus's older keys, read once.
type Keys = { patch :: String, legacy :: Maybe String, unison :: String, legacyUnison :: Maybe String, unisonDefault :: Int }

-- v3: added the unified glyph-chip preset bank alongside the scene library.
odonusKeys :: Keys
odonusKeys =
  { patch: "triggerfish.odonus.patch.v3", legacy: Just "triggerfish.odonus.patch.v2"
  , unison: unisonKey, legacyUnison: Just legacyUnisonKey, unisonDefault: 59 }

-- | Melichthys: UNISON copies direction and speed until a choice is made.
melichthysKeys :: Keys
melichthysKeys =
  { patch: "triggerfish.melichthys.patch.v1", legacy: Nothing
  , unison: "triggerfish.melichthys.unison.v1", legacyUnison: Nothing, unisonDefault: 34 }

foreign import _save :: String -> String -> Effect Unit
foreign import _load :: forall a. String -> Effect (Nullable a)
foreign import _stringify :: forall a. a -> String

-- | Persist the live patch (best-effort — the FFI swallows quota /
-- | private-mode errors). The presets are Amphora's (`Triggerfish.Library`,
-- | 2026-10-13): what this store held of them is read once, to start it.
saveLive :: Keys -> String -> Effect Unit
saveLive keys live = _save keys.patch (_stringify env)
  where
  env :: Envelope
  env = { live, scenes: [], presets: [] }

-- | Load the stored envelope. Prefers v3; migrates a v2 (live + scenes) store with
-- | an empty preset bank; `Nothing` if absent / unparseable. The caller parses each
-- | `text` back through `Lepidoptera.parsePatch`.
loadAll :: Keys -> Effect (Maybe Saved)
loadAll keys = do
  mEnv <- _load keys.patch
  case toMaybe (mEnv :: Nullable Envelope), keys.legacy of
    Just env, _ -> pure (Just (decode env))
    Nothing, Nothing -> pure Nothing
    Nothing, Just legacyKey -> do
      mV2 <- _load legacyKey
      pure $ toMaybe (mV2 :: Nullable EnvelopeV2) <#> \v2 ->
        { live: v2.live, presets: withWords (foldScenes v2.scenes []) }

decode :: Envelope -> Saved
decode env =
  { live: env.live
  , presets: withWords (foldScenes env.scenes (map fromStored env.presets))
  }

-- | Scenes as named presets after the bank's own, each once: a scene whose
-- | patch the bank already holds is not kept twice.
foldScenes :: Array { name :: String, text :: String } -> Array Preset -> Array Preset
foldScenes scenes presets =
  presets <> map (\sc -> newPreset sc.text (Just sc.name))
    (filter (\sc -> notElem sc.text (map _.content presets)) scenes)

-- | Which groups UNISON copies from head I (a `Reef.Odonus.unisonBit` mask),
-- | kept per browser so a choice made once holds (AC, 2026-10-14).
unisonKey :: String
unisonKey = "triggerfish.odonus.unison.v2"

-- | Before DIRECTION was split from PATTERN, PAT copied both.
legacyUnisonKey :: String
legacyUnisonKey = "triggerfish.odonus.unison"

saveUnison :: Keys -> Int -> Effect Unit
saveUnison keys m = _save keys.unison (_stringify m)

-- | Every group but INT until a choice is made: the voices keep their intervals.
-- | A choice saved before the split keeps its meaning: PAT lit lights DIR too.
loadUnison :: Keys -> Effect Int
loadUnison keys = do
  mask <- toMaybe <$> _load keys.unison
  case mask, keys.legacyUnison of
    Just m, _ -> pure m
    Nothing, Nothing -> pure keys.unisonDefault
    Nothing, Just legacyKey -> do
      legacy <- toMaybe <$> _load legacyKey
      pure case legacy of
        Just m | m `mod` 2 == 1 -> m + 32
        Just m -> m
        Nothing -> keys.unisonDefault
