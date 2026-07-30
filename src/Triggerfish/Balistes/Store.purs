-- | Triggerfish.Balistes.Store — localStorage persistence for the whole Balistes
-- | artefact: the fixed-rhythm library **and** the PRESET bank (the unified preset
-- | list + sequence + bars-per-step). Everything saves/loads from this one surface
-- | (the "final panel" owns persistence); the per-tab panels hold no storage.
-- |
-- | Each preset's `content` is already eDSL / compact TEXT (a brain-tagged `printTri`
-- | for a Balistes snapshot) — the Lepidoptera "save the rendering" rule; `name` and
-- | `starred` are small envelope metadata. A library entry is one `printPattern`.
-- | The JSON here is only the local envelope holding those texts. Mirrors Selene.
-- |
-- | Transient playback state (`seqEnabled`/`seqPos`/`seqStartBar`) is deliberately
-- | NOT saved — reopening the app should restore the arrangement, not start it.
module Triggerfish.Balistes.Store
  ( Saved
  , save
  , load
  ) where

import Prelude

import Data.Array (mapMaybe)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Triggerfish.Balistes.Lepidoptera (parsePattern, printPattern)
import Triggerfish.Balistes.Pattern (FixedPattern)
import Triggerfish.Preset (Preset)

-- | What a session restores: the library + the PRESET bank (unified list).
type Saved =
  { library :: Array FixedPattern
  , presets :: Array Preset
  , sequence :: Array Int
  , seqBars :: Int
  }

-- | The on-disk shape (v4): library as texts, presets as { content, name, starred }
-- | (empty `name` = anonymous). `content` is the preset's canonical text verbatim.
type Envelope =
  { library :: Array String
  , presets :: Array { content :: String, name :: String, starred :: Boolean }
  , sequence :: Array Int
  , seqBars :: Int
  }

-- | v3's on-disk shape — a fixed bank of `printTri` texts (`""` = empty slot).
-- | Migrated to the unified preset list when v4 is absent.
type EnvelopeV3 =
  { library :: Array String
  , bank :: Array String
  , sequence :: Array Int
  , seqBars :: Int
  }

-- v4: the fixed Maybe-bank became a growing unified preset list (name + starred).
storeKey :: String
storeKey = "triggerfish.balistes.v4"

legacyBankKey :: String
legacyBankKey = "triggerfish.balistes.v3"

-- v2 stored the library alone as an `Array String`; recovered if v3/v4 absent.
legacyLibraryKey :: String
legacyLibraryKey = "triggerfish.balistes.library.v2"

foreign import _save :: String -> String -> Effect Unit
foreign import _load :: forall a. String -> Effect (Nullable a)
foreign import _stringify :: forall a. a -> String

-- | Persist the whole artefact (best-effort — the FFI swallows quota / private-mode
-- | errors), each payload rendered to its canonical text.
save :: Saved -> Effect Unit
save s = _save storeKey (_stringify env)
  where
  env :: Envelope
  env =
    { library: map printPattern s.library
    , presets: map (\p -> { content: p.content, name: fromMaybe "" p.name, starred: p.starred }) s.presets
    , sequence: s.sequence
    , seqBars: s.seqBars
    }

-- | Load the artefact. Prefers v4; migrates a v3 fixed-bank store (each non-empty
-- | slot → an anonymous preset), else a v2 library-only store. `Nothing` → the
-- | bundled fallback. Unparseable library entries are dropped, never fatal.
load :: Effect (Maybe Saved)
load = do
  mEnv <- _load storeKey
  case toMaybe (mEnv :: Nullable Envelope) of
    Just env -> pure (Just (decode env))
    Nothing -> do
      mV3 <- _load legacyBankKey
      case toMaybe (mV3 :: Nullable EnvelopeV3) of
        Just v3 -> pure (Just (decodeV3 v3))
        Nothing -> do
          mLib <- _load legacyLibraryKey
          pure $ toMaybe (mLib :: Nullable (Array String)) <#> \texts ->
            { library: mapMaybe parsePattern texts, presets: [], sequence: [], seqBars: 1 }

decode :: Envelope -> Saved
decode env =
  { library: mapMaybe parsePattern env.library
  , presets: map (\e -> { content: e.content, name: if e.name == "" then Nothing else Just e.name, starred: e.starred }) env.presets
  , sequence: env.sequence
  , seqBars: env.seqBars
  }

decodeV3 :: EnvelopeV3 -> Saved
decodeV3 v3 =
  { library: mapMaybe parsePattern v3.library
  , presets: map (\t -> { content: t, name: Nothing, starred: false }) (filter (_ /= "") v3.bank)
  , sequence: v3.sequence
  , seqBars: v3.seqBars
  }
  where
  filter p = mapMaybe \x -> if p x then Just x else Nothing
