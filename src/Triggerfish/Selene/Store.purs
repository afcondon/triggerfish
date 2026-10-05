-- | Triggerfish.Selene.Store — localStorage persistence for the Selene rack
-- | library **and** the unified PRESET bank (the glyph-chip capture list). A
-- | rack's canonical form is its **eDSL doc text** (the SOURCE pane authority,
-- | `printRack`/`parseRack`); a saved rack is just `{name, doc}`, and the library
-- | is a named collection of them. A preset's `content` is likewise the recallable
-- | rack-doc text verbatim (the Lepidoptera "save the rendering" rule); `name` and
-- | `starred` are small envelope metadata. The JSON here is only the *local
-- | envelope* around those texts — the transferable unit is a single rack's `doc`
-- | (copy it into Calypso, which speaks the same dialect). Mirrors Balistes' Store.
module Triggerfish.Selene.Store
  ( Rack
  , Saved
  , saveLibrary
  , loadLibrary
  , saveLive
  , loadLive
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Triggerfish.Preset (Preset)

-- | A named rack: its eDSL doc is the whole rack, rendered.
type Rack = { name :: String, doc :: String }

-- | What we persist: the library, which rack was open, and the preset bank.
type Saved = { active :: Int, library :: Array Rack, presets :: Array Preset }

-- | The on-disk shape (v2): presets as { content, name, starred } (empty `name` =
-- | anonymous). `content` is the preset's canonical rack-doc text verbatim.
type Envelope =
  { active :: Int
  , library :: Array Rack
  , presets :: Array { content :: String, name :: String, starred :: Boolean }
  }

-- | v1's on-disk shape — library + active only, no presets. Migrated with an
-- | empty bank when v2 is absent.
type EnvelopeV1 = { active :: Int, library :: Array Rack }

-- v2: added the unified preset bank alongside the rack library.
storeKey :: String
storeKey = "triggerfish.selene.library.v2"

legacyKey :: String
legacyKey = "triggerfish.selene.library.v1"

foreign import _save :: String -> String -> Effect Unit
foreign import _load :: forall a. String -> Effect (Nullable a)
foreign import _stringify :: forall a. a -> String

-- | Persist the library + preset bank (best-effort — FFI swallows quota/private-mode
-- | errors), each payload rendered to its canonical text.
saveLibrary :: Saved -> Effect Unit
saveLibrary s = _save storeKey (_stringify env)
  where
  env :: Envelope
  env =
    { active: s.active
    , library: s.library
    , presets: map (\p -> { content: p.content, name: fromMaybe "" p.name, starred: p.starred }) s.presets
    }

-- | Load the stored library. Prefers v2; migrates a v1 (library-only) store with an
-- | empty preset bank; `Nothing` if absent / unparseable.
loadLibrary :: Effect (Maybe Saved)
loadLibrary = do
  mEnv <- _load storeKey
  case toMaybe (mEnv :: Nullable Envelope) of
    Just env -> pure (Just (decode env))
    Nothing -> do
      mV1 <- _load legacyKey
      pure $ toMaybe (mV1 :: Nullable EnvelopeV1) <#> \v1 ->
        { active: v1.active, library: v1.library, presets: [] }

decode :: Envelope -> Saved
decode env =
  { active: env.active
  , library: env.library
  , presets: map (\e -> { content: e.content, name: if e.name == "" then Nothing else Just e.name, starred: e.starred }) env.presets
  }

-- | The live rack, kept apart from the saved ones (2026-10-05): what the
-- | modular was last given, which may have moved away from any saved rack.
liveKey :: String
liveKey = "triggerfish.selene.live.v1"

saveLive :: String -> Effect Unit
saveLive doc = _save liveKey (_stringify { doc })

loadLive :: Effect (Maybe String)
loadLive = do
  m <- _load liveKey
  pure (map _.doc (toMaybe (m :: Nullable { doc :: String })))
