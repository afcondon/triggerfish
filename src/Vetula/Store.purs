-- | Vetula.Store — localStorage persistence for the progression library (the
-- | auto-capture stack) AND the unified glyph-chip **preset bank**. Following the
-- | "save the rendering, not bespoke structure" rule (cf. Triggerfish.Selene.Store):
-- | a library entry's — and a preset's — canonical form is its **Tidal source text**,
-- | the transferable unit that parses back to note-lists. So the persisted envelope
-- | is all strings + flags; no ChordNode / Key / Mode codecs, and it round-trips
-- | through the same render/parse the app already uses for copy-paste and import.
-- | (v2: added the glyph-chip preset bank alongside the library.)
module Vetula.Store
  ( Entry
  , Saved
  , saveLibrary
  , loadLibrary
  , SessionState
  , saveSession
  , loadSession
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Triggerfish.Preset (Preset)

-- | One saved progression: its display label, its Tidal source (the chords), and
-- | whether it's a promoted keeper (★) or an ephemeral auto-capture (◦).
type Entry = { name :: String, keyLabel :: String, source :: String, kept :: Boolean }

-- | What we persist: the whole library (newest-appended) + the preset bank.
type Saved = { library :: Array Entry, presets :: Array Preset }

-- | The on-disk shape (v2): presets as { content, name, starred } (empty `name` =
-- | anonymous). `content` is the preset's canonical progression source verbatim.
type Envelope =
  { library :: Array Entry
  , presets :: Array { content :: String, name :: String, starred :: Boolean }
  }

-- | v1's on-disk shape — library only, no preset bank. Migrated with an empty bank.
type EnvelopeV1 = { library :: Array Entry }

-- v2: added the unified glyph-chip preset bank alongside the library.
storeKey :: String
storeKey = "triggerfish.vetula.library.v2"

legacyKey :: String
legacyKey = "triggerfish.vetula.library.v1"

foreign import _save :: String -> String -> Effect Unit
foreign import _load :: forall a. String -> Effect (Nullable a)
foreign import _stringify :: forall a. a -> String

-- | Persist the library + preset bank (best-effort — FFI swallows quota /
-- | private-mode errors).
saveLibrary :: Saved -> Effect Unit
saveLibrary s = _save storeKey (_stringify env)
  where
  env :: Envelope
  env =
    { library: s.library
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
        { library: v1.library, presets: [] }

decode :: Envelope -> Saved
decode env =
  { library: env.library
  , presets: map (\e -> { content: e.content, name: if e.name == "" then Nothing else Just e.name, starred: e.starred }) env.presets
  }

-- ---------------------------------------------------------------------------
-- Perform SESSION — the persistent container for saved scenes (see the Perform
-- surface). One session per working body-of-work: it RESUMES across app reloads
-- (a reload must not fragment a sitting), and only an explicit "new session"
-- mints a fresh one. `alias` is the session's monochrome glyph-triple identity
-- (`Glyph.sessionAliasOf`); `name` is an optional promotion ("" = use the alias);
-- `nextScene` is the monotonic scene counter within this session.
-- ---------------------------------------------------------------------------

type SessionState = { alias :: String, name :: String, nextScene :: Int }

sessionKey :: String
sessionKey = "triggerfish.vetula.session.v1"

saveSession :: SessionState -> Effect Unit
saveSession s = _save sessionKey (_stringify s)

loadSession :: Effect (Maybe SessionState)
loadSession = do
  m <- _load sessionKey
  pure (toMaybe (m :: Nullable SessionState))
