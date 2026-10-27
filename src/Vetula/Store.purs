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
  , saveSound
  , loadSound
  , WorkingCopy
  , saveWorking
  , loadWorking
  , saveWelcomed
  , loadWelcomed
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Effect (Effect)
import Triggerfish.Preset (Preset, Stored, fromStored, toStored, withWords)

-- | One saved progression: its display label, its Tidal source (the chords), and
-- | whether it's a promoted keeper (★) or an ephemeral auto-capture (◦).
type Entry = { name :: String, keyLabel :: String, source :: String, kept :: Boolean, id :: String }

-- | An entry as stored: `id` is absent from one saved before ids.
type StoredEntry = { name :: String, keyLabel :: String, source :: String, kept :: Boolean, id :: Nullable String }

fromStoredEntry :: StoredEntry -> Entry
fromStoredEntry e = e { id = fromMaybe "" (toMaybe e.id) }

-- | What we persist: the whole library (newest-appended) + the preset bank.
type Saved = { library :: Array Entry, presets :: Array Preset }

-- | The on-disk shape (v2): presets as { content, name, starred } (empty `name` =
-- | anonymous). `content` is the preset's canonical progression source verbatim.
type Envelope =
  { library :: Array StoredEntry
  , presets :: Array Stored
  }

-- | v1's on-disk shape — library only, no preset bank. Migrated with an empty bank.
type EnvelopeV1 = { library :: Array StoredEntry }

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
    { library: map (\e -> e { id = toNullable (Just e.id) }) s.library
    , presets: map toStored s.presets
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
        { library: map fromStoredEntry v1.library, presets: [] }

decode :: Envelope -> Saved
decode env =
  { library: map fromStoredEntry env.library
  , presets: withWords (map fromStored env.presets)
  }

-- ---------------------------------------------------------------------------
-- Perform SESSION — the persistent container for saved scenes. One session per
-- working body-of-work: it RESUMES across app reloads (a reload must not
-- fragment a sitting), and is minted only on the very first launch.
-- `alias` is the session's monochrome glyph-triple identity
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

-- | Where this viewer chose to hear auditions (the bar's sound chip). Absent
-- | until they choose: then the page decides (the browser, or Continuo once
-- | the rig answers).
soundKey :: String
soundKey = "vetula.sound"

saveSound :: String -> Effect Unit
saveSound v = _save soundKey (_stringify v)

loadSound :: Effect (Maybe String)
loadSound = do
  m <- _load soundKey
  pure (toMaybe (m :: Nullable String))

-- | The progression being built, as it stands: saved locally on every change
-- | so a crash or a reload loses nothing (AC, 2026-10-06), but only the one,
-- | so it never piles up. Keeping is explicit (save), into the library.
-- | `saved` is the source last saved, so the page can say "unsaved".
type WorkingCopy = { name :: String, source :: String, saved :: String }

workingKey :: String
workingKey = "vetula.working"

-- | Whether this browser has seen the welcome card (CR-12). Storage that
-- | cannot be read says no, so a private window shows the card, never fails.
welcomedKey :: String
welcomedKey = "vetula.welcomed"

saveWelcomed :: Effect Unit
saveWelcomed = _save welcomedKey (_stringify true)

loadWelcomed :: Effect Boolean
loadWelcomed = do
  m <- _load welcomedKey
  pure (toMaybe (m :: Nullable Boolean) == Just true)

saveWorking :: WorkingCopy -> Effect Unit
saveWorking w = _save workingKey (_stringify w)

loadWorking :: Effect (Maybe WorkingCopy)
loadWorking = do
  m <- _load workingKey
  pure (toMaybe (m :: Nullable WorkingCopy))
