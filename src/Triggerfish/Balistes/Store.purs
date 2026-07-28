-- | Triggerfish.Balistes.Store — localStorage persistence for the whole Balistes
-- | artefact: the fixed-rhythm library **and** the ARRANGE rail (the tri-snapshot
-- | bank + sequence + bars-per-step). Everything saves/loads from this one surface
-- | (the "final panel" owns persistence); the per-tab panels hold no storage.
-- |
-- | Every payload is serialised as **eDSL / compact text**, never bespoke JSON —
-- | the Lepidoptera "save the rendering, not the structure" rule. A library entry
-- | is one `printPattern`; a bank slot is one `printTri` (a brain-tagged text that
-- | drops straight into Calypso for the Fixed case). The JSON here is only the
-- | local envelope holding those texts. Mirrors Selene's Store.
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
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Triggerfish.Balistes.Lepidoptera (parsePattern, printPattern)
import Triggerfish.Balistes.Pattern (FixedPattern)
import Triggerfish.Balistes.TriSnapshot (TriSnapshot, parseTri, printTri)

-- | What a session restores: the library + the ARRANGE rail. `bank` is the
-- | fixed-length slot array (`Nothing` = empty slot).
type Saved =
  { library :: Array FixedPattern
  , bank :: Array (Maybe TriSnapshot)
  , sequence :: Array Int
  , seqBars :: Int
  }

-- | The on-disk shape: every payload flattened to text (`""` = empty bank slot).
type Envelope =
  { library :: Array String
  , bank :: Array String
  , sequence :: Array Int
  , seqBars :: Int
  }

-- v3: the envelope grew from library-only (v2) to library + ARRANGE rail.
storeKey :: String
storeKey = "triggerfish.balistes.v3"

-- v2 stored the library alone as an `Array String`; recovered if v3 is absent.
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
    , bank: map (maybe "" printTri) s.bank
    , sequence: s.sequence
    , seqBars: s.seqBars
    }

-- | Load the artefact. Prefers the v3 envelope; if absent, migrates a v2
-- | library-only store (empty ARRANGE rail). `Nothing` → the bundled fallback.
-- | Unparseable library entries / bank slots are dropped, never fatal.
load :: Effect (Maybe Saved)
load = do
  mEnv <- _load storeKey
  case toMaybe (mEnv :: Nullable Envelope) of
    Just env -> pure (Just (decode env))
    Nothing -> do
      mLib <- _load legacyLibraryKey
      pure $ toMaybe (mLib :: Nullable (Array String)) <#> \texts ->
        { library: mapMaybe parsePattern texts, bank: [], sequence: [], seqBars: 1 }

decode :: Envelope -> Saved
decode env =
  { library: mapMaybe parsePattern env.library
  , bank: map slot env.bank
  , sequence: env.sequence
  , seqBars: env.seqBars
  }
  where
  slot t = if t == "" then Nothing else parseTri t
