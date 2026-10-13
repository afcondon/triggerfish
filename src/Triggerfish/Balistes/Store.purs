-- | Triggerfish.Balistes.Store — the Balistes bank as this browser kept it in
-- | localStorage before Amphora kept it (`Triggerfish.Library`, 2026-10-13):
-- | read, never written, to start the Library the first time. ONE bank
-- | holding every brain's artefacts.
-- |
-- | Each entry's `content` is canonical TEXT — a brain-tagged `printTri` — per the
-- | Lepidoptera "save the rendering, not bespoke structure" rule. `name` and
-- | `starred` are envelope metadata. The JSON here is only the local wrapper
-- | holding those texts. Mirrors Selene.
-- |
-- | **v5: the rhythm library folded into the bank.** Until v4 there were two
-- | parallel collections — `library` (RYTM rhythms: named, editable, publishable)
-- | and `presets` (snapshots of any brain: anonymous, glyphed). Everything RYTM
-- | could do and the other two brains could not came from that split. Now there is
-- | one collection and a rhythm is simply an entry whose content parses to
-- | `TSFixed`. See docs/DESIGN-balistes-bank-coherence.md.
-- |
-- | **The name lives in exactly one place: `name` here.** A rhythm's canonical
-- | text embeds its name (`balistesPattern "lo house 110" 32 …`), so folding
-- | naively would give every rhythm two names that drift apart on rename. Instead
-- | the stored content is NAME-STRIPPED (`balistesPattern "" 32 …`) and the name
-- | is injected back when a pattern is handed out — to the editor, to Amphora, to
-- | Calypso. Two things follow, both improvements:
-- |
-- |   * The glyph fingerprints the SOUND. Renaming no longer changes the content
-- |     hash, so "identical state ⇒ identical glyph" is finally true for rhythms
-- |     as the bank always claimed it was.
-- |   * There is one rename path for all three brains.
-- |
-- | Verified against all 14 published rhythms: parse → strip → print → parse →
-- | re-inject reproduces the original text byte-for-byte.
-- | (`rhythmContent` / `rhythmOfContent` live in `Triggerfish.Balistes.TriSnapshot`
-- | with the rest of the brain-tag vocabulary; import them from there.)
module Triggerfish.Balistes.Store
  ( Saved
  , load
  ) where

import Prelude

import Data.Array (mapMaybe)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Triggerfish.Balistes.Lepidoptera (parsePattern)
import Triggerfish.Balistes.TriSnapshot (TriSnapshot(..), parseTri, rhythmContent)
import Triggerfish.Preset (Preset, Stored, fromStored, withWords)

-- | What a session restores: the one bank.
type Saved = { presets :: Array Preset }

-- | The on-disk shape (v5): one list of { content, name, starred }. Empty `name`
-- | = anonymous (a capture rather than a promoted artefact).
type Envelope =
  { presets :: Array Stored }

-- | v4 — the two-collection shape this replaces.
type EnvelopeV4 =
  { library :: Array String
  , presets :: Array { content :: String, name :: String, starred :: Boolean }
  }

storeKey :: String
storeKey = "triggerfish.balistes.v5"

-- Deliberately still read, never written. A v5 store that turns out wrong can be
-- diagnosed against the untouched v4 payload sitting beside it.
legacyV4Key :: String
legacyV4Key = "triggerfish.balistes.v4"

foreign import _load :: forall a. String -> Effect (Nullable a)

-- ---------------------------------------------------------------------------
-- Persist
-- ---------------------------------------------------------------------------

-- | A rhythm saved before names moved into the envelope (2026-08) carries its
-- | name only in its text, and showed as "untitled": the name is lifted out,
-- | as `migrateRhythm` does for v4.
recoverName :: Preset -> Preset
recoverName p = case p.name, parseTri p.content of
  Nothing, Just (TSFixed pat) | pat.name /= "" ->
    p { content = rhythmContent pat, name = Just pat.name }
  _, _ -> p

-- | Load the bank. Prefers v5; otherwise migrates v4's rhythm library.
-- |
-- | The migration keeps RHYTHMS ONLY and drops v4's old snapshot bank — sanctioned
-- | by AC (2026-08-07), who is about to enter a lot of new material and did not
-- | want the fold held hostage to preserving scratch captures. **v4 is read, never
-- | written**, so its payload survives beside the v5 store and a bad fold is
-- | recoverable by hand rather than gone.
load :: Effect (Maybe Saved)
load = do
  mEnv <- _load storeKey
  case toMaybe (mEnv :: Nullable Envelope) of
    Just env -> pure (Just { presets: withWords (map (recoverName <<< fromStored) env.presets) })
    Nothing -> do
      mV4 <- _load legacyV4Key
      pure $ toMaybe (mV4 :: Nullable EnvelopeV4) <#> \v4 ->
        { presets: withWords (mapMaybe migrateRhythm v4.library) }

-- | One v4 library text → one named bank entry. The name is lifted OUT of the
-- | text and into the envelope, leaving name-stripped content, so the fold
-- | establishes the single-source-of-truth invariant rather than inheriting the
-- | duplication. Unparseable entries are dropped — they were already being
-- | dropped by v4's own `mapMaybe parsePattern` on every load.
migrateRhythm :: String -> Maybe Preset
migrateRhythm text = parsePattern text <#> \p ->
  { content: rhythmContent p
  , name: if p.name == "" then Nothing else Just p.name
  , starred: false
  , word: Nothing
  , was: []
  , id: ""
  }
