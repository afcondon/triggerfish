-- | Triggerfish.Balistes.Remote — the Amphora read client for the Balistes
-- | pattern library.
-- |
-- | "Get the patterns from the database": Balistes sources its fixed-rhythm
-- | library from the Amphora artefact store (`balistes-grid` favourite
-- | collection) rather than baking it in. The store hands back Lepidoptera
-- | strings (the canonical, hashable form); this module parses each back into a
-- | `FixedPattern`. If the store is unreachable the caller falls back to
-- | `bundledPatterns`.
-- |
-- | The JS side does the N+1 fetch (collection → each content by hash) and
-- | returns the raw payloads; PureScript stays pure and just parses.
module Triggerfish.Balistes.Remote
  ( fetchLibrary
  , publishPattern
  , publishSnapshot
  ) where

import Prelude

import Data.Array (mapMaybe)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Exception (Error)
import Triggerfish.Balistes.Lepidoptera (parsePattern, printPattern)
import Triggerfish.Balistes.Pattern (FixedPattern)

foreign import fetchCollectionImpl
  :: String
  -> (Error -> Effect Unit)
  -> (Array String -> Effect Unit)
  -> Effect Unit

foreign import publishSnapshotImpl
  :: String   -- payload (printTri text — any brain)
  -> String   -- label name (the preset's name, or its glyph alias if anonymous)
  -> String   -- display brain letter (G/R/T), rides along as a `brain:` tag
  -> String   -- source label
  -> (Error -> Effect Unit)
  -> (String -> Effect Unit)   -- resolves the content hash
  -> Effect Unit

foreign import publishPatternImpl
  :: String   -- payload (Lepidoptera)
  -> String   -- pattern name (JS derives genre + bpm tags from the trailing int)
  -> String   -- source label
  -> (Error -> Effect Unit)
  -> (String -> Effect Unit)   -- resolves the content hash
  -> Effect Unit

-- | Fetch the `balistes-grid` collection from Amphora and parse each payload.
-- | Malformed payloads are dropped (not fatal). Rejects if the store is
-- | unreachable — the caller treats that as "use the bundled fallback".
fetchLibrary :: Aff (Array FixedPattern)
fetchLibrary = do
  payloads <- makeAff \cb -> do
    fetchCollectionImpl "balistes-grid" (cb <<< Left) (cb <<< Right)
    pure nonCanceler
  pure (mapMaybe parsePattern payloads)

-- | Publish a pattern to Amphora: print it to its canonical Lepidoptera form,
-- | POST it as content (dedup by hash), label it (genre + bpm parsed from the
-- | name), and favourite it into `balistes-grid` so it round-trips on next load.
-- | Resolves the content hash. Rejects if the store is unreachable.
publishPattern :: FixedPattern -> Aff String
publishPattern p = makeAff \cb -> do
  publishPatternImpl (printPattern p) p.name "user" (cb <<< Left) (cb <<< Right)
  pure nonCanceler

-- | Save one BANK entry to Amphora: POST its `printTri` text as content (dedup by
-- | hash), label it, and favourite it into `balistes-bank` — the bank's own
-- | collection, distinct from the rhythm library's `balistes-grid` because the
-- | payloads are not interchangeable (see the note in Remote.js).
-- |
-- | Anonymous entries save happily: the caller passes `presetLabel`, which falls
-- | back to the content-derived glyph alias, so an unnamed capture arrives with a
-- | stable identity rather than a blank label (Amphora's `label.name` is NOT NULL).
-- | Naming is promotion, not a gate on sharing.
-- |
-- | Resolves the content hash. Rejects if the store is unreachable.
publishSnapshot :: String -> String -> String -> Aff String
publishSnapshot payload name brain = makeAff \cb -> do
  publishSnapshotImpl payload name brain "user" (cb <<< Left) (cb <<< Right)
  pure nonCanceler
