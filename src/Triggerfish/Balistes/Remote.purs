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
  ) where

import Prelude

import Data.Array (mapMaybe)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Exception (Error)
import Triggerfish.Balistes.Lepidoptera (parsePattern)
import Triggerfish.Balistes.Pattern (FixedPattern)

foreign import fetchCollectionImpl
  :: String
  -> (Error -> Effect Unit)
  -> (Array String -> Effect Unit)
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
