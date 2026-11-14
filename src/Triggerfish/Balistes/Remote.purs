-- | Triggerfish.Balistes.Remote — the Amphora read client for the Balistes
-- | pattern library.
-- |
-- | "Get the patterns from the database": Balistes sources its fixed-rhythm
-- | library from the Amphora artefact store (`balistes-grid` favourite
-- | collection) rather than baking it in. The store hands back Lepidoptera
-- | strings (the canonical, hashable form); this module parses each back into a
-- | `FixedPattern`. The name is in the payload, so the labels are not read.
module Triggerfish.Balistes.Remote
  ( fetchLibrary
  ) where

import Prelude

import Data.Array (mapMaybe)
import Effect.Aff (Aff)
import Reef.Balistes.Pattern (FixedPattern)
import Triggerfish.Amphora as Amphora
import Triggerfish.Balistes.Lepidoptera (parsePattern)

-- | Fetch the `balistes-grid` collection from Amphora and parse each payload.
-- | Malformed payloads are dropped (not fatal). Rejects if the store is
-- | unreachable.
fetchLibrary :: Aff (Array FixedPattern)
fetchLibrary = mapMaybe parsePattern <$> Amphora.payloads "balistes-grid"
