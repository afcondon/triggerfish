-- | Triggerfish.Amphora — the shared Amphora artefact-store client.
-- |
-- | One content-addressed read/write dance reused by every editor whose library
-- | is a named collection of eDSL / Tidal text: Odonus scenes, Selene racks,
-- | Vetula progressions. (Balistes predates this and keeps its own name-in-
-- | payload `Remote`; this module is the superset it could fold onto.)
-- |
-- | The transferable unit of content is a payload STRING (the canonical
-- | Lepidoptera / Tidal rendering the editor already round-trips). The display
-- | name and any extra metadata ride on the LABEL — so recall works whether or
-- | not the eDSL embeds a name. The JS side does the favourite→content→label
-- | join and the guarded three-POST publish; PureScript stays pure.
-- |
-- | Every call rejects if the store is unreachable — callers treat that as
-- | "fall back to the local library" (localStorage / bundled), never fatal.
module Triggerfish.Amphora
  ( LibItem
  , PublishSpec
  , fetchCollection
  , publish
  , unpublish
  ) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Exception (Error)

-- | One recalled artefact: its content hash, the label's display name, the
-- | payload (canonical text), and the label's tags (extra metadata, e.g.
-- | `key:C major` / `kept` / `bpm:122`).
type LibItem =
  { hash :: String
  , name :: String
  , payload :: String
  , tags :: Array String
  }

-- | Everything a publish needs: what KIND of content, which favourite
-- | COLLECTION it curates into, the display NAME + provenance SOURCE for the
-- | label, the canonical PAYLOAD to hash, and any label TAGS.
type PublishSpec =
  { kind :: String
  , collection :: String
  , name :: String
  , source :: String
  , payload :: String
  , tags :: Array String
  }

foreign import fetchCollectionImpl
  :: String
  -> (Error -> Effect Unit)
  -> (Array LibItem -> Effect Unit)
  -> Effect Unit

foreign import publishImpl
  :: PublishSpec
  -> (Error -> Effect Unit)
  -> (String -> Effect Unit)
  -> Effect Unit

foreign import unpublishImpl
  :: String
  -> String
  -> (Error -> Effect Unit)
  -> (Unit -> Effect Unit)
  -> Effect Unit

-- | Fetch a favourite collection as name+payload+tags items. Rejects if the
-- | store is unreachable.
fetchCollection :: String -> Aff (Array LibItem)
fetchCollection collection = makeAff \cb -> do
  fetchCollectionImpl collection (cb <<< Left) (cb <<< Right)
  pure nonCanceler

-- | Publish content → label → favourite (each guarded / deduped). Resolves the
-- | content hash. Rejects if the store is unreachable.
publish :: PublishSpec -> Aff String
publish spec = makeAff \cb -> do
  publishImpl spec (cb <<< Left) (cb <<< Right)
  pure nonCanceler

-- | Remove content from a collection ("unpublish"); content + labels stay
-- | addressable. Rejects if the store is unreachable.
unpublish :: String -> String -> Aff Unit
unpublish collection hash = makeAff \cb -> do
  unpublishImpl collection hash (cb <<< Left) (cb <<< Right)
  pure nonCanceler
