-- | Triggerfish.Amphora — the shared Amphora artefact-store client.
-- |
-- | One content-addressed read/write dance reused by every editor whose library
-- | is a named collection of eDSL / Tidal text: Odonus scenes, Selene racks,
-- | Vetula progressions, Balistes patterns.
-- |
-- | The transferable unit of content is a payload STRING (the canonical
-- | Lepidoptera / Tidal rendering the editor already round-trips). The display
-- | name and any extra metadata ride on the LABEL — so recall works whether or
-- | not the eDSL embeds a name.
-- |
-- | Every request is an operation of `Amphora.Api`, the store's own
-- | description of what it answers (amphora/api, declared in
-- | amphora/conformance/AMPHORA-API.md), asked through `ask`. Nothing here
-- | writes a path, a query or a body: remove an operation there, or rename
-- | a field, and this module stops compiling.
-- |
-- | `fetchCollection` and `publish` reject if the store is unreachable —
-- | callers treat that as "fall back to the local library" (localStorage /
-- | bundled), never fatal.
module Triggerfish.Amphora
  ( LibItem
  , PublishSpec
  , fetchCollection
  , publish
  , ask
  , payloads
  ) where

import Prelude

import Amphora.Api as Api
import Control.Parallel (parTraverse)
import Data.Array (any, catMaybes, head)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Hawser.Fetch (Trouble(..), troubleText)
import Hawser.Fetch as Fetch
import Hawser.Http (Endpoint(..), Method(..))

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

-- | Where the store is: the page's own host on :3024 (so it works at
-- | localhost or andrews-mac-mini), or `window.AMPHORA_BASE`. Null on the web
-- | build (trigger-fish.app, `window.TRIGGERFISH_WEB`), which has no Amphora.
foreign import storeBase :: Effect (Nullable String)

-- | Ask the store one operation. A down or unreachable Amphora otherwise
-- | leaves fetch pending until the browser's ~30s timeout, which stalls
-- | whoever awaits it; a read gives up after 2.5 s and a write after 10.
-- | On the web build nothing touches the network.
ask :: forall request reply. Endpoint request reply -> request -> Aff (Either Trouble reply)
ask described@(Endpoint { method }) request = liftEffect storeBase >>= \found -> case toMaybe found of
  Nothing -> pure (Left (Unreachable "no Amphora on the web"))
  Just base -> Fetch.ask { base, timeoutMs: if method == Get then 2500 else 10000 } described request

-- | The answer, or a rejection that says what went wrong.
need :: forall a. Either Trouble a -> Aff a
need = case _ of
  Right value -> pure value
  Left trouble -> throwError (error ("Amphora: " <> troubleText trouble))

-- | Fetch a favourite collection as name+payload+tags items: each favourite's
-- | content, with the name and tags of its first label. Rejects if the store
-- | is unreachable.
fetchCollection :: String -> Aff (Array LibItem)
fetchCollection collection = do
  favourites <- need =<< ask Api.listFavorites { collection: Just collection }
  favourites # parTraverse \{ contentHash } -> do
    content <- need =<< ask Api.getContent { hash: contentHash }
    labels <- need =<< ask Api.listLabels { hash: Just contentHash }
    let label = head labels
    pure
      { hash: contentHash
      , name: maybe "" _.name label
      , payload: content.payload
      , tags: maybe [] _.tags label
      }

-- | The payloads of a favourite collection, where the name is in the payload
-- | and the labels are not wanted. One that cannot be fetched is left out.
-- | Rejects if the store is unreachable.
payloads :: String -> Aff (Array String)
payloads collection = do
  favourites <- need =<< ask Api.listFavorites { collection: Just collection }
  catMaybes <$> parTraverse (\{ contentHash } -> one <$> ask Api.getContent { hash: contentHash }) favourites
  where
  one = case _ of
    Right content -> Just content.payload
    Left _ -> Nothing

-- | Publish content → label → favourite (each guarded / deduped). Resolves the
-- | content hash. Rejects if the store is unreachable.
publish :: PublishSpec -> Aff String
publish spec = do
  { hash } <- need =<< ask Api.putContent { kind: spec.kind, payload: spec.payload }
  labels <- need =<< ask Api.listLabels { hash: Just hash }
  unless (any (\label -> label.name == spec.name) labels) $ void $ need =<< ask Api.addLabel
    { contentHash: hash
    , name: spec.name
    , source: Just spec.source
    , harmonicRoot: Nothing
    , harmonicScale: Nothing
    , harmonicChord: Nothing
    , tags: Just spec.tags
    }
  favourites <- need =<< ask Api.listFavorites { collection: Just spec.collection }
  unless (any (\favourite -> favourite.contentHash == hash) favourites) $ void $ need =<< ask Api.addFavorite
    { contentHash: hash, collection: spec.collection }
  pure hash
