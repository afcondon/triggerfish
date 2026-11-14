-- | **The shelf: presets you keep**, in Amphora, the rig's artefact store.
-- |
-- | A kept preset is content (kind `conspicillum-preset`), so it is
-- | immutable and named by its hash; its name is a label on it; and the shelf
-- | is Amphora's favourite collection `conspicillum-shelf`. Keeping a changed
-- | version stores new content, records the old one as its origin (a morphism
-- | `edit`), and swaps it on the shelf. Taking a preset off the shelf keeps
-- | the content, so nothing kept is ever lost.
-- |
-- | Not the browser's storage: that belongs to one origin, and the rig's pages
-- | have lost presets to it before.
module Conspicillum.Shelf
  ( Kept
  , Stored
  , stored
  , fromStored
  , shelf
  , keep
  , replace
  , remove
  ) where

import Prelude

import Amphora.Api as Api
import Data.Array (catMaybes, find, null)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Hawser.Fetch (troubleText)
import Hawser.Fetch as Fetch
import Hawser.Http (Endpoint)
import Reef.Conspicillum.Notation (parse, print)
import Reef.Conspicillum.Parameter (fromIdentifier, identifier)
import Reef.Conspicillum.Preset (Bank(..), Preset, bankName)
import Reef.Conspicillum.Protocol (WireQuery, fromWireQuery, toWireQuery)
import Simple.JSON (E, readJSON, writeJSON)

-- | A preset on the shelf, and the hash that names it.
type Kept = { hash :: String, preset :: Preset }

-- | A preset as it is kept: the line printed, the query and knobs by name.
type Stored =
  { name :: String
  , bank :: String
  , about :: String
  , line :: String
  , query :: WireQuery
  , knobs :: Array { parameter :: String, label :: String }
  }

collection :: String
collection = "conspicillum-shelf"

kind :: String
kind = "conspicillum-preset"

stored :: Preset -> Stored
stored p =
  { name: p.name
  , bank: bankName p.bank
  , about: p.about
  , line: print p.line
  , query: toWireQuery p.query
  -- also in the line since CR-29; kept beside it for pages from before
  , knobs: p.line.knobs <#> \k -> { parameter: identifier k.parameter, label: k.label }
  }

fromStored :: Stored -> Maybe Preset
fromStored s = do
  line <- hush (parse s.line)
  pure
    { name: s.name
    , bank: fromMaybe Early (find (\b -> bankName b == s.bank) banks)
    , about: s.about
    -- a preset kept before CR-29 has its knobs beside the line, not in it
    , line: if null line.knobs then line { knobs = catMaybes (s.knobs <#> \k -> fromIdentifier k.parameter <#> \parameter -> { parameter, label: k.label }) } else line
    , query: fromWireQuery s.query
    }
  where
  banks = [ Demos, Early, Found, Polychords, Sector, Fix, Dub, Swung, Harmony, Envelopes, Effects, Resonators, Progressions, Sampler ]

-- | Everything on the shelf. A kept preset whose line no longer parses is
-- | left out rather than failing the rest.
shelf :: String -> Aff (Either String (Array Kept))
shelf host = ask host Api.listFavorites { collection: Just collection } >>= case _ of
  Left problem -> pure (Left problem)
  Right favourites -> Right <<< catMaybes <$> traverse (fetchOne host <<< _.contentHash) favourites

fetchOne :: String -> String -> Aff (Maybe Kept)
fetchOne host hash = ask host Api.getContent { hash } <#> \got -> do
  content <- hush got
  s <- hush (readJSON content.payload :: E Stored)
  preset <- fromStored s
  pure { hash, preset }

-- | Put a preset on the shelf.
keep :: String -> Preset -> Aff (Either String Kept)
keep host preset = ask host Api.putContent { kind, payload: writeJSON (stored preset) } >>= case _ of
  Left problem -> pure (Left problem)
  Right { hash } -> do
    _ <- ask host Api.addLabel
      { contentHash: hash, name: preset.name, source: Nothing, harmonicRoot: Nothing, harmonicScale: Nothing, harmonicChord: Nothing, tags: Nothing }
    _ <- ask host Api.addFavorite { contentHash: hash, collection }
    pure (Right { hash, preset })

-- | Keep a changed version in place of one on the shelf, remembering where it
-- | came from.
replace :: String -> String -> Preset -> Aff (Either String Kept)
replace host old preset = do
  kept <- keep host preset
  case kept of
    Right k | k.hash /= old -> do
      _ <- ask host Api.addMorphism { from: old, to: k.hash, kind: "edit", params: Nothing }
      _ <- remove host old
      pure kept
    _ -> pure kept

remove :: String -> String -> Aff (Either String Unit)
remove host hash = map (const unit) <$> ask host Api.removeFavorite { hash, collection }

-- | Ask the store on `host` one of its described operations (Amphora.Api).
-- | The problem, if there is one, as a sentence.
ask :: forall request reply. String -> Endpoint request reply -> request -> Aff (Either String reply)
ask host described request =
  lmap troubleText <$> Fetch.ask { base: "http://" <> host <> ":3024", timeoutMs: 5000 } described request
