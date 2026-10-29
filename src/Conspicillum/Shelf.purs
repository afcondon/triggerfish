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

import Conspicillum.Http as Http
import Data.Array (catMaybes, find, null)
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe, fromMaybe)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
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
  banks = [ Early, Found, Polychords, Sector, Fix, Dub, Swung, Harmony, Envelopes, Effects, Resonators, Progressions ]

-- | Everything on the shelf. A kept preset whose line no longer parses is
-- | left out rather than failing the rest.
shelf :: String -> Aff (Either String (Array Kept))
shelf host = do
  listed <- Http.get (base host <> "/favorites?collection=" <> collection)
  case listed >>= decodeWith (readJSON :: String -> E (Array { contentHash :: String })) of
    Left problem -> pure (Left problem)
    Right favourites -> do
      found <- traverse (fetchOne host <<< _.contentHash) favourites
      pure (Right (catMaybes found))

fetchOne :: String -> String -> Aff (Maybe Kept)
fetchOne host hash = do
  got <- Http.get (base host <> "/content/" <> hash)
  pure do
    content <- hush (got >>= decodeWith (readJSON :: String -> E { payload :: String }))
    s <- hush (readJSON content.payload :: E Stored)
    preset <- fromStored s
    pure { hash, preset }

-- | Put a preset on the shelf.
keep :: String -> Preset -> Aff (Either String Kept)
keep host preset = do
  saved <- Http.post (base host <> "/content") (writeJSON { kind, payload: writeJSON (stored preset) })
  case saved >>= decodeWith (readJSON :: String -> E { hash :: String }) of
    Left problem -> pure (Left problem)
    Right { hash } -> do
      _ <- Http.post (base host <> "/labels") (writeJSON { contentHash: hash, name: preset.name })
      _ <- Http.post (base host <> "/favorites") (writeJSON { contentHash: hash, collection })
      pure (Right { hash, preset })

-- | Keep a changed version in place of one on the shelf, remembering where it
-- | came from.
replace :: String -> String -> Preset -> Aff (Either String Kept)
replace host old preset = do
  kept <- keep host preset
  case kept of
    Right k | k.hash /= old -> do
      _ <- Http.post (base host <> "/morphisms") (writeJSON { from: old, to: k.hash, kind: "edit" })
      _ <- remove host old
      pure kept
    _ -> pure kept

remove :: String -> String -> Aff (Either String Unit)
remove host hash = map (const unit) <$> Http.delete (base host <> "/favorites?hash=" <> hash <> "&collection=" <> collection)

base :: String -> String
base host = "http://" <> host <> ":3024"

decodeWith :: forall a e. Show e => (String -> Either e a) -> String -> Either String a
decodeWith f text = case f text of
  Left e -> Left (show e)
  Right a -> Right a
