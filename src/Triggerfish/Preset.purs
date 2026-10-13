-- | `Triggerfish.Preset` — the unified preset model shared by every machine's
-- | chip bank. The resolution to fast-vs-named (see `docs/DESIGN-scene-modal.md`):
-- | ONE store per machine where a preset is anonymous (glyph-identified) OR named,
-- | freely intermixed, and promotion is just `name: Nothing → Just` in place.
-- |
-- |   • `content` — the recallable state as eDSL / snapshot TEXT (the Lepidoptera
-- |     "save the rendering" rule). Its GLYPH is always `glyphOf content` — derived,
-- |     never stored, so it can't drift; a named preset still has a glyph.
-- |   • `name` — `Nothing` = anonymous (fast capture), `Just` = promoted/authored.
-- |   • `starred` — a go-to; the recall menu surfaces these first.
-- |   • `word` — what a line plays it by and its key on the stage
-- |     (`Reef.Word`): given once, when it is first named, unique in its
-- |     machine, and kept through renames, so a line naming it never goes
-- |     silent. `Nothing` until named.
-- |
-- | Capture DEDUPS by content (identical state → identical glyph), so hammering the
-- | hotkey on an unchanged state is idempotent — the main defence against clutter.
module Triggerfish.Preset
  ( Preset
  , presetGlyph
  , presetLabel
  , presetAlias
  , indexOfContent
  , newPreset
  , withWords
  , Stored
  , toStored
  , fromStored
  ) where

import Prelude

import Data.Array (findIndex, foldl, mapMaybe, snoc)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Set as Set
import Reef.Word (freeWord)
import Triggerfish.Glyph (Glyph, glyphOf)

type Preset =
  { content :: String
  , name :: Maybe String
  , starred :: Boolean
  , word :: Maybe String
  }

-- | A preset just kept: not starred, and no word until it is named and saved.
newPreset :: String -> Maybe String -> Preset
newPreset content name = { content, name, starred: false, word: Nothing }

-- | The preset's (derived) glyph.
presetGlyph :: Preset -> Glyph
presetGlyph p = glyphOf p.content

-- | The human label: the name if promoted, else the glyph alias.
presetLabel :: Preset -> String
presetLabel p = fromMaybe (glyphOf p.content).alias p.name

-- | The typeable alias (always the shape-pair, independent of any name).
presetAlias :: Preset -> String
presetAlias p = (glyphOf p.content).alias

-- | Dedup lookup: the index of an existing preset with this exact content (same
-- | content ⇒ same glyph), so capture can re-park rather than append a duplicate.
indexOfContent :: String -> Array Preset -> Maybe Int
indexOfContent content = findIndex \p -> p.content == content

-- | Every named preset with a word: one that has none (just named, or saved
-- | before words) gets its name's, numbered if another preset holds it.
-- | Words already given are never changed.
withWords :: Array Preset -> Array Preset
withWords presets = _.out (foldl step { taken: Set.fromFoldable (mapMaybe _.word presets), out: [] } presets)
  where
  step acc p = case p.word, p.name of
    Nothing, Just name | w <- freeWord (\c -> Set.member c acc.taken) name, w /= "" ->
      { taken: Set.insert w acc.taken, out: snoc acc.out (p { word = Just w }) }
    _, _ -> acc { out = snoc acc.out p }

-- | A preset as every machine's store keeps it: an anonymous one's name is
-- | "", and a preset saved before words has none (`word` null or absent).
type Stored = { content :: String, name :: String, starred :: Boolean, word :: Nullable String }

toStored :: Preset -> Stored
toStored p = { content: p.content, name: fromMaybe "" p.name, starred: p.starred, word: toNullable p.word }

fromStored :: Stored -> Preset
fromStored e = { content: e.content, name: if e.name == "" then Nothing else Just e.name, starred: e.starred, word: toMaybe e.word }
