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
-- |     (`Reef.Word`): its name's, unique in its machine, `Nothing` until
-- |     named. It follows a rename, so what a drag carries is what the row
-- |     says (AC, 2026-10-13).
-- |   • `id` — which it is, for the Library in Amphora (`Triggerfish.Library`),
-- |     whatever it is renamed or changed to; "" until it is first kept.
-- |   • `was` — the words it had before, still on the stage, so a line
-- |     naming it by an old word never goes silent.
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
  , rename
  , wordsOf
  , withWords
  , presetDoc
  , Stored
  , toStored
  , fromStored
  ) where

import Prelude

import Data.Array (concatMap, deleteAt, filter, findIndex, foldl, modifyAt, snoc)
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Set as Set
import Effect.Aff (Aff)
import Simple.JSON (readJSON, writeJSON)
import Reef.Word (freeWord)
import Triggerfish.Glyph (Glyph, glyphOf)
import Triggerfish.Library (Doc)

type Preset =
  { content :: String
  , name :: Maybe String
  , starred :: Boolean
  , word :: Maybe String
  , was :: Array String
  , id :: String
  }

-- | A preset just kept: not starred, and no word until it is named and saved.
newPreset :: String -> Maybe String -> Preset
newPreset content name = { content, name, starred: false, word: Nothing, was: [], id: "" }

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

-- | Every word a preset plays by: its own, then its old ones.
wordsOf :: Preset -> Array String
wordsOf p = maybe p.was (\w -> [ w ] <> p.was) p.word

-- | Name the preset at `i`. A named preset's word becomes its new name's,
-- | numbered if another preset plays by it; the word it had stays playable
-- | in `was`. A blank name, or one with nothing to make a word of, leaves
-- | the words alone.
rename :: Int -> String -> Array Preset -> Array Preset
rename i name presets = fromMaybe presets (modifyAt i named presets)
  where
  taken = Set.fromFoldable (concatMap wordsOf (fromMaybe presets (deleteAt i presets)))
  named p
    | name == "" = p { name = Nothing }
    | otherwise = case freeWord (\c -> Set.member c taken) name of
        "" -> p { name = Just name }
        w | p.word == Just w -> p { name = Just name }
          | otherwise -> p { name = Just name, word = Just w, was = filter (_ /= w) (wordsOf p) }

-- | Every named preset with a word: one that has none (just named, or saved
-- | before words) gets its name's, numbered if another preset holds it.
-- | Words already given are never changed.
withWords :: Array Preset -> Array Preset
withWords presets = _.out (foldl step { taken: Set.fromFoldable (concatMap wordsOf presets), out: [] } presets)
  where
  step acc p = case p.word, p.name of
    Nothing, Just name | w <- freeWord (\c -> Set.member c acc.taken) name, w /= "" ->
      { taken: Set.insert w acc.taken, out: snoc acc.out (p { word = Just w }) }
    _, _ -> acc { out = snoc acc.out p }

-- | A preset as every machine's store keeps it: an anonymous one's name is
-- | "", and a preset saved before words has none (`word` null or absent).
-- | `was` and `id` are absent from one saved before they existed.
type Stored = { content :: String, name :: String, starred :: Boolean, word :: Nullable String, was :: Nullable (Array String), id :: Nullable String }

toStored :: Preset -> Stored
toStored p = { content: p.content, name: fromMaybe "" p.name, starred: p.starred, word: toNullable p.word, was: toNullable (Just p.was), id: toNullable (Just p.id) }

fromStored :: Stored -> Preset
fromStored e = { content: e.content, name: if e.name == "" then Nothing else Just e.name, starred: e.starred, word: toMaybe e.word, was: fromMaybe [] (toMaybe e.was), id: fromMaybe "" (toMaybe e.id) }

-- | A machine's presets as its Library drawer keeps them in Amphora, at
-- | `triggerfish/<machine>/library`; `first` is what the machine kept there
-- | before (read once). One thing kept twice before ids is one by name, or,
-- | unnamed, by content.
presetDoc :: String -> Aff (Array Preset) -> Doc Preset
presetDoc machine first =
  { ref: "triggerfish/" <> machine <> "/library"
  , kind: "triggerfish-" <> machine <> "-library"
  , idOf: _.id
  , withId: \id p -> p { id = id }
  , encode: \ps -> writeJSON (map toStored ps)
  , decode: \text -> map fromStored <$> hush (readJSON text)
  , same: \a b -> if isJust a.name then a.name == b.name else a.content == b.content
  , first
  }
