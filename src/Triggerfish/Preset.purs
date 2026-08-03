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
-- |
-- | Capture DEDUPS by content (identical state → identical glyph), so hammering the
-- | hotkey on an unchanged state is idempotent — the main defence against clutter.
module Triggerfish.Preset
  ( Preset
  , presetGlyph
  , presetLabel
  , presetAlias
  , indexOfContent
  ) where

import Prelude

import Data.Array (findIndex)
import Data.Maybe (Maybe, fromMaybe)
import Triggerfish.Glyph (Glyph, glyphOf)

type Preset =
  { content :: String
  , name :: Maybe String
  , starred :: Boolean
  }

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
