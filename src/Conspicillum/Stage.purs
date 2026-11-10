-- | **The stage: what the rig has loaded**, as this page writes and reads it.
-- |
-- | The rig keeps, for each instrument, what it is playing and whatever the
-- | page that pushed it needs to show it again, and tells every other page
-- | when that changes (purerl-tidal's `tidal_stage`). So a page reloaded
-- | mid-set comes back to what is playing, and two open pages stay in step.
-- |
-- | A push is the scene for the voice in an envelope: where the preset came
-- | from, whether it is edited, and `page`, the preset in hand as the shelf
-- | stores one. The rig never reads `page`; only pages do.
-- |
-- | **The line** is also a text object on the stage, `conspicillum/line`
-- | (docs/kb/plans/text-on-the-stage.md), so Limulus can show it and write it
-- | as `conspicillum $ …`. The page publishes its line as it changes, applies
-- | a line written elsewhere as if typed in its own box, and refuses one it
-- | cannot read.
-- |
-- | **Layers** (2026-11-10): Conspicillum plays up to four clouds at once,
-- | `a` to `d`, each its own voice on the rig, its own slot and its own line.
-- | Layer `a` keeps the names from before layers (`conspicillum`,
-- | `conspicillum/line`), so nothing written earlier is lost; `b` is
-- | `conspicillum-b` and `conspicillum/line-b`, and so on.
module Conspicillum.Stage
  ( Layer
  , layers
  , layerOfSlot
  , headOf
  , Held
  , Entry
  , push
  , stop
  , readEntry
  , textSubscribe
  , LineFrame(..)
  , readLine
  , writeLine
  , rejectLine
  , openLine
  ) where

import Prelude

import Conspicillum.Shelf (Stored)
import Data.Array (elem, find)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.String as String
import Foreign (Foreign)
import Foreign.Object (Object)
import Foreign.Object as Object
import Simple.JSON (E, read, readJSON, writeJSON)

-- | A layer's name: "a" to "d".
type Layer = String

layers :: Array Layer
layers = [ "a", "b", "c", "d" ]

slotOf :: Layer -> String
slotOf = case _ of
  "a" -> "conspicillum"
  l -> "conspicillum-" <> l

-- | The layer a stage slot is, if it is one of Conspicillum's.
layerOfSlot :: String -> Maybe Layer
layerOfSlot slot = find (\l -> slotOf l == slot) layers

lineKey :: Layer -> String
lineKey = case _ of
  "a" -> "conspicillum/line"
  l -> "conspicillum/line-" <> l

layerOfKey :: String -> Maybe Layer
layerOfKey key = find (\l -> lineKey l == key) layers

-- | How a layer's line starts in Limulus: `conspicillum` for `a`, as it
-- | always was, `conspicillum "b"` for the others.
headOf :: Layer -> String
headOf = case _ of
  "a" -> "conspicillum"
  l -> "conspicillum \"" <> l <> "\""

-- | The preset in hand: the recalled one, as edited, and whether the
-- | material is held. `origin` is the page's link token (`12`, `k…`).
type Held =
  { origin :: Maybe String
  , original :: Maybe Stored
  , current :: Stored
  , held :: Boolean
  }

-- | One slot as the rig announces it. `page` is missing when the scene was
-- | pushed bare, by a page from before the stage.
type Entry =
  { slot :: String
  , page :: Maybe Held
  , playing :: Boolean
  }

-- | The `conspicillum-scene` line for a scene (already encoded) and what the
-- | stage should record beside it.
push :: { layer :: Layer, base :: Maybe String, edited :: Boolean, page :: Held, scene :: String } -> String
push r =
  "conspicillum-scene {\"scene\":" <> r.scene <> ","
    <> String.drop 1 (writeJSON { layer: r.layer, base: toNullable r.base, edited: r.edited, page: r.page })

-- | Stop one layer's voice.
stop :: Layer -> String
stop layer = "conspicillum-stop " <> layer

-- | A `stage …` line from the rig, if that is what this is. A `page` this
-- | page cannot read loses only the page, not whether the slot is playing.
readEntry :: String -> Maybe Entry
readEntry line = do
  json <- String.stripPrefix (String.Pattern "stage ") line
  entry <- hush (readJSON json :: E { slot :: String, playing :: Boolean, page :: Foreign })
  pure { slot: entry.slot, playing: entry.playing, page: hush (read entry.page) }

textSubscribe :: String
textSubscribe = "stage-text-subscribe"

-- | What a rig frame says about the lines: what the stage holds for each
-- | layer (on subscribing), or a write (`Nothing`: deleted) of one layer's
-- | line by another page.
data LineFrame
  = LineHeld (Array (Tuple Layer String))
  | LineWritten Layer (Maybe String)

readLine :: String -> Maybe LineFrame
readLine msg = case String.stripPrefix (String.Pattern "stage-texts ") msg of
  Just json -> do
    table :: Object { text :: String } <- hush (readJSON json)
    pure (LineHeld (layers >>= \l -> case Object.lookup (lineKey l) table of
      Just t -> [ Tuple l t.text ]
      Nothing -> []))
  Nothing -> do
    json <- String.stripPrefix (String.Pattern "stage-text ") msg
    w :: { key :: String, text :: Nullable String } <- hush (readJSON json)
    l <- layerOfKey w.key
    if elem l layers then Just (LineWritten l (toMaybe w.text)) else Nothing

writeLine :: Layer -> String -> String
writeLine layer text = "stage-text " <> lineKey layer <> " " <> text

rejectLine :: Layer -> String -> String
rejectLine layer reason = "stage-reject " <> lineKey layer <> " " <> reason

-- | Ask Limulus to show a layer's line.
openLine :: Layer -> String
openLine layer = "stage-open " <> lineKey layer
