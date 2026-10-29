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
module Conspicillum.Stage
  ( Held
  , Entry
  , push
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
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.String as String
import Foreign (Foreign)
import Foreign.Object (Object)
import Foreign.Object as Object
import Simple.JSON (E, read, readJSON, writeJSON)

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
push :: { base :: Maybe String, edited :: Boolean, page :: Held, scene :: String } -> String
push r =
  "conspicillum-scene {\"scene\":" <> r.scene <> ","
    <> String.drop 1 (writeJSON { base: toNullable r.base, edited: r.edited, page: r.page })

-- | A `stage …` line from the rig, if that is what this is. A `page` this
-- | page cannot read loses only the page, not whether the slot is playing.
readEntry :: String -> Maybe Entry
readEntry line = do
  json <- String.stripPrefix (String.Pattern "stage ") line
  entry <- hush (readJSON json :: E { slot :: String, playing :: Boolean, page :: Foreign })
  pure { slot: entry.slot, playing: entry.playing, page: hush (read entry.page) }

lineKey :: String
lineKey = "conspicillum/line"

textSubscribe :: String
textSubscribe = "stage-text-subscribe"

-- | What a rig frame says about the line: what the stage holds (on
-- | subscribing), or a write (`Nothing`: deleted) by another page.
data LineFrame
  = LineHeld (Maybe String)
  | LineWritten (Maybe String)

readLine :: String -> Maybe LineFrame
readLine msg = case String.stripPrefix (String.Pattern "stage-texts ") msg of
  Just json -> do
    table :: Object { text :: String } <- hush (readJSON json)
    pure (LineHeld (_.text <$> Object.lookup lineKey table))
  Nothing -> do
    json <- String.stripPrefix (String.Pattern "stage-text ") msg
    w :: { key :: String, text :: Nullable String } <- hush (readJSON json)
    if w.key == lineKey then Just (LineWritten (toMaybe w.text)) else Nothing

writeLine :: String -> String
writeLine text = "stage-text " <> lineKey <> " " <> text

rejectLine :: String -> String
rejectLine reason = "stage-reject " <> lineKey <> " " <> reason

-- | Ask Limulus to show the line.
openLine :: String
openLine = "stage-open " <> lineKey
