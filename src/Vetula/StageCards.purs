-- | **Vetula's cards on the stage** (docs/kb/plans/text-on-the-stage.md).
-- |
-- | Each card is a text object on the rig's stage, `vetula/v<id>`, holding its
-- | line (`Vetula.Lepidoptera.printCard`). The page publishes a card when its
-- | line changes, applies a write from elsewhere (Limulus evaluating `v3 $ …`),
-- | and rejects one it cannot read. The rig never echoes a page's own write, so
-- | what this page last saw on the stage is what it last sent or received.
-- |
-- | The wire is plain verbs on the rig socket: `stage-text-subscribe` (answered
-- | with `stage-texts <json>`, the whole table), then `stage-text <json>` per
-- | write by another page; `stage-text <key> <text>` and `stage-text-del <key>`
-- | to write; `stage-open <key>` to ask Limulus to show a card;
-- | `stage-reject <key> <reason>` to refuse a write.
module Vetula.StageCards
  ( cardKey
  , cardIdOfKey
  , subscribeLine
  , openLine
  , rejectLine
  , StageFrame(..)
  , readFrame
  , tableHasKey
  , publishLines
  , readNotes
  , progressionTable
  , progressionLines
  , stageName
  ) where

import Prelude

import Data.Array (all, catMaybes, mapMaybe)
import Data.Either (hush)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.String (Pattern(..), stripPrefix)
import Data.String.CodeUnits (toCharArray)
import Reef.Vetula.Lepidoptera (progressionKey, progressionOfKey)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Simple.JSON (readJSON)

cardKey :: Int -> String
cardKey n = "vetula/v" <> show n

cardIdOfKey :: String -> Maybe Int
cardIdOfKey key = stripPrefix (Pattern "vetula/v") key >>= Int.fromString

subscribeLine :: String
subscribeLine = "stage-text-subscribe"

openLine :: Int -> String
openLine n = "stage-open " <> cardKey n

rejectLine :: Int -> String -> String
rejectLine n reason = "stage-reject " <> cardKey n <> " " <> reason

-- | What a rig frame says about the cards: the whole table (on subscribing),
-- | or one card written (`Just` its text) or deleted (`Nothing`) elsewhere.
data StageFrame
  = Table (Map Int String)
  | Written Int (Maybe String)

readFrame :: String -> Maybe StageFrame
readFrame msg = case stripPrefix (Pattern "stage-texts ") msg of
  Just json -> do
    table :: Object { text :: String } <- hush (readJSON json)
    pure (Table (Map.fromFoldable (mapMaybe entry (Object.toUnfoldable table))))
  Nothing -> do
    json <- stripPrefix (Pattern "stage-text ") msg
    w :: { key :: String, text :: Nullable String } <- hush (readJSON json)
    n <- cardIdOfKey w.key
    pure (Written n (toMaybe w.text))
  where
  entry (Tuple key v) = (\n -> Tuple n v.text) <$> cardIdOfKey key

-- | Whether the whole table (the answer to a subscribe) holds Vetula's key
-- | (`vetula/key`); `Nothing` for any other frame.
tableHasKey :: String -> Maybe Boolean
tableHasKey msg = do
  json <- stripPrefix (Pattern "stage-texts ") msg
  table :: Object { text :: String } <- hush (readJSON json)
  pure (Object.member "vetula/key" table)

-- | The notes the rig played for the cards, `vetula-notes [{pitch, ch, atUs,
-- | vel, gateMs}]`, at Unix microseconds.
readNotes :: String -> Maybe (Array { pitch :: Int, ch :: Int, atUs :: Number, vel :: Int, gateMs :: Number })
readNotes msg = stripPrefix (Pattern "vetula-notes ") msg >>= (hush <<< readJSON)

-- | The lines that bring the stage from `seen` (what it holds, as far as this
-- | page knows) to `now` (the cards as they are): a write per changed or new
-- | card, a delete per card gone.
publishLines :: Map Int String -> Map Int String -> Array String
publishLines seen now =
  catMaybes (map write (Map.toUnfoldable now :: Array (Tuple Int String)))
    <> map (\n -> "stage-text-del " <> cardKey n)
         (catMaybes (map gone (Map.toUnfoldable seen :: Array (Tuple Int String))))
  where
  write (Tuple n text) =
    if Map.lookup n seen == Just text then Nothing
    else Just ("stage-text " <> cardKey n <> " " <> text)
  gone (Tuple n _) = if Map.member n now then Nothing else Just n

-- | The saved progressions the stage holds (`vetula/progression/<name>`, step
-- | 4b), by name, from the whole table; `Nothing` for any other frame.
progressionTable :: String -> Maybe (Map String String)
progressionTable msg = do
  json <- stripPrefix (Pattern "stage-texts ") msg
  table :: Object { text :: String } <- hush (readJSON json)
  pure (Map.fromFoldable (mapMaybe entry (Object.toUnfoldable table)))
  where
  entry (Tuple key v) = (\name -> Tuple name v.text) <$> progressionOfKey key

-- | The writes that put each progression in `now` on the stage, where it
-- | differs from what the stage holds (`seen`). None are deleted: a name a
-- | card plays stays playable after this page forgets it.
progressionLines :: Map String String -> Map String String -> Array String
progressionLines seen now = catMaybes (map write (Map.toUnfoldable now :: Array (Tuple String String)))
  where
  write (Tuple name text) =
    if Map.lookup name seen == Just text then Nothing
    else Just ("stage-text " <> progressionKey name <> " " <> text)

-- | Whether a name can be a stage key (letters, digits, `_-.`), and so be
-- | named by a card.
stageName :: String -> Boolean
stageName name = name /= "" && all ok (toCharArray name)
  where
  ok c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_' || c == '-' || c == '.'
