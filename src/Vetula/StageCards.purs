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
  , publishLines
  ) where

import Prelude

import Data.Array (catMaybes, mapMaybe)
import Data.Either (hush)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.String (Pattern(..), stripPrefix)
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
