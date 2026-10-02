-- | **A Vetula card is one line, and the stage gets only what changed.**
-- | `printCard`/`parseCard` round-trip a card's line exactly (the form it has
-- | on the stage and in Limulus as `v3 $ …`), and `publishLines` turns two
-- | views of the cards into the writes and deletes that bring the stage up to
-- | date (docs/kb/plans/text-on-the-stage.md).
module Test.CardLineSpec (runCardLineTests) where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assertEqual')
import Vetula.Lepidoptera (parseCard, printCard)
import Vetula.StageCards (StageFrame(..), publishLines, readFrame)

runCardLineTests :: Effect Unit
runCardLineTests = do
  log "Vetula cards as lines, and what the stage is sent"
  let
    lines =
      [ "ch3 \"<[c4,e4,g4] [a3,c4,e4]>\" \"0 1 2 3\" # arpup 4 # every 2 # transpose 2 # out odo"
      , "ch1 \"<[d4,f4,a4,c5]>\" \"<0 0*2>\" # voice open # strum 30 # mute"
      , "ch4 - \"\""
      ]
    again l = printCard <$> parseCard l
  assertEqual' "cards round-trip" { actual: map again lines, expected: map Just lines }
  -- Tidal's note names: c5 is middle C (60), so c4 is 48
  assertEqual' "chords read back"
    { actual: _.chords <$> parseCard "ch2 \"<[c4,e4,g4] [a3,c4,e4]>\" \"0 1\""
    , expected: Just [ [ 48, 52, 55 ], [ 45, 48, 52 ] ] }
  let
    seen = Map.fromFoldable [ Tuple 1 "a", Tuple 2 "b", Tuple 3 "c" ]
    now = Map.fromFoldable [ Tuple 1 "a", Tuple 2 "B", Tuple 4 "d" ]
  assertEqual' "only changes are published"
    { actual: publishLines seen now
    , expected: [ "stage-text vetula/v2 B", "stage-text vetula/v4 d", "stage-text-del vetula/v3" ] }
  assertEqual' "nothing to send when in step" { actual: publishLines now now, expected: [] }
  assertEqual' "a write elsewhere is read"
    { actual: describe (readFrame "stage-text {\"key\":\"vetula/v7\",\"text\":\"ch7 - \\\"0\\\"\",\"ver\":3}")
    , expected: "written 7 ch7 - \"0\"" }
  assertEqual' "another slot's object is not a card"
    { actual: describe (readFrame "stage-text {\"key\":\"odonus/patch\",\"text\":\"x\",\"ver\":1}"), expected: "-" }
  log "   ok"
  where
  describe = case _ of
    Just (Written n (Just t)) -> "written " <> show n <> " " <> t
    Just (Written n Nothing) -> "deleted " <> show n
    Just (Table m) -> "table " <> show (Map.size m)
    Nothing -> "-"
