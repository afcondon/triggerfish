-- | Triggerfish.Balistes.Lepidoptera — the eDSL print/parse for Balistes fixed
-- | rhythms (Lepidoptera A2). Per AC's rule, a fixed rhythm is rendered as a
-- | **fully-structured record**, not mini-notation: each lane lists its hits as
-- | `hit <step> <vel>` (default overlay) or `cell <step> <vel> { … }` (tweaked).
-- | Mini-notation is reserved for where it compresses and reveals structure;
-- | a dense per-cell grid is neither, and a structured list reads far better
-- | than a rhythm string aligned against a parallel gain string.
-- |
-- | The output is valid `Tidal.Balistes` eDSL (`balistesPattern` / `lane` /
-- | `hit` / `cell` are functions), so it's consumable by Calypso and shippable
-- | to purerl-tidal — the canonical, transferable form of a rhythm.
module Triggerfish.Balistes.Lepidoptera
  ( printPattern
  , parsePattern
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (mapMaybe, range)
import Data.Either (hush)
import Data.Foldable (foldl)
import Data.Int as Int
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split) as Str
import Data.String.CodeUnits (fromCharArray)
import Data.String.Common (joinWith)
import Parsing (Parser, runParser) as Par
import Parsing.Combinators (sepBy, try) as PC
import Parsing.Combinators.Array (many) as PCA
import Parsing.String (char, eof, satisfy, string)
import Parsing.String.Basic (intDecimal, skipSpaces)
import Triggerfish.Balistes.Pattern as P

-- ---------------------------------------------------------------------------
-- Print
-- ---------------------------------------------------------------------------

-- | Render a fixed rhythm as `balistesPattern "<name>" <steps> [ <lanes> ]`.
printPattern :: P.FixedPattern -> String
printPattern p =
  "balistesPattern " <> show p.name <> " " <> show p.steps <> "\n"
    <> "  [ " <> joinWith "\n  , " (map (printLane p) (P.usedLanes p)) <> "\n  ]"

-- | One lane: `lane "<name>" <note> [ <cells> ]`, listing only its hits.
printLane :: P.FixedPattern -> Int -> String
printLane p lane =
  "lane " <> show (P.laneName lane) <> " " <> show (P.noteOf p lane)
    <> " [ " <> joinWith ", " (mapMaybe atStep (range 0 (p.steps - 1))) <> " ]"
  where
  atStep step =
    let c = P.cellAt p lane step
    in if c.vel <= 0 then Nothing else Just (printCell step c)

-- | A plain hit, or — when the overlay is tweaked — the fuller `cell` form.
printCell :: Int -> P.Cell -> String
printCell step c =
  if P.cellTweaked c then
    "cell " <> show step <> " " <> show c.vel
      <> " { prob: " <> show c.prob
      <> ", cond: " <> show (P.condLabel c.cond)
      <> ", ratchet: " <> show c.ratchet <> " }"
  else
    "hit " <> show step <> " " <> show c.vel

-- ---------------------------------------------------------------------------
-- Parse
-- ---------------------------------------------------------------------------

type Parser a = Par.Parser String a

-- A parsed lane before it's folded into the grid.
type PLane = { nm :: String, note :: Int, cells :: List { step :: Int, cell :: P.Cell } }

-- | Parse the eDSL back to a fixed rhythm (Nothing on malformed input — the
-- | Store then drops it and falls back to the bundled patterns).
parsePattern :: String -> Maybe P.FixedPattern
parsePattern input = hush (runParser' input (patternP <* eof))
  where
  runParser' s p = Par.runParser s p

patternP :: Parser P.FixedPattern
patternP = do
  ws
  _ <- sym "balistesPattern"
  name <- strL
  steps <- intL
  lanes <- bracketed (PC.sepBy laneP (sym ","))
  pure (build name steps lanes)

laneP :: Parser PLane
laneP = do
  _ <- sym "lane"
  nm <- strL
  note <- intL
  cells <- bracketed (PC.sepBy cellP (sym ","))
  pure { nm, note, cells }

cellP :: Parser { step :: Int, cell :: P.Cell }
cellP = PC.try hitP <|> tweakP

hitP :: Parser { step :: Int, cell :: P.Cell }
hitP = do
  _ <- sym "hit"
  s <- intL
  v <- intL
  pure { step: s, cell: P.hitCell v }

tweakP :: Parser { step :: Int, cell :: P.Cell }
tweakP = do
  _ <- sym "cell"
  s <- intL
  v <- intL
  _ <- sym "{"
  _ <- sym "prob:"
  prob <- intL
  _ <- sym ","
  _ <- sym "cond:"
  cs <- strL
  _ <- sym ","
  _ <- sym "ratchet:"
  rt <- intL
  _ <- sym "}"
  pure { step: s, cell: { vel: v, prob, cond: parseCond cs, ratchet: rt } }

-- "X:Y" → CEvery X Y; anything else (incl. "—") → CAlways.
parseCond :: String -> P.TrigCond
parseCond s = case Str.split (Str.Pattern ":") s of
  [ a, b ] -> case Int.fromString a, Int.fromString b of
    Just x, Just y -> P.CEvery x y
    _, _ -> P.CAlways
  _ -> P.CAlways

build :: String -> Int -> List PLane -> P.FixedPattern
build name steps lanes = foldl applyLane (P.emptyPattern name steps) lanes
  where
  applyLane p l = case P.laneIndexOf l.nm of
    Nothing -> p
    Just idx -> foldl (placeCell idx) (P.setNoteAt idx l.note p) l.cells
  placeCell idx p c = P.modifyCell idx c.step (const c.cell) p

-- --- lexing helpers ---------------------------------------------------------

ws :: Parser Unit
ws = skipSpaces

sym :: String -> Parser Unit
sym s = void (string s) <* ws

intL :: Parser Int
intL = intDecimal <* ws

strL :: Parser String
strL = stringLit <* ws

stringLit :: Parser String
stringLit = char '"' *> (fromCharArray <$> PCA.many (satisfy (_ /= '"'))) <* char '"'

bracketed :: forall a. Parser a -> Parser a
bracketed p = sym "[" *> p <* sym "]"
