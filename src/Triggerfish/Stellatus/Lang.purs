-- | Triggerfish.Stellatus.Lang — S1a of the Stellatus language: turn the PLAYER
-- | text into a ring of arcs, live. Two placement forms, both real:
-- |
-- |   • KIT mode    — `place "bd sn hh*2 cp sn"` runs through the actual Tidal
-- |     mini-notation parser (`parseMiniPattern` + `firstCycle`), so arc onset =
-- |     event time and arc length = event span. This *proves* the placement line
-- |     is valid upstream Tidal (Lepidoptera constraint 1), not a look-alike.
-- |   • BUFFER mode — `slice N` cuts one buffer into N equal arcs (Sector's
-- |     classic mode); the slots are indices 0..N-1. Richer slicings
-- |     (explicit cut points, onset detection) come later.
-- |
-- | The KIT panel (`name = "src"` lines) supplies the ordered names, used only
-- | for colouring here. Verbs (`# speed …`) and the `jump` matrix are parsed in
-- | a later slice; this module is placement + mode.
module Triggerfish.Stellatus.Lang
  ( Arc
  , KitEntry
  , Mode(..)
  , Scene
  , parseScene
  ) where

import Prelude

import Data.Array (find, head, mapMaybe, null, range, sortBy)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Rational (toNumber) as Rat
import Data.String (Pattern(..))
import Data.String.CodeUnits (drop, indexOf, stripPrefix, take) as SCU
import Data.String.Common (split, trim)
import Tidal.Pattern.Core (firstCycle)
import Tidal.Pattern.Mini (parseMiniPattern)
import Tidal.Pattern.Types (Event(..))

type Arc = { onset :: Number, span :: Number, name :: String }

-- | One KIT line: a short name and its sample source (`bd`, `"808bd:3"`).
type KitEntry = { name :: String, src :: String }

data Mode = KitMode | BufferMode

derive instance eqMode :: Eq Mode

type Scene = { arcs :: Array Arc, mode :: Mode, kit :: Array KitEntry }

-- | Parse the KIT text and PLAYER text into a ring. Left = a human-readable
-- | reason the PLAYER placement line didn't parse (the caller keeps the last
-- | good ring and shows the message).
parseScene :: String -> String -> Either String Scene
parseScene kitText playerText = do
  place <- parsePlacement playerText
  pure { arcs: place.arcs, mode: place.mode, kit: parseKit kitText }

-- KIT lines: `name = "src"` → name + source (comments and blanks skipped).
parseKit :: String -> Array KitEntry
parseKit t = mapMaybe kitEntry (split (Pattern "\n") t)
  where
  kitEntry line =
    let l = trim line
    in if l == "" || isComment l then Nothing
       else case SCU.indexOf (Pattern "=") l of
         Just i -> do
           nm <- firstWord (trim (SCU.take i l))
           pure { name: nm, src: fromMaybe "" (quotedIn (SCU.drop (i + 1) l)) }
         Nothing -> Nothing

firstWord :: String -> Maybe String
firstWord s = case head (split (Pattern " ") (trim s)) of
  Just w | w /= "" -> Just w
  _ -> Nothing

isComment :: String -> Boolean
isComment l = case SCU.stripPrefix (Pattern "--") l of
  Just _ -> true
  Nothing -> false

startsWith :: String -> String -> Boolean
startsWith p s = case SCU.stripPrefix (Pattern p) s of
  Just _ -> true
  Nothing -> false

parsePlacement :: String -> Either String { arcs :: Array Arc, mode :: Mode }
parsePlacement txt =
  let ls = map trim (split (Pattern "\n") txt)
  in case find (startsWith "slice") ls of
       Just sl -> bufferArcs sl
       Nothing -> case find (startsWith "place") ls of
         Just pl -> kitArcs pl
         Nothing -> Left "expected a  place \"…\"  or  slice N  line"

-- `slice N` → N equal arcs of one buffer, slots named by index.
bufferArcs :: String -> Either String { arcs :: Array Arc, mode :: Mode }
bufferArcs line =
  let rest = trim (SCU.drop 5 line)   -- drop "slice"
  in case Int.fromString rest of
       Just n | n >= 1 -> Right { arcs: evenArcs n, mode: BufferMode }
       _ -> Left "slice expects a count, e.g.  slice 16"

evenArcs :: Int -> Array Arc
evenArcs n =
  map (\i -> { onset: Int.toNumber i / Int.toNumber n, span: 1.0 / Int.toNumber n, name: show i })
    (range 0 (n - 1))

-- `place "…"` → the real mini-notation parser, queried over the first cycle.
kitArcs :: String -> Either String { arcs :: Array Arc, mode :: Mode }
kitArcs line = do
  q <- maybe (Left "place needs a quoted pattern, e.g.  place \"bd sn\"") Right (quotedIn line)
  pat <- parseMiniPattern q
  let arcs = sortBy (\x y -> compare x.onset y.onset) (mapMaybe evToArc (firstCycle pat))
  if null arcs then Left "empty placement" else Right { arcs, mode: KitMode }

evToArc :: Event String -> Maybe Arc
evToArc = case _ of
  Digital e ->
    let a = unwrap e.whole
        on = Rat.toNumber a.start
    in if on >= 0.0 && on < 1.0
         then Just { onset: on, span: Rat.toNumber a.stop - on, name: e.value }
         else Nothing
  Analog _ -> Nothing

-- The content between the first pair of double quotes.
quotedIn :: String -> Maybe String
quotedIn s = case SCU.indexOf (Pattern "\"") s of
  Just i ->
    let rest = SCU.drop (i + 1) s
    in case SCU.indexOf (Pattern "\"") rest of
         Just j -> Just (SCU.take j rest)
         Nothing -> Nothing
  Nothing -> Nothing
