-- | Triggerfish.Stellatus.Lang — the Stellatus language: turn the KIT + PLAYER
-- | text into a live, sounding scene.
-- |
-- | Placement (S1a) — two forms, both real:
-- |   • KIT mode    — `place "bd sn hh*2 cp sn"` runs through the actual Tidal
-- |     mini-notation parser (`parseMiniPattern` + `firstCycle`), so arc onset =
-- |     event time and arc length = event span. This *proves* the placement line
-- |     is valid upstream Tidal (Lepidoptera constraint 1), not a look-alike.
-- |   • BUFFER mode — `slice N` cuts one buffer into N equal arcs.
-- |
-- | Playback (S1b) — the TEXT now steers the sound:
-- |   • VERBS   — `# speed "1 1 2 1 0.5"`, `# gain …`, `# begin …`, `# end …` are
-- |     mini-notation number patterns, queried over the cycle and SAMPLED at each
-- |     arc's onset (Tidal's structure-from-the-left: the placement provides the
-- |     structure, the verb patterns are read where each event lands). A bare
-- |     number (`# gain 0.8`) is a constant.
-- |   • GLITCH  — `# sometimes rev`, `# rarely (# speed 2)` are (probability,
-- |     effect) rules rolled per fired hit (deterministic on the seed).
-- |   • JUMPS   — `jump P` + `name -> target w  target w …` rows: a global jump
-- |     probability and a name→weighted-targets adjacency table steering the walk.
module Triggerfish.Stellatus.Lang
  ( Arc
  , KitEntry
  , Mode(..)
  , ArcParams
  , GlitchEffect(..)
  , GlitchRule
  , Target
  , JumpRow
  , JumpSpec
  , Scene
  , parseScene
  ) where

import Prelude

import Data.Array (find, head, mapMaybe, mapWithIndex, null, range, sortBy, uncons, (!!))
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Number (fromString) as Num
import Data.Rational (toNumber) as Rat
import Data.String (Pattern(..))
import Data.String (Replacement(..), replaceAll) as Str
import Data.String.CodeUnits (drop, indexOf, length, stripPrefix, take) as SCU
import Data.String.Common (split, trim)
import Tidal.Pattern.Core (firstCycle)
import Tidal.Pattern.Mini (parseMiniPattern)
import Tidal.Pattern.Types (Event(..))

type Arc = { onset :: Number, span :: Number, name :: String }

-- | One KIT line: a short name and its sample source (`bd`, `"808bd:3"`).
type KitEntry = { name :: String, src :: String }

data Mode = KitMode | BufferMode

derive instance eqMode :: Eq Mode

-- | Per-arc SuperDirt params, index-aligned to `arcs`. `speed`/`gain` carry
-- | defaults; `begin`/`end` are Nothing when the verb is absent (the emitter
-- | then falls back to the mode's natural window).
type ArcParams = { speed :: Number, gain :: Number, begin :: Maybe Number, end :: Maybe Number }

-- | A stochastic per-hit warp.
data GlitchEffect = GReverse | GSpeed Number

derive instance eqGlitchEffect :: Eq GlitchEffect

type GlitchRule = { prob :: Number, effect :: GlitchEffect }

type Target = { name :: String, weight :: Number }

type JumpRow = { from :: String, targets :: Array Target }

type JumpSpec = { prob :: Number, table :: Array JumpRow }

type Scene =
  { arcs :: Array Arc
  , mode :: Mode
  , kit :: Array KitEntry
  , params :: Array ArcParams
  , glitch :: Array GlitchRule
  , jumps :: JumpSpec
  }

-- | Parse the KIT text and PLAYER text into a scene. Left = a human-readable
-- | reason the PLAYER placement line didn't parse (the caller keeps the last
-- | good scene and shows the message). Verbs/glitch/jumps never fail the parse —
-- | an unparseable verb just leaves its default.
parseScene :: String -> String -> Either String Scene
parseScene kitText playerText = do
  place <- parsePlacement playerText
  let arcs = place.arcs
  pure
    { arcs
    , mode: place.mode
    , kit: parseKit kitText
    , params: buildParams playerText arcs
    , glitch: parseGlitch playerText
    , jumps: parseJumps playerText
    }

lines :: String -> Array String
lines = split (Pattern "\n")

-- | Tolerant number parse: accepts leading-dot forms (`.6`, `-.5`) that some
-- | `Data.Number.fromString` builds reject, then delegates.
num :: String -> Maybe Number
num s0 =
  let s = trim s0
      s' = case SCU.stripPrefix (Pattern ".") s of
        Just r -> "0." <> r
        Nothing -> case SCU.stripPrefix (Pattern "-.") s of
          Just r -> "-0." <> r
          Nothing -> s
  in Num.fromString s'

-- KIT lines: `name = "src"` → name + source (comments and blanks skipped).
parseKit :: String -> Array KitEntry
parseKit t = mapMaybe kitEntry (lines t)
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

-- ── Placement ────────────────────────────────────────────────────────────────

parsePlacement :: String -> Either String { arcs :: Array Arc, mode :: Mode }
parsePlacement txt =
  let ls = map trim (lines txt)
  in case find (startsWith "slice") ls of
       Just sl -> bufferArcs sl
       Nothing -> case find (startsWith "place") ls of
         Just pl -> kitArcs pl
         Nothing -> Left "expected a  place \"…\"  or  slice N  line"

bufferArcs :: String -> Either String { arcs :: Array Arc, mode :: Mode }
bufferArcs line =
  let rest = trim (SCU.drop 5 line)
  in case Int.fromString rest of
       Just n | n >= 1 -> Right { arcs: evenArcs n, mode: BufferMode }
       _ -> Left "slice expects a count, e.g.  slice 16"

evenArcs :: Int -> Array Arc
evenArcs n =
  map (\i -> { onset: Int.toNumber i / Int.toNumber n, span: 1.0 / Int.toNumber n, name: show i })
    (range 0 (n - 1))

kitArcs :: String -> Either String { arcs :: Array Arc, mode :: Mode }
kitArcs line = do
  q <- case quotedIn line of
    Just s -> Right s
    Nothing -> Left "place needs a quoted pattern, e.g.  place \"bd sn\""
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

-- ── Verbs (# speed / gain / begin / end) ─────────────────────────────────────

buildParams :: String -> Array Arc -> Array ArcParams
buildParams txt arcs =
  let spd = verbAt "speed" txt arcs
      gn = verbAt "gain" txt arcs
      bg = verbAt "begin" txt arcs
      en = verbAt "end" txt arcs
  in mapWithIndex
       (\i _ ->
          { speed: fromMaybe 1.0 (fromMaybe Nothing (spd !! i))
          , gain: fromMaybe 0.9 (fromMaybe Nothing (gn !! i))
          , begin: fromMaybe Nothing (bg !! i)
          , end: fromMaybe Nothing (en !! i)
          })
       arcs

-- The `# <verb> …` payload sampled at each arc's onset. A quoted mini-notation
-- pattern is queried over the cycle; a bare number is constant; absent = all
-- Nothing (default applied downstream).
verbAt :: String -> String -> Array Arc -> Array (Maybe Number)
verbAt verb txt arcs = case findVerb verb txt of
  Nothing -> map (const Nothing) arcs
  Just payload -> case quotedIn payload of
    Just pat -> sampleNums pat arcs
    Nothing -> let v = num payload in map (const v) arcs

findVerb :: String -> String -> Maybe String
findVerb verb txt =
  let pfx = "# " <> verb
  in case find (startsWith pfx) (map trim (lines txt)) of
       Just l -> Just (trim (SCU.drop (SCU.length pfx) l))
       Nothing -> Nothing

sampleNums :: String -> Array Arc -> Array (Maybe Number)
sampleNums pat arcs = case parseMiniPattern pat of
  Left _ -> map (const Nothing) arcs
  Right p ->
    let evs = mapMaybe numEv (firstCycle p)
    in map (\a -> sampleAt a.onset evs) arcs
  where
  numEv = case _ of
    Digital e ->
      let w = unwrap e.whole
      in map (\v -> { s: Rat.toNumber w.start, e: Rat.toNumber w.stop, v }) (num e.value)
    Analog _ -> Nothing
  sampleAt t evs = map _.v (find (\r -> t >= r.s && t < r.e) evs)

-- ── Glitch (# sometimes rev / # rarely (# speed 2)) ──────────────────────────

parseGlitch :: String -> Array GlitchRule
parseGlitch txt = mapMaybe rule (map trim (lines txt))
  where
  rule l = do
    body <- SCU.stripPrefix (Pattern "# ") l
    pw <- probWord body
    eff <- effect (trim pw.rest)
    pure { prob: pw.prob, effect: eff }
  effect r
    | startsWith "rev" r = Just GReverse
    | otherwise = map GSpeed (speedIn r)

probWord :: String -> Maybe { prob :: Number, rest :: String }
probWord s = do
  w <- firstWord s
  p <- probVal w
  pure { prob: p, rest: SCU.drop (SCU.length w) s }

probVal :: String -> Maybe Number
probVal = case _ of
  "always" -> Just 1.0
  "almostAlways" -> Just 0.9
  "often" -> Just 0.75
  "sometimes" -> Just 0.5
  "rarely" -> Just 0.25
  "almostNever" -> Just 0.1
  _ -> Nothing

speedIn :: String -> Maybe Number
speedIn r = case SCU.indexOf (Pattern "speed") r of
  Just i -> num (deParen (SCU.drop (i + 5) r))
  Nothing -> Nothing
  where
  deParen = Str.replaceAll (Pattern ")") (Str.Replacement "")
    <<< Str.replaceAll (Pattern "(") (Str.Replacement "")

-- ── Jumps (jump P / name -> target w …) ──────────────────────────────────────

parseJumps :: String -> JumpSpec
parseJumps txt =
  let ls = map trim (lines txt)
      prob = case find (startsWith "jump") ls of
        Just j -> fromMaybe 0.0 (num (trim (SCU.drop 4 j)))
        Nothing -> 0.0
  in { prob, table: mapMaybe row ls }
  where
  row l = case SCU.indexOf (Pattern "->") l of
    Just i -> do
      from <- firstWord (SCU.take i l)
      let targets = parseTargets (SCU.drop (i + 2) l)
      if null targets then Nothing else Just { from, targets }
    Nothing -> Nothing

parseTargets :: String -> Array Target
parseTargets s = pairs (mapMaybe nonEmpty (split (Pattern " ") (trim s)))
  where
  nonEmpty w = if trim w == "" then Nothing else Just (trim w)
  pairs ts = case uncons ts of
    Just { head: nm, tail: t1 } -> case uncons t1 of
      Just { head: w, tail: rest } -> case num w of
        Just weight -> [ { name: nm, weight } ] <> pairs rest
        Nothing -> []
      Nothing -> []
    Nothing -> []

-- The content between the first pair of double quotes.
quotedIn :: String -> Maybe String
quotedIn s = case SCU.indexOf (Pattern "\"") s of
  Just i ->
    let rest = SCU.drop (i + 1) s
    in case SCU.indexOf (Pattern "\"") rest of
         Just j -> Just (SCU.take j rest)
         Nothing -> Nothing
  Nothing -> Nothing
