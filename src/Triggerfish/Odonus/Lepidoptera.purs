-- | Triggerfish.Odonus.Lepidoptera — the eDSL print/parse for an Odonus
-- | **patch**: the whole authored setup, faithfully round-tripping. Where
-- | Balistes' fixed rhythms render as structured per-cell records, Odonus is the
-- | other case AC's rule points at — its sixteen cells are a *sequence*, so they
-- | serialise as parallel parameter-arrays (`notes: [...]`, `gates: [T,F,...]`):
-- | those columns compress and reveal the line far better than sixteen records.
-- |
-- | A patch spans more than the `Odonus` core: the gen matrix, the Marbles X-Y
-- | spread/bias, swing, velocity-humanise and the step divider all live on the
-- | component State, and they're part of what you authored — so the unit here is
-- | an `OdonusPatch`, captured from State. Runtime fields (playhead cursor /
-- | seqPos / accumulator / pendStep, the chord clock's ix / phase, the PRNG
-- | seed) are NOT serialised — they reset on load. The harmony is rendered as
-- | the single `quantize :: PitchSource` (scale | chords | vetula), the
-- | reframe's contract; see `Triggerfish.Odonus.PitchSource`.
-- |
-- | `odonusPatch "<name>" { … }` is valid-shaped `Tidal.*` eDSL, so a patch
-- | drops into Calypso and ships to purerl-tidal like every other Lepidoptera
-- | value. Print order is canonical and fixed; the parser reads that same order
-- | (value edits round-trip; reordering/deleting fields is the documented
-- | limitation — malformed input yields `Nothing` and the caller keeps state).
module Triggerfish.Odonus.Lepidoptera
  ( OdonusPatch
  , printPatch
  , parsePatch
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (fromFoldable, mapWithIndex, range, (!!))
import Data.Array (find, findIndex) as Array
import Data.Either (hush)
import Data.Foldable (minimumBy)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Ord (abs)
import Data.String.CodeUnits (fromCharArray)
import Data.String.Common (joinWith, toLower)
import Data.Tuple (Tuple(..), fst)
import Parsing (Parser, runParser) as Par
import Parsing.Combinators (optional, sepBy, try) as PC
import Parsing.Combinators.Array (many) as PCA
import Parsing.String (char, eof, satisfy, string)
import Parsing.String.Basic (intDecimal, number, skipSpaces)
import Triggerfish.Odonus.Grid.Types (GenKind(..), GenSource, genKinds)
import Triggerfish.Odonus.Model as M
import Triggerfish.Odonus.PitchSource (PitchSource(..), applyPitchSource, pitchSourceFrom)
import Triggerfish.Scale (Distribution(..), rootName, rootNames) as Scale

-- | The authored slice of the Odonus component: the model core plus the
-- | State-side fields that are part of the patch (not transport / runtime).
-- | `follow` is the live Vetula voice id (folded into `quantize` on print).
type OdonusPatch =
  { name :: String
  , odo :: M.Odonus
  , follow :: Maybe Int
  , gen :: Array GenSource
  , genSpread :: Number
  , genBias :: Number
  , swing :: Number          -- 0..0.6 (fraction of a step the off-beats lag)
  , velHumanize :: Int
  , stepDiv :: Int
  }

-- ---------------------------------------------------------------------------
-- Print
-- ---------------------------------------------------------------------------

printPatch :: OdonusPatch -> String
printPatch p =
  let
    o = p.odo
    ints xs = "[ " <> joinWith ", " (map show xs) <> " ]"
    bools xs = "[ " <> joinWith ", " (map (\b -> if b then "T" else "F") xs) <> " ]"
    cellInts f = ints (map f o.cells)
    cellBools f = bools (map f o.cells)
    pct x = show (round (x * 100.0))
  in
    joinWith "\n"
      [ "odonusPatch " <> show p.name
      , "  { scale: " <> Scale.rootName o.rootPc <> " " <> ints o.scaleIvls
      , "  , distribution: " <> show o.dist
      , "  , octave: " <> show o.octaveShift
      , "  , scalarTransp: " <> show o.degShift
      , "  , gate: " <> show o.gatePct
      , "  , quantize: " <> printSource (pitchSourceFrom o p.follow)
      , "  , swing: " <> pct p.swing
      , "  , velHumanize: " <> show p.velHumanize
      , "  , stepDiv: " <> show p.stepDiv
      , "  , marbles: { spread: " <> pct p.genSpread <> ", bias: " <> pct p.genBias <> " }"
      , "  , notes: " <> cellInts _.note
      , "  , gates: " <> cellBools _.gate
      , "  , skips: " <> cellBools _.skip
      , "  , glides: " <> cellBools _.glide
      , "  , durs: " <> cellInts _.dur
      , "  , ratchets: " <> cellInts _.ratchet
      , "  , vels: " <> cellInts _.vel
      , "  , heads:"
      , "      [ " <> joinWith "\n      , " (map printHead o.heads) <> " ]"
      , "  , gen:"
      , "      [ " <> joinWith "\n      , " (map printGen p.gen) <> " ]"
      , "  }"
      ]

printHead :: M.Head -> String
printHead hd =
  "head " <> patternSlug hd.patternIx <> " " <> show (M.speedOf hd) <> " " <> dirSlug hd.direction
    <> " transp " <> show hd.transp <> " off " <> show hd.offset <> " len " <> show hd.len
    <> " euclid " <> show hd.pulses <> " " <> show hd.esteps
    <> (if hd.mute then " mute" else "")

printGen :: GenSource -> String
printGen g =
  "source " <> genSlug g.kind <> " rate " <> show g.rate <> " amt " <> show g.amt
    <> (if g.on then " on" else " off")

printSource :: PitchSource -> String
printSource = case _ of
  PScale -> "scale"
  PChordsMcMullen picks per -> "chords mcmullen " <> intArr picks <> " every " <> show per
  PChordsPCs sets per ->
    "chords pcs [ " <> joinWith ", " (map intArr sets) <> " ] every " <> show per
  PVetula fid -> "vetula " <> show fid
  where
  intArr xs = "[ " <> joinWith ", " (map show xs) <> " ]"

-- ---------------------------------------------------------------------------
-- Parse
-- ---------------------------------------------------------------------------

type Parser a = Par.Parser String a

parsePatch :: String -> Maybe OdonusPatch
parsePatch input = hush (Par.runParser input (patchP <* eof))

patchP :: Parser OdonusPatch
patchP = do
  ws
  _ <- sym "odonusPatch"
  name <- strL
  _ <- sym "{"
  Tuple root ivls <- fld "scale" scaleVal
  dist <- fld "distribution" distVal
  octave <- fld "octave" intL
  scalarT <- fld "scalarTransp" intL
  gatePct <- fld "gate" intL
  quant <- fld "quantize" sourceVal
  swing <- fld "swing" intL
  velH <- fld "velHumanize" intL
  stepDiv <- fld "stepDiv" intL
  Tuple spread bias <- fld "marbles" marblesVal
  notes <- fld "notes" intArray
  gates <- fld "gates" boolArray
  skips <- fld "skips" boolArray
  glides <- fld "glides" boolArray
  durs <- fld "durs" intArray
  ratchets <- fld "ratchets" intArray
  vels <- fld "vels" intArray
  heads <- fld "heads" headsArray
  gen <- fld "gen" genArray
  _ <- sym "}"
  let
    cells = mapWithIndex
      ( \i _ ->
          { note: at notes i 60, gate: at gates i true, skip: at skips i false
          , glide: at glides i false, dur: at durs i 1, ratchet: at ratchets i 1
          , vel: at vels i 100 }
      )
      (range 0 15)
    baseOdo = M.defaultOdonus
      { rootPc = root, scaleIvls = ivls, dist = dist
      , octaveShift = octave, degShift = scalarT, gatePct = gatePct
      , cells = cells, heads = heads }
    applied = applyPitchSource quant baseOdo
  pure
    { name, odo: applied.odo, follow: applied.follow
    , gen, genSpread: toNumber spread / 100.0, genBias: toNumber bias / 100.0
    , swing: toNumber swing / 100.0, velHumanize: velH, stepDiv }
  where
  at :: forall a. Array a -> Int -> a -> a
  at arr i d = fromMaybe d (arr !! i)

-- --- field + value parsers --------------------------------------------------

-- A `key: value` field, tolerating the trailing comma between fields.
fld :: forall a. String -> Parser a -> Parser a
fld k vp = sym k *> sym ":" *> vp <* PC.optional (sym ",")

scaleVal :: Parser (Tuple Int (Array Int))
scaleVal = Tuple <$> rootP <*> intArray

distVal :: Parser Scale.Distribution
distVal = (Scale.Natural <$ sym "Natural") <|> (Scale.Equal <$ sym "Equal")

marblesVal :: Parser (Tuple Int Int)
marblesVal = do
  _ <- sym "{"
  _ <- sym "spread"
  _ <- sym ":"
  sp <- intL
  _ <- sym ","
  _ <- sym "bias"
  _ <- sym ":"
  bi <- intL
  _ <- sym "}"
  pure (Tuple sp bi)

sourceVal :: Parser PitchSource
sourceVal = PC.try scaleSrc <|> PC.try chordsSrc <|> vetulaSrc
  where
  scaleSrc = PScale <$ sym "scale"
  vetulaSrc = sym "vetula" *> (PVetula <$> intL)
  chordsSrc = sym "chords" *> (mcmullen <|> pcs)
  mcmullen = do
    _ <- sym "mcmullen"
    ps <- intArray
    _ <- sym "every"
    per <- intL
    pure (PChordsMcMullen ps per)
  pcs = do
    _ <- sym "pcs"
    sets <- fromFoldable <$> bracketed (PC.sepBy intArray (sym ","))
    _ <- sym "every"
    per <- intL
    pure (PChordsPCs sets per)

headsArray :: Parser (Array M.Head)
headsArray = fromFoldable <$> bracketed (PC.sepBy headP (sym ","))

headP :: Parser M.Head
headP = do
  _ <- sym "head"
  pat <- patternIxOf <$> ident
  spd <- speedIxOf <$> numberL
  dir <- dirOf <$> ident
  _ <- sym "transp"
  tr <- intL
  _ <- sym "off"
  off <- intL
  _ <- sym "len"
  ln <- intL
  _ <- sym "euclid"
  pul <- intL
  est <- intL
  mute <- (true <$ PC.try (sym "mute")) <|> pure false
  pure
    { cursor: 0, seqPos: 0, accumulator: 0, pendStep: 1
    , speedIx: spd, direction: dir, transp: tr, mute, patternIx: pat
    , offset: off, len: ln, pulses: pul, esteps: est }

genArray :: Parser (Array GenSource)
genArray = fromFoldable <$> bracketed (PC.sepBy genP (sym ","))

genP :: Parser GenSource
genP = do
  _ <- sym "source"
  k <- genOf <$> ident
  _ <- sym "rate"
  r <- intL
  _ <- sym "amt"
  a <- intL
  on <- (true <$ sym "on") <|> (false <$ sym "off")
  pure { kind: k, on, rate: r, amt: a }

-- --- name maps --------------------------------------------------------------

patternSlug :: Int -> String
patternSlug i = toLower (maybe "rows" _.name (M.patternLibrary !! i))

patternIxOf :: String -> Int
patternIxOf slug = fromMaybe 0 (Array.findIndex (\p -> toLower p.name == slug) M.patternLibrary)

speedIxOf :: Number -> Int
speedIxOf v =
  maybe 4 fst (minimumBy (comparing (\(Tuple _ s) -> abs (s - v))) (mapWithIndex Tuple M.speedTable))

dirSlug :: Int -> String
dirSlug n = case n of
  1 -> "back"
  2 -> "pend"
  _ -> "fwd"

dirOf :: String -> Int
dirOf s = case s of
  "back" -> 1
  "pend" -> 2
  _ -> 0

genSlug :: GenKind -> String
genSlug = case _ of
  GNotes -> "notes"
  GGate -> "gate"
  GSkip -> "skip"
  GGlide -> "glide"
  GLen -> "len"
  GRatchet -> "ratchet"
  GHeads -> "heads"
  GTransp -> "transp"
  GPattern -> "pattern"
  GSpeed -> "speed"
  GKey -> "key"

genOf :: String -> GenKind
genOf slug = fromMaybe GNotes (Array.find (\k -> genSlug k == slug) genKinds)

rootP :: Parser Int
rootP = fromMaybe 0 <$> (matchRoot <$> ident')
  where
  -- A root token: an upper-case letter then an optional accidental char.
  ident' = do
    c <- satisfy (\ch -> ch >= 'A' && ch <= 'G')
    acc <- (PCA.many (satisfy (\ch -> ch == '#' || ch == 'b')))
    ws
    pure (fromCharArray ([ c ] <> acc))
  matchRoot tok = Array.findIndex (_ == tok) Scale.rootNames

-- --- lexing helpers ---------------------------------------------------------

ws :: Parser Unit
ws = skipSpaces

sym :: String -> Parser Unit
sym s = void (string s) <* ws

intL :: Parser Int
intL = intDecimal <* ws

numberL :: Parser Number
numberL = number <* ws

strL :: Parser String
strL = stringLit <* ws

stringLit :: Parser String
stringLit = char '"' *> (fromCharArray <$> PCA.many (satisfy (_ /= '"'))) <* char '"'

-- An identifier: a run of lower-case letters.
ident :: Parser String
ident = (fromCharArray <$> PCA.many (satisfy (\c -> c >= 'a' && c <= 'z'))) <* ws

intArray :: Parser (Array Int)
intArray = fromFoldable <$> bracketed (PC.sepBy intL (sym ","))

boolArray :: Parser (Array Boolean)
boolArray = fromFoldable <$> bracketed (PC.sepBy boolL (sym ","))

boolL :: Parser Boolean
boolL = (true <$ sym "T") <|> (false <$ sym "F")

bracketed :: forall a. Parser a -> Parser a
bracketed p = sym "[" *> p <* sym "]"
