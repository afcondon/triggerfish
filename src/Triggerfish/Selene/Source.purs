-- | Triggerfish.Selene.Source — the round-trip between the rack and an editable
-- | text document. Line-oriented (not the bracketed eDSL) because the document
-- | is the instrument: you edit the numbers here and the visualisations follow.
-- |
-- | Grammar (POLYTRIG relocated to Balistes' TIDAL tab; Selene is CV/gate only):
-- |   <kind> <target>          opens a destination — a group of 8. kind ∈
-- |                            lfo | euclid | clock | note; target is a wire
-- |                            token (es9main, es9gt0, es98cv0, fh2_0, midi1,
-- |                            virtual:bus).
-- |   <slot>                   one line per slot, following the header; the
-- |                            block ends at the next blank line.
-- |   -- <slot>                a muted slot (that output goes silent); the
-- |                            position still counts, so mutes don't shift the
-- |                            bank. A blank line ends a block, so comments in
-- |                            the gaps between blocks are free notes.
-- |
-- | Slot syntax per kind:
-- |   lfo     <rate> @<phase> [lvl <v>] [sin <a>] [sqr <a>] [tri <a>] [saw <a>] [rnd <a>] [nse <a>]
-- |   euclid  <beats> <steps> [@<rate>] [acc <n>]
-- |   clock   <base> x<mult> [<pw>%] [ph<deg>]      (base = 1/4, 1/8T, …)
-- |   note    <name-or-midi>                        (C4 or 60)
-- |
-- | `parseRack` is total + lenient: bad fields fall back to defaults and short
-- | blocks pad to eight with silent slots, so the live viz never blanks while
-- | you type. The printer matches the parser, so print∘parse round-trips.
module Triggerfish.Selene.Source
  ( printRack
  , printDest
  , parseRack
  ) where

import Prelude

import Data.Array (drop, filter, foldl, mapWithIndex, range, snoc, take, (!!))
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number as Number
import Data.String as Str
import Data.String.Common (joinWith, toLower)
import Triggerfish.Selene.Model as M

-- ---------------------------------------------------------------------------
-- Print
-- ---------------------------------------------------------------------------

printRack :: M.Selene -> String
printRack sel =
  legend <> "\n\n" <> joinWith "\n\n" (map printDest sel.destinations)
  where
  legend =
    joinWith "\n"
      [ "-- SELENE · edit the numbers; the rack follows."
      , "-- <kind> <target> opens a group of 8 · one slot per line · -- mutes a slot." ]

-- | One destination: a `<kind> <target>` header then eight tagged slots.
printDest :: M.Destination -> String
printDest d =
  (kindKeyword d.bank <> " " <> M.targetWire d.target) <> "\n"
    <> joinWith "\n" (mapWithIndex slotLine (bankSlots d.bank))
  where
  slotLine i row = "  " <> row <> "   -- " <> show (i + 1)
  bankSlots = case _ of
    M.GLfo slots -> map lfoLine slots
    M.GEuclid slots -> map euclidLine slots
    M.GClock slots -> map clockLine slots
    M.GNote slots -> map noteLine slots

kindKeyword :: M.GenBank -> String
kindKeyword = case _ of
  M.GLfo _ -> "lfo"
  M.GEuclid _ -> "euclid"
  M.GClock _ -> "clock"
  M.GNote _ -> "note"

lfoLine :: M.ModSlot -> String
lfoLine sl =
  fmt sl.rate <> " @" <> fmt sl.phase
    <> amp "lvl" sl.level
    <> amp "sin" sl.sin
    <> amp "sqr" sl.sqr
    <> amp "tri" sl.tri
    <> amp "saw" sl.saw
    <> amp "rnd" sl.rnd
    <> amp "nse" sl.nse
  where
  amp k v = if v == 0.0 then "" else " " <> k <> " " <> fmt v

euclidLine :: M.EuclidSlot -> String
euclidLine sl =
  show sl.beats <> " " <> show sl.steps <> " @" <> show sl.rate
    <> (if sl.accentRate == 0 then "" else " acc " <> show sl.accentRate)

clockLine :: M.ClockSlot -> String
clockLine sl =
  M.clockBaseLabel sl.base <> " x" <> show sl.multiplier <> " " <> show sl.pulseWidth <> "%"
    <> (if sl.phase == 0 then "" else " ph" <> show sl.phase)

noteLine :: M.PresetNoteSlot -> String
noteLine sl = M.noteName sl.note

-- ---------------------------------------------------------------------------
-- Parse — total, lenient
-- ---------------------------------------------------------------------------

-- | Accumulator: the destinations closed so far + the block currently open.
-- | `rows` are slot lines for the four bank kinds (Nothing = muted).
type PState =
  { dests :: Array M.Destination
  , open :: Maybe
      { target :: M.Target
      , kind :: M.GenKind
      , rows :: Array (Maybe String)
      }
  }

parseRack :: String -> M.Selene
parseRack doc =
  let
    final = foldl step { dests: [], open: Nothing } (Str.split (Str.Pattern "\n") doc)
  in
    { destinations: (closeBlock final).dests }
  where
  step :: PState -> String -> PState
  step st line =
    let t = Str.trim line
    in
      if t == "" then closeBlock st
      else case headerOf t of
        Just hdr -> (closeBlock st) { open = Just { target: hdr.target, kind: hdr.kind, rows: [] } }
        Nothing -> case st.open of
          Just blk -> st { open = Just (addLine blk t) }
          Nothing -> st   -- a stray line outside any block: ignore

  -- A line inside an open block: every line is a slot row (muted → Nothing).
  addLine blk line = blk { rows = snoc blk.rows (slotText line) }

-- A slot's text, or Nothing if the line is muted (leading `--`). Trailing
-- `-- n` tags are stripped either way.
slotText :: String -> Maybe String
slotText t =
  if isMuted then Nothing else Just (stripTrailingComment t)
  where
  isMuted = maybe false (const true) (Str.stripPrefix (Str.Pattern "--") t)

stripTrailingComment :: String -> String
stripTrailingComment s = case Str.indexOf (Str.Pattern "--") s of
  Just i -> Str.trim (Str.take i s)
  Nothing -> Str.trim s

-- Close the open block (if any), padding/truncating it to eight slots.
closeBlock :: PState -> PState
closeBlock st = case st.open of
  Nothing -> st
  Just blk ->
    let bank = buildBank blk.kind (fitTo M.slotCount blk.rows)
    in { dests: snoc st.dests { target: blk.target, range: M.Bipolar5V, bank }, open: Nothing }

-- Pad/truncate the row list to n (missing positions are muted → silent).
fitTo :: Int -> Array (Maybe String) -> Array (Maybe String)
fitTo n rows = take n (rows <> map (const Nothing) (range 0 n))

buildBank :: M.GenKind -> Array (Maybe String) -> M.GenBank
buildBank kind rows = case kind of
  M.KLfo -> M.GLfo (map (maybe silentLfo parseLfo) rows)
  M.KEuclid -> M.GEuclid (map (maybe silentEuclid parseEuclid) rows)
  M.KClock -> M.GClock (map (maybe silentClock parseClock) rows)
  M.KNote -> M.GNote (map (maybe silentNote parseNote) rows)

-- ---------------------------------------------------------------------------
-- Header
-- ---------------------------------------------------------------------------

-- A block header — exactly `<kind> <target>`.
headerOf :: String -> Maybe { kind :: M.GenKind, target :: M.Target }
headerOf t = case words t of
  [] -> Nothing
  toks -> case toks !! 0 >>= kindOf of
    Just k -> case toks of
      [ _, tgt ] -> Just { kind: k, target: parseTarget tgt }
      _ -> Nothing
    Nothing -> Nothing

kindOf :: String -> Maybe M.GenKind
kindOf = case _ of
  "lfo" -> Just M.KLfo
  "euclid" -> Just M.KEuclid
  "clock" -> Just M.KClock
  "note" -> Just M.KNote
  _ -> Nothing

parseTarget :: String -> M.Target
parseTarget tok =
  fromMaybe (M.Virtual tok) (afterInt "es9gt" M.ES9Gt <|> afterInt "es98cv" M.ES9Cv <|> afterInt "fh2_" M.FH2 <|> afterInt "midi" M.Midi <|> exact)
  where
  exact = case tok of
    "es9main" -> Just M.ES9Main
    _ -> case Str.stripPrefix (Str.Pattern "virtual:") tok of
      Just s -> Just (M.Virtual s)
      Nothing -> Nothing
  afterInt pfx ctor = case Str.stripPrefix (Str.Pattern pfx) tok of
    Just rest -> map ctor (Int.fromString rest)
    Nothing -> Nothing

-- ---------------------------------------------------------------------------
-- Per-kind slot parsers + silent defaults
-- ---------------------------------------------------------------------------

silentLfo :: M.ModSlot
silentLfo = { rate: 0.0, phase: 0.0, level: 0.0, sin: 0.0, sqr: 0.0, tri: 0.0, saw: 0.0, rnd: 0.0, nse: 0.0 }

silentEuclid :: M.EuclidSlot
silentEuclid = { beats: 0, steps: 8, rate: 4, accentRate: 0 }

silentClock :: M.ClockSlot
silentClock = { base: M.ClockQuarter, multiplier: 1, pulseWidth: 0, phase: 0 }

silentNote :: M.PresetNoteSlot
silentNote = { note: 0 }

-- lfo  <rate> @<phase> [lvl v] [sin a] [sqr a] [tri a] [saw a] [rnd a] [nse a]
parseLfo :: String -> M.ModSlot
parseLfo s = foldl apply (silentLfo { rate = rate0 }) (pairs rest)
  where
  toks = words s
  rate0 = num (fromMaybe "0" (toks !! 0))
  rest = drop 1 toks
  -- consume `@x` as phase; consume `key val` shape pairs
  pairs = collectPairs
  apply sl = case _ of
    Phase p -> sl { phase = p }
    Shape "lvl" v -> sl { level = v }
    Shape "sin" v -> sl { sin = v }
    Shape "sqr" v -> sl { sqr = v }
    Shape "tri" v -> sl { tri = v }
    Shape "saw" v -> sl { saw = v }
    Shape "rnd" v -> sl { rnd = v }
    Shape "nse" v -> sl { nse = v }
    _ -> sl

data Tok = Phase Number | Shape String Number

collectPairs :: Array String -> Array Tok
collectPairs = go
  where
  go toks = case toks !! 0 of
    Nothing -> []
    Just h -> case Str.stripPrefix (Str.Pattern "@") h of
      Just p -> snocTok (Phase (num p)) (go (drop 1 toks))
      Nothing -> case toks !! 1 of
        Just v -> snocTok (Shape (toLower h) (num v)) (go (drop 2 toks))
        Nothing -> []
  snocTok x xs = [ x ] <> xs

-- euclid <beats> <steps> [@rate] [acc n]
parseEuclid :: String -> M.EuclidSlot
parseEuclid s =
  { beats: int (idx 0) silentEuclid.beats
  , steps: max 1 (int (idx 1) silentEuclid.steps)
  , rate: fromMaybe 4 atRate
  , accentRate: fromMaybe 0 accVal
  }
  where
  toks = words s
  idx i = fromMaybe "" (toks !! i)
  atRate = firstJust (map (\tk -> map (\r -> r) (intMaybe =<< Str.stripPrefix (Str.Pattern "@") tk)) toks)
  accVal = afterKey "acc" toks

-- clock <base> x<mult> [<pw>%] [ph<deg>]
parseClock :: String -> M.ClockSlot
parseClock s =
  { base: parseBase (idx 0)
  , multiplier: fromMaybe 1 mult
  , pulseWidth: fromMaybe 50 pw
  , phase: fromMaybe 0 ph
  }
  where
  toks = words s
  idx i = fromMaybe "" (toks !! i)
  mult = firstJust (map (\tk -> intMaybe =<< Str.stripPrefix (Str.Pattern "x") tk) toks)
  pw = firstJust (map (\tk -> intMaybe =<< Str.stripSuffix (Str.Pattern "%") tk) toks)
  ph = firstJust (map (\tk -> intMaybe =<< Str.stripPrefix (Str.Pattern "ph") tk) toks)

-- note <name-or-midi>
parseNote :: String -> M.PresetNoteSlot
parseNote s = { note: M.clampI 0 127 (fromMaybe 60 (noteToken (fromMaybe "" (words s !! 0)))) }


noteToken :: String -> Maybe Int
noteToken tok = case Int.fromString tok of
  Just n -> Just n
  Nothing -> parseNoteName tok

-- C4 / C#3 / Eb… (just sharps) → MIDI
parseNoteName :: String -> Maybe Int
parseNoteName tok =
  case Str.uncons tok of
    Just { head, tail } -> do
      pc0 <- pcOf (Str.toUpper (Str.singleton head))
      let sharp = isJustPrefix "#" tail
          octStr = if sharp then Str.drop 1 tail else tail
      oct <- Int.fromString octStr
      Just ((oct + 1) * 12 + pc0 + (if sharp then 1 else 0))
    Nothing -> Nothing
  where
  isJustPrefix p s = maybe false (const true) (Str.stripPrefix (Str.Pattern p) s)
  pcOf = case _ of
    "C" -> Just 0
    "D" -> Just 2
    "E" -> Just 4
    "F" -> Just 5
    "G" -> Just 7
    "A" -> Just 9
    "B" -> Just 11
    _ -> Nothing

parseBase :: String -> M.ClockBase
parseBase lbl = fromMaybe M.ClockQuarter (find (Str.toUpper lbl))
  where
  find l = case l of
    "1/1" -> Just M.ClockWhole
    "1/2" -> Just M.ClockHalf
    "1/4" -> Just M.ClockQuarter
    "1/4T" -> Just M.ClockQuarterT
    "1/8" -> Just M.ClockEighth
    "1/8T" -> Just M.ClockEighthT
    "1/16" -> Just M.ClockSixteenth
    "1/16T" -> Just M.ClockSixteenthT
    "1/32" -> Just M.ClockThirtySecond
    "1/32T" -> Just M.ClockThirtySecondT
    "1/64T" -> Just M.ClockSixtyFourthT
    _ -> Nothing

-- ---------------------------------------------------------------------------
-- helpers
-- ---------------------------------------------------------------------------

words :: String -> Array String
words = filter (_ /= "") <<< Str.split (Str.Pattern " ") <<< normalizeWs

-- collapse tabs to spaces (split only on space)
normalizeWs :: String -> String
normalizeWs = Str.replaceAll (Str.Pattern "\t") (Str.Replacement " ")

num :: String -> Number
num s = fromMaybe 0.0 (Number.fromString s)

int :: String -> Int -> Int
int s d = fromMaybe d (Int.fromString s)

intMaybe :: String -> Maybe Int
intMaybe = Int.fromString

afterKey :: String -> Array String -> Maybe Int
afterKey key toks = go 0
  where
  go i = case toks !! i of
    Just k | k == key -> intMaybe =<< toks !! (i + 1)
    Just _ -> go (i + 1)
    Nothing -> Nothing

firstJust :: forall a. Array (Maybe a) -> Maybe a
firstJust = foldl (\acc x -> acc <|> x) Nothing

fmt :: Number -> String
fmt x = show (Int.toNumber (Int.round (x * 100.0)) / 100.0)

-- local <|> for Maybe so we needn't pull Control.Alt's name into scope twice
infixl 3 alt as <|>

alt :: forall a. Maybe a -> Maybe a -> Maybe a
alt a b = case a of
  Just _ -> a
  Nothing -> b
