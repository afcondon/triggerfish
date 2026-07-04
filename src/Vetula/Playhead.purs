-- | `Vetula.Playhead` — evaluate a mini-notation playhead into a reef `PerfClock`.
-- |
-- | A Vetula performance voice reads the loaded progression on its own read-head.
-- | Historically that read-head was a bars-per-chord `durs` array (a form/grid);
-- | here it becomes a live-coded **Tidal pattern of chord indices** — `"0 1 2 3"`,
-- | `"<0 2> 1"`, `"0 [1 2] 3"`, `"0(3,8)"` — parsed by the vendored Tidal engine and
-- | QUERIED into the exact same `{ ix, start, len }` segment clock the shared reef
-- | realiser (`Reef.Vetula.Perf.renderClockMidiAt`) already consumes. So block / arp
-- | / strum are untouched: the pattern only changes WHERE the segments come from.
-- |
-- | Convention (decided with the rig's 1/16 grid): **one pattern cycle = one bar =
-- | 16 pulses**. `"0 1 2 3"` is a chord per beat; `"[0 1 2 3]/4"` is one chord per bar
-- | over four bars (the old durs=[1,1,1,1] default — bracketed `/` slow; note this
-- | engine's `<…>` is grouping, NOT slow-alternation). Sub-bar subdivisions that don't
-- | divide 16 (triplets, …) round to the nearest pulse — the rig IS a 16-grid.
-- |
-- | This is the FRONTEND half of the pattern path (SOLO). The identical parse+query
-- | moves into reef when the rig catches up (#77) — the segment clock is the seam,
-- | so browser and BEAM stay byte-identical by construction.
module Vetula.Playhead
  ( patternClock
  , clockFor
  , noteClock
  , defaultPattern
  ) where

import Prelude

import Control.Alternative (guard)
import Data.Array (concatMap, find, mapMaybe, range, sortBy)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Rational (Rational, fromInt, toNumber)
import Data.String (joinWith, trim)
import Data.Tuple (Tuple(..))
import Reef.Vetula.Perf (PerfClock, Seg, clockOfDurs)
import Tidal.Pattern.Mini (parseMiniPattern)
import Tidal.Pattern.Types (Arc(..), Event, State, eventValue, eventWhole, isDigital, mkArc, mkState, query, Pattern)

-- | Pulses per pattern cycle (= per bar). The whole rig rides this 1/16 grid.
pulsesPerCycle :: Int
pulsesPerCycle = 16

-- | Cap on the cycles we scan for a pattern's period (`<a b c …>` alternations).
-- | 16 covers any reasonable hand-written alternation; beyond it the loop is just
-- | truncated to 16 bars, which no one is live-coding by hand.
capCycles :: Int
capCycles = 16

-- | A cycle-time (Rational, in cycles) to an absolute pulse index on the 16-grid.
toPulse :: Rational -> Int
toPulse t = Int.round (toNumber t * Int.toNumber pulsesPerCycle)

-- | The clock a voice actually plays: its committed pattern if it has a non-empty,
-- | PARSEABLE one, otherwise its legacy `durs` (so untouched voices keep working and
-- | an empty field means "no pattern yet"). `nChords` bounds valid indices.
clockFor :: Int -> { pattern :: String, durs :: Array Int } -> PerfClock
clockFor nChords v =
  if trim v.pattern == "" then clockOfDurs nChords v.durs
  else case patternClock nChords v.pattern of
    Right clock -> clock
    Left _ -> clockOfDurs nChords v.durs

-- | A NOTE-index pattern → clock (Axis B). Same evaluation as `patternClock`, but the
-- | indices are note positions within a chord, bounded generously (reef wraps them into
-- | the actual chord size at play time). So `"0 1 2 3"` arps, `"3"` holds the top note,
-- | `"[0 1 2 3]*4"` is a fast arp. Empty or unparseable → Nothing (fall back to the
-- | voice's block/arp/strum renderer).
noteClock :: String -> Maybe PerfClock
noteClock src =
  if trim src == "" then Nothing
  else case patternClockBounded (-127) 128 src of
    Right c -> Just c
    Left _ -> Nothing

-- | Parse + query a mini-notation string into a segment clock, or return the parse
-- | error (for the commit UI). Indices outside `0 .. nChords-1` are dropped (they
-- | read as rests), so a pattern can safely outlive a shortened progression.
patternClock :: Int -> String -> Either String PerfClock
patternClock nChords = patternClockBounded 0 nChords

-- | `patternClock` generalised to an arbitrary index range `[lo, hi)`. Chord patterns
-- | use `[0, nChords)`; note patterns use a range that ADMITS NEGATIVES (from-the-top
-- | indexing, `-1` = highest), reef normalising them into the actual chord at play time.
patternClockBounded :: Int -> Int -> String -> Either String PerfClock
patternClockBounded lo hi src = case parseMiniPattern src of
  Left err -> Left err
  Right pat ->
    let period = detectPeriod pat
        segs = sortBy (comparing _.start)
                 (concatMap (cycleSegs pat lo hi) (range 0 (period - 1)))
    in Right { segs, loopLen: period * pulsesPerCycle }

-- | The segments whose ONSET falls in cycle `c` (so an event is counted once, in the
-- | cycle it starts). Each digital event becomes `{ ix, start, len }` in pulses.
cycleSegs :: Pattern String -> Int -> Int -> Int -> Array Seg
cycleSegs pat lo hi c =
  mapMaybe (toSeg lo hi c) (query pat (cycleArc c))

-- | One digital, in-cycle, in-range `[lo, hi)`, positive-length event → a segment.
-- | Anything else (analog, wrong cycle, non-numeric / out-of-range, zero length) drops.
toSeg :: Int -> Int -> Int -> Event String -> Maybe Seg
toSeg lo hi c ev = do
  guard (isDigital ev)
  Arc w <- eventWhole ev
  guard (w.start >= fromInt c && w.start < fromInt (c + 1))
  ix <- Int.fromString (trim (eventValue ev))
  guard (ix >= lo && ix < hi)
  let start = toPulse w.start
      len = toPulse w.stop - start
  guard (len > 0)
  pure { ix, start, len }

-- | Smallest cycle count after which the pattern repeats, capped. A pattern has
-- | period `p` when cycle `p`, shifted back to the origin, matches cycle 0.
detectPeriod :: Pattern String -> Int
detectPeriod pat =
  let sig0 = cycleSig pat 0
  in fromMaybe capCycles (find (\p -> cycleSig pat p == sig0) (range 1 capCycles))

-- | A cycle's event signature, normalised to the origin (whole start/stop minus the
-- | cycle index) and sorted — so two cycles compare equal iff they'd sound the same.
cycleSig :: Pattern String -> Int -> Array (Tuple Rational (Tuple Rational String))
cycleSig pat c =
  sortBy (comparing (\(Tuple s _) -> toNumber s))
    (mapMaybe entry (query pat (cycleArc c)))
  where
  entry ev = do
    guard (isDigital ev)
    Arc w <- eventWhole ev
    guard (w.start >= fromInt c && w.start < fromInt (c + 1))
    pure (Tuple (w.start - fromInt c) (Tuple (w.stop - fromInt c) (eventValue ev)))

cycleArc :: Int -> State
cycleArc c = mkState (mkArc (fromInt c) (fromInt (c + 1)))

-- | The pattern that reproduces the old "one chord per bar, in order" default under
-- | the one-cycle-per-bar convention: `[0 1 … n-1]/n` (bracketed slow — one chord per
-- | bar over n bars). A single chord needs no slow; empty progression → empty.
defaultPattern :: Int -> String
defaultPattern n
  | n <= 0 = ""
  | n == 1 = "0"
  | otherwise = "[" <> joinWith " " (map show (range 0 (n - 1))) <> "]/" <> show n
