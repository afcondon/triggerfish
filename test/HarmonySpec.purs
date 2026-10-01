-- | **Vetula's harmony text means what Vetula's clock means.**
-- |
-- | Vetula tells Odonus its harmony as a Tidal pattern written by reef
-- | (`Reef.Vetula.Harmony.clockHarmony`), and the hosts read it back with
-- | Littorina (`Tidal.Harmony.harmonySampler`). The law: at every pulse, the
-- | pitch classes Littorina samples from the text are those of the chord the
-- | voice's own clock is on (`Reef.Vetula.Perf.cursorAtClock`), holding the
-- | last chord through a rest as the conductor does.
-- |
-- | Checked over every bars-per-chord clock of up to three chords (skips
-- | included) at every phase, and over clocks whose loop is not a whole number
-- | of bars, which write a fractional `/` and so exercise Tidal's reading of a
-- | decimal rate (the check that found Littorina rounding `/0.5625` to
-- | thousandths). Pulses are compared over the second and third loops, once the
-- | held chord is known.
module Test.HarmonySpec (runHarmonyTests) where

import Prelude

import Data.Array (concatMap, filter, length, nub, range, sort, (!!))
import Data.Foldable (foldl)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Haskell.Integer as Integer
import Haskell.Rational (ratio)
import Effect (Effect)
import Effect.Console (log)
import Reef.Vetula.Harmony (clockHarmony)
import Reef.Vetula.Perf (PerfClock, clockOfDurs, cursorAtClock)
import Test.Assert (assertTrue')
import Tidal.Harmony (harmonyAt, parseHarmony)

chords :: Array (Array Int)
chords = [ [ 60, 64, 67 ], [ 62, 65, 69, 72 ], [ 55, 59, 62, 65 ] ]

pcs :: Array Int -> Array Int
pcs = sort <<< nub <<< map (\n -> ((n `mod` 12) + 12) `mod` 12)

-- | Every pulse of loops two and three where the text and the clock disagree.
mismatches :: PerfClock -> Int -> Array Int
mismatches clock phase = case clockHarmony chords clock phase of
  Nothing -> if clock.loopLen > 0 then [ -1 ] else []
  Just txt -> case parseHarmony txt of
   Left _ -> [ -2 ]
   Right h ->
    let
      walk acc p =
        let
          cur = case cursorAtClock clock phase p of
            Just c -> Just c
            Nothing -> acc.cur
          want = pcs (fromMaybe [] (cur >>= (chords !! _)))
          bad = p >= clock.loopLen && harmonyAt h (Integer.fromInt p `ratio` Integer.fromInt 16) /= want
        in
          { cur, out: if bad then acc.out <> [ p ] else acc.out }
    in
      (foldl walk { cur: Nothing, out: [] } (range 0 (3 * clock.loopLen - 1))).out

-- | Every bars-per-chord column of 1..3 chords, each 0..3 bars.
dursClocks :: Array PerfClock
dursClocks = concatMap (\n -> map (clockOfDurs n) (columns n)) [ 1, 2, 3 ]
  where
  columns n = foldl (\acc _ -> concatMap (\c -> map (\d -> c <> [ d ]) (range 0 3)) acc) [ [] ] (range 1 n)

-- | Loops of odd lengths, with gaps between segments and after the last.
oddClocks :: Array PerfClock
oddClocks =
  [ { segs: [ { ix: 0, start: 0, len: 6 }, { ix: 1, start: 6, len: 3 } ], loopLen: 9 }
  , { segs: [ { ix: 2, start: 1, len: 4 }, { ix: 0, start: 7, len: 2 } ], loopLen: 11 }
  , { segs: [ { ix: 1, start: 0, len: 5 } ], loopLen: 7 }
  , { segs: [ { ix: 0, start: 0, len: 4 }, { ix: 1, start: 4, len: 17 } ], loopLen: 21 }
  , { segs: [ { ix: 0, start: 2, len: 3 }, { ix: 2, start: 5, len: 3 }, { ix: 1, start: 9, len: 1 } ], loopLen: 13 }
  ]

runHarmonyTests :: Effect Unit
runHarmonyTests = do
  log "Vetula harmony text vs Vetula's clock, sampled by Littorina"
  let
    every step cs = concatMap (\c -> map (\ph -> { c, ph }) (filter (\ph -> ph `mod` step == 0) (range 0 (max 0 (c.loopLen - 1))))) cs
    check name step cs0 = do
      let
        cases = every step cs0
        failing = filter (\k -> length (mismatches k.c k.ph) > 0) cases
      log ("   " <> name <> ": " <> show (length cases) <> " clock/phase pairs, "
        <> show (length failing) <> " disagree")
      assertTrue' (name <> ": harmony text and clock disagree") (length failing == 0)
  check "bars-per-chord clocks, every third phase" 3 dursClocks
  check "odd-length loops, every phase" 1 oddClocks
  -- one text, to read
  log ("   e.g. " <> fromMaybe "" (clockHarmony chords (clockOfDurs 3 [ 2, 0, 1 ]) 4))
