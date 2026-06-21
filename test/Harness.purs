-- | A headless sampling harness for QA. It drives the REAL engine — the same
-- | `Gen.runGen` → `tickChord` → `stepEmit` pipeline the live component runs —
-- | over thousands of steps and reports statistics, so we can see what the
-- | randomisers and the chord quantiser actually produce. No DOM, no MIDI.
-- |
-- | Run with:  spago run --main Triggerfish.Harness
module Triggerfish.Harness (main) where

import Prelude

import Data.Array (elem, filter, length)
import Data.Foldable (foldl)
import Data.Int (round, toNumber)
import Effect (Effect)
import Effect.Console (log)
import Triggerfish.Odonus.Model as M
import Triggerfish.Odonus.Marbles as Marbles
import Triggerfish.Odonus.Gen as Gen
import Triggerfish.Odonus.Grid.Types (GenKind(..), GenSource, genDefaultAmt, genDefaultRate, genKinds)

type Sim = { odo :: M.Odonus, seed :: Marbles.Seed }

type Stats =
  { steps :: Int
  , fired :: Int
  , chordHits :: Int      -- fired notes whose pitch-class is in the live chord
  , chordSeen :: Int      -- fired notes checked (only while chord overlay is on)
  , skipSum :: Int        -- Σ skipped-cell count per step
  , skipMax :: Int
  , gateOffSum :: Int     -- Σ rested-cell count per step
  , minActive :: Int      -- fewest unmuted voices seen (HEADS must never hit 0)
  , pitchLo :: Int
  , pitchHi :: Int
  }

emptyStats :: Stats
emptyStats =
  { steps: 0, fired: 0, chordHits: 0, chordSeen: 0, skipSum: 0, skipMax: 0
  , gateOffSum: 0, minActive: 99, pitchLo: 999, pitchHi: -999 }

-- | One model step through the real pipeline, returning the post-step world,
-- | the world the heads READ (o1), and what fired.
advance
  :: Array GenSource -> Number -> Number -> Sim
  -> { next :: Sim, o1 :: M.Odonus, fired :: Array M.Fired }
advance gen spread bias st =
  let g = Gen.runGen { gen, spread, bias, odo: st.odo, seed: st.seed }
      o1 = if g.odo.chord.on then M.tickChord g.odo else g.odo
      r = M.stepEmit o1
  in { next: { odo: r.odo, seed: g.seed }, o1, fired: r.fired }

update :: Stats -> M.Odonus -> Array M.Fired -> Stats
update s o1 fired =
  let skipped = length (filter _.skip o1.cells)
      gatesOff = length (filter (not <<< _.gate) o1.cells)
      active = length (filter (not <<< _.mute) o1.heads)
      pcs = M.currentChordPCs o1
      hits = if o1.chord.on then length (filter (\f -> elem (mod f.pitch 12) pcs) fired) else 0
      seen = if o1.chord.on then length fired else 0
      pitches = map _.pitch fired
  in s
       { steps = s.steps + 1
       , fired = s.fired + length fired
       , chordHits = s.chordHits + hits
       , chordSeen = s.chordSeen + seen
       , skipSum = s.skipSum + skipped
       , skipMax = max s.skipMax skipped
       , gateOffSum = s.gateOffSum + gatesOff
       , minActive = min s.minActive active
       , pitchLo = foldl min s.pitchLo pitches
       , pitchHi = foldl max s.pitchHi pitches
       }

simulate :: Int -> Array GenSource -> Number -> Number -> M.Odonus -> Marbles.Seed -> Stats
simulate count gen spread bias odo0 seed0 = go count { odo: odo0, seed: seed0 } emptyStats
  where
  go k st stats
    | k <= 0 = stats
    | otherwise =
        let a = advance gen spread bias st
        in go (k - 1) a.next (update stats a.o1 a.fired)

-- | All sources off, at their default rate/amt.
baseGen :: Array GenSource
baseGen = map (\k -> { kind: k, on: false, rate: genDefaultRate k, amt: genDefaultAmt k }) genKinds

-- | Turn one source on with explicit rate index + depth.
oneSource :: GenKind -> Int -> Int -> Array GenSource
oneSource k rate amt =
  map (\src -> if src.kind == k then src { on = true, rate = rate, amt = amt } else src) baseGen

-- | Every source on at its defaults — the kitchen sink.
allOn :: Array GenSource
allOn = map (_ { on = true }) baseGen

-- | A default patch with all four voices sounding.
allVoices :: M.Odonus
allVoices = M.setHeadMask 15 M.defaultOdonus

pct :: Int -> Int -> String
pct a b = if b == 0 then "n/a" else show (round (100.0 * toNumber a / toNumber b)) <> "%"

mean1 :: Int -> Int -> String
mean1 a b = if b == 0 then "0" else
  let t = round (10.0 * toNumber a / toNumber b) in show (t / 10) <> "." <> show (mod t 10)

n :: Int
n = 1500

main :: Effect Unit
main = do
  log "════ Triggerfish sampling harness ════"
  log ("steps per scenario: " <> show n <> "\n")

  -- A. Chord quantiser: NOTES churning every step, chord overlay on. EVERY
  --    emitted note must be a tone of the current chord (across octaves).
  let a = simulate n (oneSource GNotes 0 100) 0.5 0.5 (M.toggleChord allVoices) (Marbles.seedFrom 11)
  log "A. CHORD ADHERENCE  (NOTES max, chord on)"
  log ("   fired notes:        " <> show a.fired)
  log ("   on-chord:           " <> pct a.chordHits a.chordSeen <> "   (want 100%)")
  log ("   pitch range:        " <> show a.pitchLo <> "–" <> show a.pitchHi <> "\n")

  -- B. SKIP sparse-bias: turning SKIP on alone should hover around a couple of
  --    skips, NOT drift to ~8 (half the grid).
  let b1 = simulate n (oneSource GSkip 0 30) 0.5 0.5 allVoices (Marbles.seedFrom 22)
  let b2 = simulate n (oneSource GSkip 0 100) 0.5 0.5 allVoices (Marbles.seedFrom 23)
  log "B. SKIP DENSITY  (of 16 cells)"
  log ("   depth 30%:  mean " <> mean1 b1.skipSum b1.steps <> "  max " <> show b1.skipMax)
  log ("   depth 100%: mean " <> mean1 b2.skipSum b2.steps <> "  max " <> show b2.skipMax <> "\n")

  -- C. GATE rests: same sparse bias, biased toward mostly-open.
  let c = simulate n (oneSource GGate 0 30) 0.5 0.5 allVoices (Marbles.seedFrom 33)
  log "C. GATE RESTS  (of 16 cells)"
  log ("   depth 30%:  mean " <> mean1 c.gateOffSum c.steps <> " rests\n")

  -- D. HEADS walk must never silence everything.
  let d = simulate n (oneSource GHeads 0 100) 0.5 0.5 allVoices (Marbles.seedFrom 44)
  log "D. HEADS WALK"
  log ("   min active voices:  " <> show d.minActive <> "   (must be ≥ 1)\n")

  -- E. Kitchen sink: everything on at defaults, chord on.
  let e = simulate n allOn 0.5 0.5 (M.toggleChord allVoices) (Marbles.seedFrom 55)
  log "E. KITCHEN SINK  (all sources on, chord on)"
  log ("   fired notes:        " <> show e.fired)
  log ("   on-chord:           " <> pct e.chordHits e.chordSeen)
  log ("   pitch range:        " <> show e.pitchLo <> "–" <> show e.pitchHi)
  log ("   skip mean/max:      " <> mean1 e.skipSum e.steps <> " / " <> show e.skipMax)
  log ("   min active voices:  " <> show e.minActive)
  log "\n════ done ════"
