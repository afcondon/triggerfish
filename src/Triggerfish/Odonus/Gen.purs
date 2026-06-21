-- | The randomisation engine: a matrix of independent slow-drift SOURCES, each
-- | with its own firing rate (a bare period — "one change per N steps"). On
-- | every model step each enabled source draws from the shared PRNG and, with
-- | probability 1/period, mutates ONE random element of its domain by a single
-- | notch. Keeping each change tiny and rare lets several sources run at once
-- | without descending into chaos — the texture evolves, it doesn't scramble.
-- |
-- | The NOTES source draws pitches from the Marbles Beta distribution (so the
-- | X-Y pad still shapes where new notes land); the others walk discrete
-- | parameters. This is pure: same state + same seed → same mutation.
module Triggerfish.Odonus.Gen
  ( GenInput
  , runGen
  , rollAllNotes
  , rollChords
  ) where

import Prelude

import Data.Array (range, (!!))
import Data.Foldable (foldl)
import Data.Int (round, toNumber)
import Data.Int.Bits (shl, xor)
import Data.Maybe (maybe)
import Triggerfish.Odonus.Model as M
import Triggerfish.Odonus.Marbles as Marbles
import Triggerfish.Odonus.Grid.Types (GenKind(..), GenSource, periodOf)

type GenInput =
  { gen :: Array GenSource
  , spread :: Number
  , bias :: Number
  , odo :: M.Odonus
  , seed :: Marbles.Seed
  }

-- | Run every enabled source once over this step, threading the seed. Each
-- | source fires with probability 1/period; a firing applies one notch.
runGen :: GenInput -> { odo :: M.Odonus, seed :: Marbles.Seed }
runGen inp = foldl stepSrc { odo: inp.odo, seed: inp.seed } inp.gen
  where
  stepSrc acc src =
    if not src.on then acc
    else
      let { u, seed: s1 } = Marbles.nextRand acc.seed
      in if u < 1.0 / toNumber (periodOf src.rate)
         then applyGen src.kind inp.spread inp.bias src.amt acc.odo s1
         else acc { seed = s1 }

-- | One firing of a source. `amt` (0..100) is the DEPTH: how big the change is.
-- | For note rolls it's how many cells reroll; for the booleans it's the sparse
-- | density; for the nudges it's the step magnitude; for KEY it's the chance of
-- | a full modal change vs a gentle fifth. Note candidates stay chromatic
-- | (range 36..84) — the quantizer reins them into the scale.
applyGen
  :: GenKind -> Number -> Number -> Int -> M.Odonus -> Marbles.Seed
  -> { odo :: M.Odonus, seed :: Marbles.Seed }
applyGen kind spread bias amt odo seed =
  let amt01 = toNumber amt / 100.0
  in case kind of
    GNotes -> rerollNotes (1 + round (amt01 * 5.0)) bias spread odo seed
    -- A symmetric toggle drifts to ~50% on; these stay sparse instead — landing
    -- on the "rare" state always restores it, the common state only flips with a
    -- low probability (= depth). So gates stay mostly open, skips/glides occasional.
    GGate -> stepBias (amt01 * 0.5) (\c -> not c.gate) M.toggleGate odo seed
    GSkip -> stepBias (amt01 * 0.5) _.skip M.toggleSkip odo seed
    GGlide -> stepBias (amt01 * 0.5) _.glide M.toggleGlide odo seed
    GLen ->
      let mag = 1 + round (amt01 * 3.0)   -- ±1..±4
          { n: i, seed: s1 } = Marbles.nextInt 16 seed
          { n: d, seed: s2 } = Marbles.nextInt 2 s1
          cur = maybe 1 _.dur (odo.cells !! i)
      in { odo: M.setCellDur i (cur + (if d == 0 then -mag else mag)) odo, seed: s2 }
    GHeads -> flipHeads (1 + round (amt01 * 2.0)) odo seed   -- 1..3 bits/fire
    GTransp ->
      let mag = 1 + round (amt01 * 11.0)   -- ±1..±12 semitones (deep enough to re-voice)
          { n: h, seed: s1 } = Marbles.nextInt 4 seed
          { n: d, seed: s2 } = Marbles.nextInt 2 s1
          cur = maybe 0 _.transp (odo.heads !! h)
      in { odo: M.setHeadTransp h (cur + (if d == 0 then -mag else mag)) odo, seed: s2 }
    GPattern ->
      let { n: h, seed: s1 } = Marbles.nextInt 4 seed
      in { odo: M.cyclePattern h odo, seed: s1 }
    GSpeed ->
      let mag = 1 + round (amt01 * 3.0)   -- ±1..±4 speed steps
          { n: h, seed: s1 } = Marbles.nextInt 4 seed
          { n: d, seed: s2 } = Marbles.nextInt 2 s1
          cur = maybe 4 _.speedIx (odo.heads !! h)
      in { odo: M.setHeadSpeedIx h (cur + (if d == 0 then -mag else mag)) odo, seed: s2 }
    GKey ->
      -- Depth = adventurousness. Mostly nudge the tonal centre by a fifth; with
      -- probability `amt01` instead jump to a whole new reasonable scale (never
      -- a random note cluster — see Model.setRandScale / Scale.randomisableScales).
      let { u, seed: s1 } = Marbles.nextRand seed
      in if u < amt01
         then let { n: ix, seed: s2 } = Marbles.nextInt M.numRandScales s1
              in { odo: M.setRandScale ix odo, seed: s2 }
         else let { n: dir, seed: s2 } = Marbles.nextInt 2 s1
              in { odo: M.setRoot (odo.rootPc + (if dir == 0 then 7 else 5)) odo, seed: s2 }

-- | Reroll `n` random cells from the Beta distribution, threading the seed.
rerollNotes
  :: Int -> Number -> Number -> M.Odonus -> Marbles.Seed
  -> { odo :: M.Odonus, seed :: Marbles.Seed }
rerollNotes n bias spread odo seed
  | n <= 0 = { odo, seed }
  | otherwise =
      let { n: i, seed: s1 } = Marbles.nextInt 16 seed
          r = Marbles.rollValue { bias, spread } (range 36 84) s1
      in rerollNotes (n - 1) bias spread (M.setNote i r.value odo) r.seed

-- | Flip `n` random head bits, never landing on all-voices-off: an empty mask
-- | hands the lone voice to a neighbour instead.
flipHeads :: Int -> M.Odonus -> Marbles.Seed -> { odo :: M.Odonus, seed :: Marbles.Seed }
flipHeads n odo seed
  | n <= 0 = { odo, seed }
  | otherwise =
      let { n: h, seed: s1 } = Marbles.nextInt 4 seed
          raw = xor (M.headMask odo) (shl 1 h)
          mask = if raw == 0 then shl 1 ((h + 1) `mod` 4) else raw
      in flipHeads (n - 1) (M.setHeadMask mask odo) s1

-- | A sparse on/off mutation. `isRare` marks the state we want to occur seldom
-- | (a rest for GATE, a skip, a glide). Landing on a cell already in the rare
-- | state always restores it; a cell in the common state enters the rare state
-- | only with probability `pEnter` (the source's depth). Equilibrium ≈
-- | pEnter/(1+pEnter) of 16 cells — occasional, not half-and-half.
stepBias
  :: Number -> (M.Cell -> Boolean) -> (Int -> M.Odonus -> M.Odonus)
  -> M.Odonus -> Marbles.Seed -> { odo :: M.Odonus, seed :: Marbles.Seed }
stepBias pEnter isRare toggle odo seed =
  let { n: i, seed: s1 } = Marbles.nextInt 16 seed
      rare = maybe false isRare (odo.cells !! i)
  in if rare then { odo: toggle i odo, seed: s1 }
     else let { u, seed: s2 } = Marbles.nextRand s1
          in if u < pEnter then { odo: toggle i odo, seed: s2 } else { odo, seed: s2 }

-- | One-shot: reroll EVERY cell note from the current Beta distribution (the
-- | "Roll once" button). amount = 1.0 ⇒ all sixteen regenerate.
rollAllNotes
  :: Number -> Number -> M.Odonus -> Marbles.Seed
  -> { odo :: M.Odonus, seed :: Marbles.Seed }
rollAllNotes spread bias odo seed =
  let m = Marbles.mutateInts { spread, bias, amount: 1.0 } (range 36 84) (map _.note odo.cells) seed
  in { odo: M.setNotes m.values odo, seed: m.seed }

-- | Draw four random chord indices from a table of `tableSize` — a fresh
-- | four-chord progression for the KEY·CHORDS quantiser.
rollChords :: Int -> Marbles.Seed -> { picks :: Array Int, seed :: Marbles.Seed }
rollChords tableSize = go 4 []
  where
  go n acc seed
    | n <= 0 = { picks: acc, seed }
    | otherwise =
        let { n: ix, seed: seed' } = Marbles.nextInt tableSize seed
        in go (n - 1) (acc <> [ ix ]) seed'
