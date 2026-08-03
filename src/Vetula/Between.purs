-- | `Vetula.Between` — the betweening engine: the passing chords that BRIDGE two
-- | chords of a progression. This is the "grow, reformulated to N steps to reach
-- | different cadences" idea (AC, 2026-07-30), sourced from **Harmonia's
-- | functional chord model** rather than from either lattice.
-- |
-- | v1 is a *tonicizing turnaround*: to reach a target chord with `n` bridge
-- | chords, we treat the target's root as a temporary I and lay the last `n` of a
-- | circle-of-fifths approach in front of it, realized in that temporary key:
-- |
-- |   n = 1 : V7            — a dominant push
-- |   n = 2 : ii–V          — the full authentic cadence
-- |   n = 3 : vi–ii–V       — a longer turnaround
-- |   n = 4 : iii–vi–ii–V   — the whole circle
-- |
-- | So the "cadence length" dial and the harmonic result are the same knob. This
-- | is deliberately the simplest *real* engine (major ii–V uniformly, no minor-
-- | target ii°, no voice-leading optimisation) — it lives behind `bridgeNotes` so
-- | a richer engine (functional substitution, minor cadences, VL-smoothed voicing)
-- | can replace it without touching the arrange workflow that calls it.
-- |
-- | Pure — no Halogen, no FFI. Returns absolute-MIDI note lists ready for
-- | `importChord`; the register is a placeholder middle voicing, since control C
-- | (Voices) owns the real voicing.
module Vetula.Between
  ( bridgeNotes
  , maxBridge
  ) where

import Prelude

import Data.Array (drop, length, sort)
import Harmonia.Chord (Chord(..), DegreeChord, Mode(..), Numeral(..), Quality(..), deg, realize)

-- | The deepest bridge the ladder offers (the dial's ceiling).
maxBridge :: Int
maxBridge = 4

-- | The circle-of-fifths approach into a target, dominant LAST. `bridgeNotes`
-- | takes the final `n` of these so a bigger `n` reaches further back.
ladder :: Array DegreeChord
ladder =
  [ deg III Min7 []
  , deg VI  Min7 []
  , deg II  Min7 []
  , deg V   Dom7 []
  ]

-- | The `n` bridge chords leading INTO `targetRootPc` (a pitch class 0..11), as
-- | absolute-MIDI note lists in a middle register. `n <= 0` → no bridge.
bridgeNotes :: Int -> Int -> Array (Array Int)
bridgeNotes n targetRootPc
  | n <= 0 = []
  | otherwise =
      let tempKey = { tonic: targetRootPc, mode: Ionian }
          chosen = drop (max 0 (length ladder - n)) ladder
      in map (\dc -> case realize tempKey dc of Chord pcs -> voiceMid pcs) chosen

-- | Drop each pitch class into the octave above C3 — a plain, compact reading
-- | good enough to audition. Voicing proper is the Voices control's job.
voiceMid :: Array Int -> Array Int
voiceMid pcs = sort (map (\pc -> 48 + pc) pcs)
