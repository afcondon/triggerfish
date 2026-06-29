-- | Triggerfish.Balistes.Model — the virtual Grids brain: the ~16 bytes of
-- | Grids state (X, Y, three densities, randomness, step, the per-pattern
-- | perturbations + RNG) plus the stepping that drives playback.
-- |
-- | The hardware has one navigator: a clock gate. This model keeps that
-- | faithful core (so it agrees with the BEAM `balistes_voice`) and is the
-- | surface we'll grow past the hardware from — X/Y as a navigable landscape,
-- | per-instrument accent logic, eventually pattern-space motion the module
-- | could never offer.
module Triggerfish.Balistes.Model
  ( Balistes
  , defaultBalistes
  , tick
  , reset
  , reseed
  , setX
  , setY
  , setDensity
  , setRandomness
  , levelAt
  , wouldFire
  , densityOf
  , Inst
  , instNote
  , instName
  , ratchetAt
  , setRatchetAt
  , clearRatchets
  , pushOf
  , setPush
  , dillaPush
  , flatPush
  , clampI
  ) where

import Prelude

import Data.Array (replicate, updateAt, (!!))
import Data.Maybe (fromMaybe)
import Triggerfish.Balistes.Engine (Trigger, evaluateStep, freshPerturbations, readDrumMap, clampDensity)

-- | The whole module state — the faithful firmware core (X, Y, densities,
-- | randomness, step, perts, rng), what agrees byte-for-byte with the BEAM
-- | `balistes_voice` driving the three Grids lanes (BD/SD/HH), plus a thin
-- | Triggerfish overlay the engine ignores and the component applies on emit:
-- |   • `ratchet` — a 96-slot mask (lane*32+step) over the Grids lanes, >=1 =
-- |     subdivide that beat into N retriggers when it fires (drag a cell);
-- |   • `push` — four signed-ms timing offsets, one per voice (BD/SD/HH/OH): the
-- |     J Dilla "drag and push" feel (snare late, hats early). The 4th slot is
-- |     the open hat, which now emerges from the HH stream via the OPEN dial.
type Balistes =
  { x :: Int
  , y :: Int
  , densBd :: Int
  , densSd :: Int
  , densHh :: Int
  , randomness :: Int
  , step :: Int
  , perts :: Array Int
  , rng :: Int
  -- authoring overlay (Triggerfish extension, not in the firmware)
  , ratchet :: Array Int
  , push :: Array Int
  }

-- | Central node, moderate density, no randomness — the firmware's neutral
-- | starting point. Perturbations pre-sampled for the pattern starting at 0;
-- | overlay starts inert (no ratchets, no timing push).
defaultBalistes :: Balistes
defaultBalistes =
  let seeded = (freshPerturbations 0 initialSeed)
  in
    { x: 128
    , y: 128
    , densBd: 192
    , densSd: 150
    , densHh: 170
    , randomness: 0
    , step: 0
    , perts: seeded.perts
    , rng: seeded.rng
    -- one ratchet slot per (lane, step) over the 3 Grids lanes (96 = 3×32)
    , ratchet: replicate 96 1
    , push: [ 0, 0, 0, 0 ]
    }

-- | A nonzero seed (xorshift fixed-points at 0).
initialSeed :: Int
initialSeed = 0x1A2B3C4D

-- | Play the current step, then advance. Returns the triggers fired *this*
-- | step (so the caller emits them with the tick's fire-time) and the advanced
-- | state. When the step wraps back to 0 a fresh set of perturbations is
-- | sampled for the new pattern — the firmware's once-per-pattern-start rule.
tick :: Balistes -> { bal :: Balistes, fired :: Array Trigger }
tick b =
  let
    fired = evaluateStep b.step b.x b.y [ b.densBd, b.densSd, b.densHh ] b.perts
    nextStep = (b.step + 1) `mod` 32
    b' =
      if nextStep == 0 then
        let s = freshPerturbations b.randomness b.rng
        in b { step = nextStep, perts = s.perts, rng = s.rng }
      else
        b { step = nextStep }
  in
    { bal: b', fired }

-- | Jump to step 0 and resample — the panel RESET / restart.
reset :: Balistes -> Balistes
reset b =
  let s = freshPerturbations b.randomness b.rng
  in b { step = 0, perts = s.perts, rng = s.rng }

-- | Re-roll the perturbation seed for a different "take" at the same settings —
-- | the DICE. Advances the RNG by a chaotic kick then resamples.
reseed :: Balistes -> Balistes
reseed b =
  let kicked = b.rng + 0x6D2B79F5
      s = freshPerturbations b.randomness kicked
  in b { perts = s.perts, rng = s.rng }

clamp255 :: Int -> Int
clamp255 = clampDensity

setX :: Int -> Balistes -> Balistes
setX v b = b { x = clamp255 v }

setY :: Int -> Balistes -> Balistes
setY v b = b { y = clamp255 v }

setRandomness :: Int -> Balistes -> Balistes
setRandomness v b = b { randomness = clamp255 v }

-- | Instruments are indexed 0=BD, 1=SD, 2=HH everywhere (the firmware order).
type Inst = Int

setDensity :: Inst -> Int -> Balistes -> Balistes
setDensity inst v b = case inst of
  0 -> b { densBd = clamp255 v }
  1 -> b { densSd = clamp255 v }
  _ -> b { densHh = clamp255 v }

densityOf :: Inst -> Balistes -> Int
densityOf inst b = case inst of
  0 -> b.densBd
  1 -> b.densSd
  _ -> b.densHh

-- | The deterministic interpolated level (0..255) at (step, inst) for the
-- | current X/Y — the bilinear landscape, ignoring the random perturbation.
-- | This is what the step heatmap paints: the pattern X/Y selects, before the
-- | dice of randomness nudge it.
levelAt :: Balistes -> Inst -> Int -> Int
levelAt b inst step = readDrumMap step inst b.x b.y

-- | Would this (inst, step) trigger on the deterministic level alone, at the
-- | current density? (The heatmap marks these; live randomness can add more.)
wouldFire :: Balistes -> Inst -> Int -> Boolean
wouldFire b inst step = levelAt b inst step > (255 - clampDensity (densityOf inst b))

-- | Default GM-ish drum notes (the Balistes binding defaults): BD=36, SD=38,
-- | HH=42. One MIDI channel; accent raises velocity.
instNote :: Inst -> Int
instNote inst = case inst of
  0 -> 36
  1 -> 38
  _ -> 42

-- | The three Grids lanes are 0=BD, 1=SD, 2=HH.
instName :: Inst -> String
instName inst = case inst of
  0 -> "BD"
  1 -> "SD"
  _ -> "HH"

-- ---------------------------------------------------------------------------
-- Authoring overlay (Triggerfish extension)
-- ---------------------------------------------------------------------------

clampI :: Int -> Int -> Int -> Int
clampI lo hi v = if v < lo then lo else if v > hi then hi else v

-- | The ratchet count for a grid slot (inst, step), >= 1. 1 = a single hit.
ratchetAt :: Balistes -> Inst -> Int -> Int
ratchetAt b inst step = fromMaybe 1 (b.ratchet !! (inst * 32 + step))

-- | Set the ratchet count for a slot (clamped 1..8). Drag a heatmap cell.
setRatchetAt :: Inst -> Int -> Int -> Balistes -> Balistes
setRatchetAt inst step v b =
  b { ratchet = fromMaybe b.ratchet (updateAt (inst * 32 + step) (clampI 1 8 v) b.ratchet) }

-- | Wipe the Grids lanes' ratchets (the whole 96-slot overlay) back to single
-- | hits. Called when the X/Y cursor moves: Grids ratchets decorate the
-- | generative pattern you were on, so they don't follow you to a new one.
clearRatchets :: Balistes -> Balistes
clearRatchets b = b { ratchet = map (const 1) b.ratchet }

-- | Per-lane timing offset in ms (signed: − earlier, + later).
pushOf :: Inst -> Balistes -> Int
pushOf inst b = fromMaybe 0 (b.push !! inst)

setPush :: Inst -> Int -> Balistes -> Balistes
setPush inst v b = b { push = fromMaybe b.push (updateAt inst (clampI (-50) 50 v) b.push) }

-- | The classic Dilla feel: snare a touch late, hats (closed + open) a touch
-- | early, kick on the grid. A one-tap groove preset.
dillaPush :: Balistes -> Balistes
dillaPush b = b { push = [ 0, 16, -9, -9 ] }

-- | Zero every lane's timing offset.
flatPush :: Balistes -> Balistes
flatPush b = b { push = [ 0, 0, 0, 0 ] }

