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
  , noteOf
  , setNote
  , instName
  , ratchetAt
  , setRatchetAt
  , clearRatchets
  , pushOf
  , setPush
  , dillaPush
  , flatPush
  , openOf
  , setOpen
  , opensAt
  , Snapshot
  , snapshotCount
  , captureSnapshot
  , applySnapshot
  , clampI
  , TrigBank
  , TrigSlot
  , defaultTrig
  , setJackSource
  , setJackName
  , setJackNote
  , setRoute
  , addRoute
  , removeRoute
  ) where

import Prelude

import Data.Array (deleteAt, length, modifyAt, range, replicate, snoc, updateAt, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Reef.Balistes.Engine (Trigger, freshPerturbations, readDrumMap, clampDensity)
import Reef.Balistes.Sim as Sim

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
  -- per-lane MIDI note (BD/SD/HH/OH), editable so the same Grids pattern can
  -- drive a different kick/snare/etc — another axis of saved variation.
  , notes :: Array Int
  -- `open` (0..255) — the OPEN-hat dial. It sets a boundary that descends the
  -- HH level landscape: a firing hat whose level clears the boundary fires
  -- OPEN (and chokes its closed self) instead of closed. At 0 nothing opens;
  -- turning it up recruits the loudest/most-stressed hats first.
  , open :: Int
  }

-- | A captured point in control space: the X/Y cursor + the three densities +
-- | randomness + open + the per-voice push. NOT the pattern position
-- | (step/perts/rng) or the ratchet overlay — a snapshot is the knob+pad state,
-- | recallable instantly. Agnostic to where it came from (a live gesture or, in
-- | future, an imported pattern), per the rethink doc.
type Snapshot =
  { x :: Int
  , y :: Int
  , densBd :: Int
  , densSd :: Int
  , densHh :: Int
  , randomness :: Int
  , open :: Int
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
    , notes: [ 36, 38, 42, 46 ]
    -- a touch of open by default, so the loudest hats breathe
    , open: 70
    }

-- | A nonzero seed (xorshift fixed-points at 0).
initialSeed :: Int
initialSeed = 0x1A2B3C4D

-- | Play the current step, then advance. Returns the triggers fired *this*
-- | step (so the caller emits them with the tick's fire-time) and the advanced
-- | state. When the step wraps back to 0 a fresh set of perturbations is
-- | sampled for the new pattern — the firmware's once-per-pattern-start rule.
tick :: Balistes -> { bal :: Balistes, fired :: Array Trigger }
tick = Sim.stepBal

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
instNote = Sim.instNote

-- | This pattern's MIDI note for a lane (0=BD,1=SD,2=HH,3=OH), falling back to
-- | the GM default. Editable — the same Grids beat, a different kick. Delegated
-- | to the shared sim so the frontend and the rig map notes identically.
noteOf :: Inst -> Balistes -> Int
noteOf = Sim.noteOf

-- | Set a lane's MIDI note (clamped to 0..127).
setNote :: Inst -> Int -> Balistes -> Balistes
setNote inst n b = b { notes = fromMaybe b.notes (updateAt inst (clampI 0 127 n) b.notes) }

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
clampI = Sim.clampI

-- | The ratchet count for a grid slot (inst, step), >= 1. 1 = a single hit.
-- | Delegated to the shared sim (note the arg order flips: Sim takes inst/step/b).
ratchetAt :: Balistes -> Inst -> Int -> Int
ratchetAt b inst step = Sim.ratchetAt inst step b

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
pushOf = Sim.pushOf

setPush :: Inst -> Int -> Balistes -> Balistes
setPush inst v b = b { push = fromMaybe b.push (updateAt inst (clampI (-50) 50 v) b.push) }

-- | The classic Dilla feel: snare a touch late, hats (closed + open) a touch
-- | early, kick on the grid. A one-tap groove preset.
dillaPush :: Balistes -> Balistes
dillaPush b = b { push = [ 0, 16, -9, -9 ] }

-- | Zero every lane's timing offset.
flatPush :: Balistes -> Balistes
flatPush b = b { push = [ 0, 0, 0, 0 ] }

-- ---------------------------------------------------------------------------
-- Open hat (the OPEN dial) — a descending boundary on the HH landscape
-- ---------------------------------------------------------------------------

openOf :: Balistes -> Int
openOf b = b.open

setOpen :: Int -> Balistes -> Balistes
setOpen v b = b { open = clampI 0 255 v }

-- | Does the HH voice fire OPEN at this step? The open boundary is `255 - open`,
-- | so it descends from above the landscape (nothing opens at 0) down through
-- | the accent line and toward the fire threshold as the dial rises — the
-- | loudest, most-stressed hats convert to open first. Caller fires a hat here;
-- | open means: ring longer, and choke the closed hit. Uses the deterministic
-- | level (the same landscape the heatmap paints), so visual + audio agree.
opensAt :: Balistes -> Int -> Boolean
opensAt b step = Sim.opensAt step b

-- ---------------------------------------------------------------------------
-- Snapshots — captured control points (the snapshot bank)
-- ---------------------------------------------------------------------------

snapshotCount :: Int
snapshotCount = 8

-- | Extract the current control point.
captureSnapshot :: Balistes -> Snapshot
captureSnapshot b =
  { x: b.x, y: b.y, densBd: b.densBd, densSd: b.densSd, densHh: b.densHh
  , randomness: b.randomness, open: b.open, push: b.push }

-- | Set the control fields from a snapshot (instant jump). Pattern position and
-- | the ratchet overlay are left as they are.
applySnapshot :: Snapshot -> Balistes -> Balistes
applySnapshot s b =
  b { x = s.x, y = s.y, densBd = s.densBd, densSd = s.densSd, densHh = s.densHh
    , randomness = s.randomness, open = s.open, push = s.push }

-- ---------------------------------------------------------------------------
-- POLYTRIG — the third Balistes drum-brain (relocated from Selene, browser-only)
-- ---------------------------------------------------------------------------

-- | A POLYTRIG jack: one named output. `name` is the atom a route addresses
-- | (`bd`); `note` is the MIDI note it fires; `source` is an optional per-jack
-- | mini-notation pattern whose onsets are gate times over one cycle. A jack
-- | fires from its own `source` stacked with any route onsets addressed to its
-- | `name`. Copied verbatim from Selene's `TrigSlot` (same field names, so the
-- | shared `Tidal.Lane` engine drives it), minus the CV/gate target apparatus —
-- | on Balistes every jack lands on the drums MIDI channel.
type TrigSlot =
  { name :: String
  , note :: Int
  , source :: String
  }

-- | A POLYTRIG bank: named output **jacks** plus lane-spanning **routes**. A
-- | route is a mini-notation string whose atoms fire jacks by name
-- | (`"bd sn cp sn"`); both stack at playback.
type TrigBank =
  { jacks :: Array TrigSlot
  , routes :: Array String
  }

-- | Eight named jacks — two carry their own ostinato (hh, oh), the rest are
-- | driven by the lane-spanning route "bd sn cp sn", so the default reads as
-- | real Tidal: named voices + a spanning pattern. Notes are the GM-ish drum
-- | ladder. Same seed as Selene's default trig block.
defaultTrig :: TrigBank
defaultTrig =
  { jacks: map jack (range 0 7)
  , routes: [ "bd sn cp sn" ]
  }
  where
  jack i =
    { name: fromMaybe "j" (names !! i)
    , note: fromMaybe (36 + i) (notes !! i)
    , source: fromMaybe "" (pats !! i)
    }
  names = [ "bd", "sn", "cp", "hh", "oh", "rs", "lt", "ht" ]
  notes = [ 36, 38, 39, 42, 46, 37, 45, 50 ]
  pats = [ "", "", "", "x*8", "~ ~ x ~", "", "", "" ]

-- | Edit a jack's per-jack source pattern.
setJackSource :: Int -> String -> TrigBank -> TrigBank
setJackSource i src tb =
  tb { jacks = fromMaybe tb.jacks (modifyAt i (_ { source = src }) tb.jacks) }

-- | Rename a jack (the atom a route addresses).
setJackName :: Int -> String -> TrigBank -> TrigBank
setJackName i nm tb =
  tb { jacks = fromMaybe tb.jacks (modifyAt i (_ { name = nm }) tb.jacks) }

-- | Set a jack's MIDI note (clamped 0..127).
setJackNote :: Int -> Int -> TrigBank -> TrigBank
setJackNote i n tb =
  tb { jacks = fromMaybe tb.jacks (modifyAt i (_ { note = clampI 0 127 n }) tb.jacks) }

-- | Edit a route line.
setRoute :: Int -> String -> TrigBank -> TrigBank
setRoute i src tb =
  tb { routes = fromMaybe tb.routes (updateAt i src tb.routes) }

-- | Append a fresh (empty) route line.
addRoute :: TrigBank -> TrigBank
addRoute tb = tb { routes = snoc tb.routes "" }

-- | Drop a route line.
removeRoute :: Int -> TrigBank -> TrigBank
removeRoute i tb = tb { routes = fromMaybe tb.routes (deleteAt i tb.routes) }

