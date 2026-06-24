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
  , PatternLane
  , firstPadLane
  , padCount
  , padNote
  , padName
  , padColor
  , padSource
  , padClicks
  , setPadSource
  , setPadClicks
  , setPadName
  , clickAt
  , setRoute
  , setRoutes
  , ohPadIndex
  , clampI
  ) where

import Prelude

import Data.Array (length, replicate, updateAt, (!!))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Triggerfish.Balistes.Engine (Trigger, evaluateStep, freshPerturbations, readDrumMap, clampDensity)

-- | The whole module state. The top block is the faithful firmware core (X, Y,
-- | densities, randomness, step, perts, rng) — what agrees byte-for-byte with
-- | the BEAM `balistes_voice`, driving the three Grids lanes (BD/SD/HH). The
-- | bottom block is the Triggerfish overlay that grows past the hardware:
-- |   • `ratchet` — a 96-slot mask (lane*32+step) over the Grids lanes, >=1 =
-- |     subdivide that beat into N retriggers when it fires (drag a cell);
-- |   • `push` — four signed-ms timing offsets, one per lane (BD/SD/HH/OH): the
-- |     J Dilla "drag and push" feel (snare late, hats early);
-- |   • `patternLanes` — the explicit drum-machine lanes *below* the Grids
-- |     device: each a name + MIDI note + colour + 32-step on/off pattern, NOT
-- |     Grids-generated. Lane 3 is OH (open hat); the rest are the other pads
-- |     (clap, toms, cymbals, perc) — 16 pads total. These are the lanes that
-- |     will eventually be authored in Tidal; today you click them in.
-- | The Grids engine ignores the overlay; the component applies it on emit.
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
  , patternLanes :: Array PatternLane
  -- routing patterns: `s`-style multi-voice mini-notation whose atoms route to
  -- the Tidal kit lanes (and OH) by label. A small stack; merged at playback.
  , routes :: Array String
  }

-- | One explicit pad lane below the Grids device: a named MIDI voice that is a
-- | *pattern*, not a fixed grid. Two layers, merged (`stack`) at playback:
-- |   • `source` — a Tidal mini-notation string (`"bd*3 ~ bd(3,8)"`); the
-- |     vendored engine parses + queries it, and the lane's visible subdivision
-- |     (its "meter") is *derived* from this pattern — polymeter across lanes.
-- |   • `clicks` — a hand-toggled overlay on that derived meter (so empty/odd
-- |     patterns still give you cells to click). Length tracks the meter.
-- | Onsets fire at their true fractional time in the cycle — nothing snaps to
-- | the Grids 32-grid. See `Triggerfish.Balistes.Tidal` for meter/onset logic.
type PatternLane =
  { name :: String
  , note :: Int
  , color :: String
  , source :: String
  , clicks :: Array Boolean
  }

-- | Central node, moderate density, no randomness — the firmware's neutral
-- | starting point. Perturbations pre-sampled for the pattern starting at 0;
-- | overlay starts inert (no ratchets, no timing push, empty OH lane).
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
    -- one ratchet slot per (lane, step) over the 3 Grids lanes only (96 = 3×32);
    -- pad lanes subdivide through their pattern (`bd*2`), not the ratchet overlay
    , ratchet: replicate 96 1
    , push: [ 0, 0, 0, 0 ]
    , patternLanes: defaultPatternLanes
    , routes: [ "", "", "" ]
    }

-- | The pad-lane index of OH — the open hat, the one lane that bridges the
-- | Grids device and the Tidal kit. Fixed label, sits with the Grids group,
-- | reachable by routing (`oh`) and by click.
ohPadIndex :: Int
ohPadIndex = 0

-- | The cell count a pad lane falls back to when its `source` is empty or
-- | unparseable — a friendly 16ths grid you can click straight into.
defaultMeter :: Int
defaultMeter = 16

-- | The 13 pad lanes below the 3-lane Grids device. OH first (index 0) — the
-- | fixed bridge into the Grids group — then the standalone Tidal drum machine:
-- | 12 lanes with editable digraph labels, a GM-ish kit, on their own MIDI
-- | channel. Routing patterns address these by label.
defaultPatternLanes :: Array PatternLane
defaultPatternLanes =
  [ mkLane "OH" 46 "#2f8a8a"   -- open hat — the Grids/Tidal bridge (label fixed)
  -- the Tidal kit (labels editable; routing matches against them)
  , mkLane "bd" 36 "#b04a2f"   -- kick
  , mkLane "sn" 38 "#5f7d3f"   -- snare
  , mkLane "ch" 42 "#3f6f8a"   -- closed hat
  , mkLane "cp" 39 "#a8683f"   -- clap
  , mkLane "rs" 37 "#8a6f3f"   -- rim / side-stick
  , mkLane "lt" 45 "#6a5f8a"   -- low tom
  , mkLane "mt" 47 "#7a5f7a"   -- mid tom
  , mkLane "ht" 50 "#8a5f6a"   -- high tom
  , mkLane "cr" 49 "#5f7a8a"   -- crash
  , mkLane "rd" 51 "#5f8a7a"   -- ride
  , mkLane "cb" 56 "#8a8a3f"   -- cowbell
  , mkLane "sh" 70 "#7a8a6a"   -- shaker
  ]
  where
  mkLane name note color = { name, note, color, source: "", clicks: replicate defaultMeter false }

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

-- ---------------------------------------------------------------------------
-- Pattern lanes (the explicit drum-machine pads below the Grids device)
-- ---------------------------------------------------------------------------

-- | The heatmap lane index of the first pad lane (after BD/SD/HH).
firstPadLane :: Int
firstPadLane = 3

-- | How many pad lanes there are.
padCount :: Balistes -> Int
padCount b = length b.patternLanes

-- | A pad lane's mini-notation source string (the Tidal layer).
padSource :: Balistes -> Int -> String
padSource b i = maybe "" _.source (b.patternLanes !! i)

-- | A pad lane's clicked overlay (length = its derived meter).
padClicks :: Balistes -> Int -> Array Boolean
padClicks b i = maybe [] _.clicks (b.patternLanes !! i)

-- | Set a pad lane's mini-notation source (raw — meter/clicks resize is handled
-- | by `Triggerfish.Balistes.Tidal.setLaneSource`, which knows the parser).
setPadSource :: Int -> String -> Balistes -> Balistes
setPadSource i src b = case b.patternLanes !! i of
  Just pl -> b { patternLanes = fromMaybe b.patternLanes (updateAt i (pl { source = src }) b.patternLanes) }
  Nothing -> b

-- | Replace a pad lane's clicked overlay wholesale.
setPadClicks :: Int -> Array Boolean -> Balistes -> Balistes
setPadClicks i cs b = case b.patternLanes !! i of
  Just pl -> b { patternLanes = fromMaybe b.patternLanes (updateAt i (pl { clicks = cs }) b.patternLanes) }
  Nothing -> b

-- | Rename a Tidal kit lane (its label is what routing patterns match against).
-- | OH's label is fixed, so the component declines to call this for it.
setPadName :: Int -> String -> Balistes -> Balistes
setPadName i nm b = case b.patternLanes !! i of
  Just pl -> b { patternLanes = fromMaybe b.patternLanes (updateAt i (pl { name = nm }) b.patternLanes) }
  Nothing -> b

-- | Set routing-pattern slot `i` (a small fixed stack).
setRoute :: Int -> String -> Balistes -> Balistes
setRoute i src b = b { routes = fromMaybe b.routes (updateAt i src b.routes) }

-- | Replace the whole routing stack (variable length) — used when the SOURCE
-- | document is the authority for routes.
setRoutes :: Array String -> Balistes -> Balistes
setRoutes rs b = b { routes = rs }

-- | Is the clicked overlay of pad lane `i` on at cell `cell`?
clickAt :: Balistes -> Int -> Int -> Boolean
clickAt b i cell = maybe false (\pl -> fromMaybe false (pl.clicks !! cell)) (b.patternLanes !! i)

padNote :: Balistes -> Int -> Int
padNote b i = maybe 46 _.note (b.patternLanes !! i)

padName :: Balistes -> Int -> String
padName b i = maybe "" _.name (b.patternLanes !! i)

padColor :: Balistes -> Int -> String
padColor b i = maybe "#888888" _.color (b.patternLanes !! i)
