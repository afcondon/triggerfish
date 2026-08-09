-- | `Triggerfish.Selene.EnvLibrary` — starting points for FH-2 polyenv shapes.
-- |
-- | > "we could make Selene pick the envelope from a library that we curate to
-- | > get the fullest possible range of staring points to tweak."
-- |
-- | **Curated for COVERAGE, not for taste.** The job of this list is to tile the
-- | space so that flicking through it lands you within tweaking distance of
-- | anything, not to collect favourites. Fifteen shapes that span the axes beat
-- | forty that cluster around one aesthetic. The axes worth spanning:
-- |
-- |   * **attack** — instant (percussion) through to a slow swell
-- |   * **sustain** — 0 gives an AD/pluck that closes by itself; high gives an
-- |     organ/gate that holds for the length of the note
-- |   * **time bucket** — the firmware's 200ms…50s scale, and the single biggest
-- |     determinant of whether a shape reads as "snappy". A pluck at bucket 2
-- |     (1s) is a slow blob; the same numbers at bucket 0 (200ms) is a click.
-- |   * **depth sign** — above 64 is a normal positive envelope, below 64 is the
-- |     same shape INVERTED (duck a VCA, pull a filter down). Free, and easy to
-- |     forget exists.
-- |   * **velocity** — 64 is static, 127 is fully velocity-scaled. Whether a
-- |     shape responds to playing dynamics is a musical decision, not a default.
-- |
-- | **Two axes are deliberately absent**, and it is the same reason for both:
-- | `EnvDraw` renders straight segments, so neither the three curve-shape bytes
-- | (`attackShape` / `decayShape` / `releaseShape`, all 64 here) nor
-- | `randomDepth` changes the picture. Shapes differing only in those would be
-- | identical-looking cells on a wall whose entire premise is that a shape
-- | describes itself. They are real axes and worth covering — but the drawing
-- | has to be able to show them first, and in the curve-shapes' case we do not
-- | yet know what the byte values mean (see `FH2.Modes.PolyEnv`: the Preset Tool
-- | stores a raw 0..127 with no exposed enum).
-- |
-- | These are a STARTER set, deliberately small and deliberately editable. The
-- | natural next step is Amphora artefacts — pick, tweak, name, and the named one
-- | joins the library — which is the same promotion idiom as every other bank in
-- | the system. Kept in code for now so there is something to curate FROM.
-- |
-- | An envelope shape carries no range and no jack: those are placement, and
-- | placement lives on the routing destination. That is what makes a shape
-- | portable between rigs and therefore worth collecting.
module Triggerfish.Selene.EnvLibrary
  ( Starter
  , starters
  , starterAt
  , nameOf
  ) where

import Prelude

import Data.Array (findIndex, length, (!!))
import Data.Maybe (Maybe)

import Triggerfish.Selene.Model (EnvSlot, defaultEnvSlot)

type Starter = { name :: String, slot :: EnvSlot }

-- | Build on `defaultEnvSlot` so a shape only states what it actually decides,
-- | and so any future field gains a sane value here for free.
mk
  :: String
  -> { a :: Int, d :: Int, s :: Int, r :: Int, t :: Int }
  -> Starter
mk name p =
  { name
  , slot: defaultEnvSlot { attack = p.a, decay = p.d, sustain = p.s, release = p.r, timeRange = p.t }
  }

-- | Variant with the two expressive fields pinned: `dep` is the attenuverter
-- | (64 zero, <64 inverted) and `vel` the velocity scaling (64 = none).
mkX
  :: String
  -> { a :: Int, d :: Int, s :: Int, r :: Int, t :: Int, dep :: Int, vel :: Int }
  -> Starter
mkX name p =
  { name
  , slot: defaultEnvSlot
      { attack = p.a, decay = p.d, sustain = p.s, release = p.r
      , timeRange = p.t, depth = p.dep, velDepth = p.vel }
  }

-- | Time buckets, by name rather than by magic number at each call site.
-- | 0..7 → 200ms / 500ms / 1s / 2s / 5s / 10s / 20s / 50s.
fast :: Int
fast = 0

quick :: Int
quick = 1

mid :: Int
mid = 2

slow :: Int
slow = 3

long :: Int
long = 4

vlong :: Int
vlong = 5

huge :: Int
huge = 6

vast :: Int
vast = 7

-- | The set, in four groups of nine. Ordered so that CYCLING through it is
-- | itself a gesture rather than a shuffle through a bag: percussive first,
-- | opening out through gated and sustained, then the long-tailed and slow, and
-- | finally the expressive edges where velocity and the attenuverter's negative
-- | half live. Flicking `[` / `]` walks that continuum end to end.
starters :: Array Starter
starters =
  -- ── PERCUSSIVE — sustain 0, fast buckets. These close by themselves. ──
  [ mk "click"    { a: 0,   d: 4,   s: 0,   r: 2,   t: fast }
  , mk "stab"     { a: 22,  d: 26,  s: 0,   r: 14,  t: fast }
  , mk "blip"     { a: 2,   d: 12,  s: 0,   r: 6,   t: fast }
  , mk "pluck"    { a: 0,   d: 30,  s: 0,   r: 20,  t: fast }
  , mk "snap"     { a: 0,   d: 18,  s: 8,   r: 10,  t: fast }
  , mk "knock"    { a: 0,   d: 24,  s: 0,   r: 64,  t: fast }
  , mk "perc"     { a: 0,   d: 60,  s: 0,   r: 40,  t: fast }
  , mk "tom"      { a: 0,   d: 74,  s: 0,   r: 56,  t: quick }
  , mk "mallet"   { a: 4,   d: 46,  s: 14,  r: 36,  t: quick }
  -- ── GATED / SUSTAINED — held for the length of the note. ──
  , mk "gate"     { a: 0,   d: 0,   s: 127, r: 0,   t: fast }
  , mk "organ"    { a: 6,   d: 0,   s: 127, r: 12,  t: quick }
  , mk "hold"     { a: 0,   d: 20,  s: 96,  r: 8,   t: quick }
  , mk "asr"      { a: 26,  d: 0,   s: 118, r: 44,  t: mid }
  , mk "bloom"    { a: 40,  d: 30,  s: 80,  r: 60,  t: mid }
  , mk "pad"      { a: 72,  d: 60,  s: 96,  r: 100, t: slow }
  , mk "drift"    { a: 90,  d: 40,  s: 110, r: 90,  t: long }
  , mk "swell"    { a: 112, d: 84,  s: 104, r: 120, t: long }
  , mk "drone"    { a: 127, d: 0,   s: 127, r: 127, t: vlong }
  -- ── LONG-TAILED and SLOW — the release-heavy class, and the two slowest
  --    buckets, which nothing else in the set reached. ──
  , mk "echo"     { a: 0,   d: 10,  s: 0,   r: 100, t: mid }
  , mk "bell"     { a: 0,   d: 20,  s: 0,   r: 120, t: slow }
  , mk "gong"     { a: 0,   d: 40,  s: 10,  r: 127, t: long }
  , mk "fall"     { a: 0,   d: 127, s: 0,   r: 10,  t: vlong }
  , mk "ramp"     { a: 104, d: 10,  s: 0,   r: 10,  t: long }
  , mk "tide"     { a: 100, d: 60,  s: 90,  r: 110, t: vlong }
  , mk "glacier"  { a: 120, d: 90,  s: 100, r: 127, t: huge }
  , mk "dawn"     { a: 127, d: 20,  s: 0,   r: 40,  t: huge }
  , mk "aeon"     { a: 127, d: 100, s: 110, r: 127, t: vast }
  -- ── EXPRESSIVE — velocity response and the attenuverter's negative half.
  --    `soft` is INVERSE velocity: quiet notes open it further, which is a real
  --    modular gesture and impossible to reach by accident. ──
  , mkX "accent"  { a: 0,   d: 36,  s: 0,   r: 26,  t: fast,  dep: 127, vel: 127 }
  , mkX "static"  { a: 0,   d: 36,  s: 0,   r: 26,  t: fast,  dep: 127, vel: 64 }
  , mkX "soft"    { a: 0,   d: 36,  s: 0,   r: 26,  t: fast,  dep: 127, vel: 32 }
  , mkX "lean"    { a: 0,   d: 36,  s: 0,   r: 26,  t: fast,  dep: 90,  vel: 96 }
  , mkX "halfduck" { a: 0,  d: 50,  s: 0,   r: 40,  t: quick, dep: 40,  vel: 96 }
  , mkX "dip"     { a: 4,   d: 54,  s: 20,  r: 44,  t: quick, dep: 28,  vel: 96 }
  , mkX "duck"    { a: 0,   d: 70,  s: 0,   r: 54,  t: quick, dep: 0,   vel: 96 }
  , mkX "igate"   { a: 0,   d: 0,   s: 127, r: 0,   t: fast,  dep: 0,   vel: 96 }
  , mkX "isw"     { a: 80,  d: 60,  s: 70,  r: 90,  t: slow,  dep: 10,  vel: 96 }
  ]

starterAt :: Int -> Maybe Starter
starterAt i = starters !! (((i `mod` n) + n) `mod` n)
  where
  n = max 1 (length starters)

-- | The library name of a slot, if it is still exactly one of the starters.
-- | Returns `Nothing` once it has been tweaked — which is the honest answer: a
-- | shape that has been edited is no longer "pluck", it is an unnamed shape that
-- | started there. (Naming it is what would promote it to an artefact.)
nameOf :: EnvSlot -> Maybe String
nameOf sl = map _.name (findIndex (\st -> st.slot == sl) starters >>= starterAt)
