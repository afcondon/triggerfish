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
import Data.Maybe (Maybe(..), fromMaybe)

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

-- | The set. Ordered so that CYCLING through it is itself a gesture: percussive
-- | at the top, opening out through sustained and into the long swells, with the
-- | two inverted shapes last. Flicking `[` / `]` walks that continuum.
starters :: Array Starter
starters =
  -- ── percussive: sustain 0, fastest bucket. These close by themselves. ──
  [ mk "click"    { a: 0,   d: 4,   s: 0,   r: 2,   t: fast }
  , mk "blip"     { a: 2,   d: 12,  s: 0,   r: 6,   t: fast }
  , mk "pluck"    { a: 0,   d: 30,  s: 0,   r: 20,  t: fast }
  , mk "snap"     { a: 0,   d: 18,  s: 8,   r: 10,  t: fast }
  , mk "perc"     { a: 0,   d: 60,  s: 0,   r: 40,  t: fast }
  , mk "tom"      { a: 0,   d: 74,  s: 0,   r: 56,  t: quick }
  , mk "mallet"   { a: 4,   d: 46,  s: 14,  r: 36,  t: quick }
  -- ── gated: sustain high, so the shape holds for the length of the note ──
  , mk "gate"     { a: 0,   d: 0,   s: 127, r: 0,   t: fast }
  , mk "organ"    { a: 6,   d: 0,   s: 127, r: 12,  t: quick }
  , mk "asr"      { a: 26,  d: 0,   s: 118, r: 44,  t: mid }
  -- ── sustained and slow: the swell end of the continuum ──
  , mk "pad"      { a: 72,  d: 60,  s: 96,  r: 100, t: slow }
  , mk "swell"    { a: 112, d: 84,  s: 104, r: 120, t: long }
  , mk "drone"    { a: 127, d: 0,   s: 127, r: 127, t: vlong }
  , mk "ramp"     { a: 104, d: 10,  s: 0,   r: 10,  t: long }
  -- ── expressive edges: velocity-forward, and the inverted pair ──
  , mkX "accent"  { a: 0,   d: 36,  s: 0,   r: 26,  t: fast,  dep: 127, vel: 127 }
  , mkX "static"  { a: 0,   d: 36,  s: 0,   r: 26,  t: fast,  dep: 127, vel: 64 }
  -- depth BELOW 64 inverts: the envelope pulls DOWN from rest. `duck` is the
  -- sidechain shape; `dip` is a gentler, partially-inverted version.
  , mkX "duck"    { a: 0,   d: 70,  s: 0,   r: 54,  t: quick, dep: 0,   vel: 96 }
  , mkX "dip"     { a: 4,   d: 54,  s: 20,  r: 44,  t: quick, dep: 28,  vel: 96 }
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
