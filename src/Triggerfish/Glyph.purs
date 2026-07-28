-- | `Triggerfish.Glyph` — the **glyph substrate**: a deterministic map from a
-- | captured state's canonical eDSL text to a memorable pictographic identity.
-- | The resolution to the fast-vs-named preset tension (see
-- | `docs/DESIGN-scene-modal.md`): every capture gets an auto glyph — identity
-- | WITHOUT a name — so anonymous captures are recallable and sequenceable with
-- | zero flow-break, and naming becomes a later, optional promotion.
-- |
-- | Two orthogonal axes, per the design:
-- |
-- |   • **content → icons.** A content hash of the canonical text picks an
-- |     ORDERED PAIR from a fixed icon deck (`glyphOf`). Identical states hash to
-- |     the same pair (free dedup); a real edit avalanches to a visibly different
-- |     pair (the honest "this changed" signal). Pairs, not singles: ~64 icons
-- |     give ~4000 ordered pairs (collisions vanish), and an absurd pair
-- |     ("cow-ambulance", "star-bomb") is far more memorable than a lone icon —
-- |     real bizarre-imagery mnemonics.
-- |   • **machine → hue.** The colour comes from the machine (`hueOf`), never the
-- |     content, so a row reads at a glance ("four green Balistes then one amber
-- |     Vetula"). FontAwesome solid glyphs inherit CSS `color`, so the hue is a
-- |     one-property tint at the view layer.
-- |
-- | This module is PURE and shared across all six machines — it holds no
-- | rendering (the view maps `icon` → a FontAwesome class and applies the hue)
-- | and no state. The `alias` (`"cow-ambulance"`) is the typeable form used by
-- | the `:`+Tab completion in macro-Tidal patterns; the icons are its rendering.
module Triggerfish.Glyph
  ( Machine(..)
  , allMachines
  , machineLabel
  , hueOf
  , DeckEntry
  , deck
  , deckSize
  , Glyph
  , glyphOf
  , glyphFromAlias
  , ChipView
  ) where

import Prelude

import Data.Array (find, index, length)
import Data.Char (toCharCode)
import Data.Foldable (foldl)
import Data.Maybe (fromMaybe)
import Data.String.CodeUnits (toCharArray)
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))

-- ---------------------------------------------------------------------------
-- Machines — the six instrument panels. Hue is a per-machine identity colour
-- (the tab-bar status board and the chips tint their icons with it).
-- ---------------------------------------------------------------------------

data Machine
  = Odonus
  | Balistes
  | Selene
  | Vetula
  | Sufflamen
  | Stellatus

derive instance eqMachine :: Eq Machine

allMachines :: Array Machine
allMachines = [ Odonus, Balistes, Selene, Vetula, Sufflamen, Stellatus ]

machineLabel :: Machine -> String
machineLabel = case _ of
  Odonus -> "ODONUS"
  Balistes -> "BALISTES"
  Selene -> "SELENE"
  Vetula -> "VETULA"
  Sufflamen -> "SUFFLAMEN"
  Stellatus -> "STELLATUS"

-- | The machine's identity hue, as a CSS colour. Restrained, spaced around the
-- | wheel so six chips are distinguishable at tab size but stay Swiss-muted
-- | (mid-saturation, mid-lightness — not primary-bright).
hueOf :: Machine -> String
hueOf = case _ of
  Odonus -> "hsl(210, 46%, 46%)" -- blue
  Balistes -> "hsl(150, 40%, 40%)" -- green
  Selene -> "hsl(270, 32%, 52%)" -- violet
  Vetula -> "hsl(35, 55%, 46%)" -- amber
  Sufflamen -> "hsl(0, 48%, 50%)" -- red
  Stellatus -> "hsl(188, 44%, 40%)" -- teal

-- ---------------------------------------------------------------------------
-- The icon deck — memorable, concrete nouns that each have a FontAwesome free
-- SOLID glyph of the same name (the view builds `fa-solid fa-<icon>`). The
-- `alias` IS the icon name here, so the typeable form and the picture agree.
-- Order is FIXED and APPEND-ONLY: an index is a persisted identity, so
-- reordering or removing an entry would silently remap every stored glyph.
-- ---------------------------------------------------------------------------

type DeckEntry = { icon :: String }

deck :: Array DeckEntry
deck = map { icon: _ }
  [ "bomb", "star", "moon", "sun", "cloud", "bolt", "fire", "leaf"
  , "tree", "feather", "fish", "frog", "crow", "dove", "cat", "dog"
  , "horse", "hippo", "dragon", "spider", "bug", "ghost", "skull", "heart"
  , "anchor", "bell", "key", "lock", "gem", "crown", "cube", "dice"
  , "flask", "rocket", "bicycle", "car", "plane", "ship", "truck", "bus"
  , "tractor", "compass", "map", "book", "guitar", "drum", "music", "umbrella"
  , "snowflake", "mountain", "tornado", "meteor", "atom", "brain", "eye", "cow"
  , "ambulance", "hammer", "wrench", "gear", "seedling", "spa", "plug", "bath"
  ]

deckSize :: Int
deckSize = length deck

-- ---------------------------------------------------------------------------
-- The glyph itself — an ordered pair of deck entries plus its typeable alias.
-- ---------------------------------------------------------------------------

-- | A captured state's identity: two icons (rendered as the picture) and the
-- | hyphen-joined `alias` (`"cow-ambulance"`) — the single mini-notation token
-- | the `:`+Tab completion inserts.
type Glyph =
  { first :: DeckEntry
  , second :: DeckEntry
  , alias :: String
  }

-- | What a machine reports up to the shell's six-machine status board: its parked
-- | glyph and whether the live state has diverged from it (`true` → render ghosted
-- | + MOD, `false` → solid/held). A machine with no parked identity reports
-- | `Nothing` (empty), so `Maybe ChipView` is the full per-machine chip state.
type ChipView =
  { glyph :: Glyph
  , diverged :: Boolean
  }

-- | The glyph for a canonical text (e.g. a `TriSnapshot`'s `printTri`). Two
-- | independent hashes pick an ordered pair; the second is bumped off the first
-- | so the pair is always genuinely two distinct icons.
glyphOf :: String -> Glyph
glyphOf text =
  let
    n = deckSize
    i1 = hashWith 5381 33 text `mod` n
    i2raw = hashWith 7919 37 text `mod` n
    i2 = if i2raw == i1 then (i2raw + 1) `mod` n else i2raw
    a = entryAt i1
    b = entryAt i2
  in
    { first: a, second: b, alias: a.icon <> "-" <> b.icon }

-- | Recover a glyph from its alias (`"cow-ambulance"`) — for rendering a token
-- | that a macro-pattern already carries, without re-hashing anything. Falls
-- | back to the raw halves if either name isn't in the deck (so a hand-typed or
-- | promoted alias still renders sensibly).
glyphFromAlias :: String -> Glyph
glyphFromAlias alias = case split (Pattern "-") alias of
  [ a, b ] -> { first: lookupEntry a, second: lookupEntry b, alias }
  _ -> { first: lookupEntry alias, second: lookupEntry alias, alias }

-- ---------------------------------------------------------------------------
-- Internals
-- ---------------------------------------------------------------------------

-- A bounded, deterministic string hash (djb2-family, reduced each step to stay
-- inside Int's safe range regardless of length). `seed`/`mult` vary to get two
-- weakly-independent hashes from one text.
hashWith :: Int -> Int -> String -> Int
hashWith seed mult text =
  abs (foldl step seed (toCharArray text))
  where
  step h c = (h * mult + toCharCode c) `mod` 1000003
  abs x = if x < 0 then -x else x

entryAt :: Int -> DeckEntry
entryAt i = fromMaybe { icon: "star" } (index deck i)

lookupEntry :: String -> DeckEntry
lookupEntry name =
  fromMaybe { icon: name } (find (\e -> e.icon == name) deck)
