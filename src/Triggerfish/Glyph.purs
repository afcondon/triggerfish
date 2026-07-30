-- | `Triggerfish.Glyph` — the **glyph substrate**: a deterministic map from a
-- | captured state's canonical eDSL text to a memorable pictographic identity.
-- | The resolution to the fast-vs-named preset tension (see
-- | `docs/DESIGN-scene-modal.md`): every capture gets an auto glyph — identity
-- | WITHOUT a name — so anonymous captures are recallable and sequenceable with
-- | zero flow-break, and naming becomes a later, optional promotion.
-- |
-- | A glyph is an ORDERED PAIR of **coloured icons** — "red cow, blue star":
-- |
-- |   • **content → icon shapes.** A content hash of the canonical text picks an
-- |     ordered pair from a fixed icon deck (`glyphOf`). Identical states hash to
-- |     the same pair (free dedup); a real edit avalanches to a visibly different
-- |     pair. Pairs, not singles: ~64 icons give ~4000 ordered pairs, and an
-- |     absurd pair ("cow-ambulance", "star-bomb") is far more memorable than a
-- |     lone icon — real bizarre-imagery mnemonics.
-- |   • **icon name → colour.** Each icon carries a colour derived from its OWN
-- |     name, so `cow` is always red and `star` always blue: the pair reads as
-- |     "red cow, blue star", and it renders identically whether drawn live or
-- |     reconstructed from the typed `alias` in the pictographic mirror. Colour is
-- |     a memorability accent, not extra identity (the shape pair is the identity).
-- |
-- | Machine identity is NOT colour anymore (colour belongs to the content). In a
-- | mixed sequencer lane a preset carries a two-letter `machineTag` (Od/Ba/Se/Ve/
-- | Su/St); in the tab-bar board the machine is already obvious from its segment.
-- |
-- | PURE and shared across all six machines — no rendering (the view maps `icon`
-- | → a FontAwesome class and applies `color`) and no state. The `alias`
-- | (`"cow-ambulance"`) is the typeable form used by the `:`+Tab completion.
module Triggerfish.Glyph
  ( Machine(..)
  , allMachines
  , machineLabel
  , machineTag
  , hueOf
  , palette
  , GlyphIcon
  , deck
  , deckSize
  , Glyph
  , glyphOf
  , glyphFromAlias
  , ChipView
  ) where

import Prelude

import Data.Array (index, length)
import Data.Char (toCharCode)
import Data.Foldable (foldl)
import Data.Maybe (fromMaybe)
import Data.String.CodeUnits (toCharArray)
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))

-- ---------------------------------------------------------------------------
-- Machines — the six instrument panels.
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

-- | The two-letter machine tag for a mixed sequencer lane, where colour now
-- | encodes content rather than machine. Selene / Sufflamen / Stellatus stay
-- | distinct as Se / Su / St.
machineTag :: Machine -> String
machineTag = case _ of
  Odonus -> "Od"
  Balistes -> "Ba"
  Selene -> "Se"
  Vetula -> "Ve"
  Sufflamen -> "Su"
  Stellatus -> "St"

-- | A per-machine accent hue, kept for chrome that still wants to colour BY
-- | machine (e.g. a tag badge). No longer used to tint glyph icons — those are
-- | coloured by content now. Darkened for legibility on the light/gold bar.
hueOf :: Machine -> String
hueOf = case _ of
  Odonus -> "hsl(210, 60%, 32%)" -- blue
  Balistes -> "hsl(150, 55%, 26%)" -- green
  Selene -> "hsl(270, 42%, 40%)" -- violet
  Vetula -> "hsl(32, 75%, 34%)" -- amber
  Sufflamen -> "hsl(0, 58%, 40%)" -- red
  Stellatus -> "hsl(188, 60%, 26%)" -- teal

-- ---------------------------------------------------------------------------
-- The icon deck — memorable, concrete nouns that each have a FontAwesome free
-- SOLID glyph of the same name (the view builds `fa-solid fa-<icon>`).
-- Order is FIXED and APPEND-ONLY: an index is a persisted identity, so
-- reordering or removing an entry would silently remap every stored glyph.
-- ---------------------------------------------------------------------------

deck :: Array String
deck =
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

-- | The per-icon colour palette. Distinguishable, mid-dark so a small icon reads
-- | on the light/gold tab bar. Spaced around the wheel; FontAwesome solid inherits
-- | CSS `color`, so a colour is a one-property tint at the view layer.
palette :: Array String
palette =
  [ "hsl(0, 62%, 44%)"    -- red
  , "hsl(28, 72%, 42%)"   -- orange
  , "hsl(45, 80%, 36%)"   -- ochre
  , "hsl(142, 55%, 32%)"  -- green
  , "hsl(188, 62%, 32%)"  -- teal
  , "hsl(214, 62%, 44%)"  -- blue
  , "hsl(262, 44%, 50%)"  -- violet
  , "hsl(322, 52%, 46%)"  -- magenta
  ]

paletteSize :: Int
paletteSize = length palette

-- ---------------------------------------------------------------------------
-- The glyph — an ordered pair of coloured icons plus its typeable alias.
-- ---------------------------------------------------------------------------

-- | One rendered element of a glyph: a FontAwesome icon name + its CSS colour.
type GlyphIcon =
  { icon :: String
  , color :: String
  }

-- | A captured state's identity: two coloured icons (the picture) + the
-- | hyphen-joined `alias` (`"cow-ambulance"`), the single mini-notation token the
-- | `:`+Tab completion inserts.
type Glyph =
  { first :: GlyphIcon
  , second :: GlyphIcon
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
-- | independent hashes pick an ordered SHAPE pair (second bumped off the first so
-- | the two icons always differ); each icon's colour follows its name.
glyphOf :: String -> Glyph
glyphOf text =
  let
    i1 = hashWith 5381 33 text `mod` deckSize
    i2raw = hashWith 7919 37 text `mod` deckSize
    i2 = if i2raw == i1 then (i2raw + 1) `mod` deckSize else i2raw
  in
    glyphFor (entryAt i1) (entryAt i2)

-- | Recover a glyph from its alias (`"cow-ambulance"`) — for rendering a token a
-- | macro-pattern already carries. Because colour follows the icon NAME, the
-- | result is identical to the live `glyphOf` render of the same pair.
glyphFromAlias :: String -> Glyph
glyphFromAlias alias = case split (Pattern "-") alias of
  [ a, b ] -> glyphFor a b
  _ -> glyphFor alias alias

-- ---------------------------------------------------------------------------
-- Internals
-- ---------------------------------------------------------------------------

-- Build a coloured pair from two icon names: each icon's colour is a hash of its
-- own name; the second is bumped off the first so a pair shows two colours.
glyphFor :: String -> String -> Glyph
glyphFor n1 n2 =
  let
    c1 = colorIdx n1
    c2raw = colorIdx n2
    c2 = if c2raw == c1 then (c2raw + 1) `mod` paletteSize else c2raw
  in
    { first: { icon: n1, color: colorAt c1 }
    , second: { icon: n2, color: colorAt c2 }
    , alias: n1 <> "-" <> n2
    }

colorIdx :: String -> Int
colorIdx name = hashWith 2749 31 name `mod` paletteSize

colorAt :: Int -> String
colorAt i = fromMaybe "#5a564b" (index palette i)

-- A bounded, deterministic string hash (djb2-family, reduced each step to stay
-- inside Int's safe range regardless of length). `seed`/`mult` vary to get
-- weakly-independent hashes from one text.
hashWith :: Int -> Int -> String -> Int
hashWith seed mult text =
  abs (foldl step seed (toCharArray text))
  where
  step h c = (h * mult + toCharCode c) `mod` 1000003
  abs x = if x < 0 then -x else x

-- A deck index → its icon name; a name not in the deck falls back to itself so a
-- hand-typed / promoted alias still renders.
entryAt :: Int -> String
entryAt i = fromMaybe "star" (index deck i)
