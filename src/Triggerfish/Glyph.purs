-- | `Triggerfish.Glyph` — Triggerfish's window onto **Rebus**, plus the six
-- | machines.
-- |
-- | The glyph substrate — the deterministic map from a captured state's canonical
-- | eDSL text to an ordered run of coloured icons, and the alias that spells it —
-- | moved out to its own library on 2026-09-13
-- | (`code-typography/rebus`, `import Rebus`). It left because a second consumer
-- | appeared: Quadrat names sample sets the same way, and two programs that must
-- | draw the SAME picture for the same content cannot each keep their own copy of
-- | the deck and the hash. Rebus's `Canonical` class and its golden corpus are
-- | what hold them to it.
-- |
-- | What stayed here is what is actually Triggerfish's: the six machines, their
-- | labels, their two-letter tags and their accent hues. A machine is not a
-- | general idea, and Rebus has no business knowing about Odonus.
-- |
-- | The names below are the ones the rest of Triggerfish already calls
-- | (`glyphOf`, `glyphFromAlias`, `sessionAliasOf`), so this module is a thin
-- | rename over Rebus rather than a layer. New code can `import Rebus` directly —
-- | and chord content in particular now has a `Rebus.Chords` instance carrying
-- | the exact serialisation `Vetula.App` hand-builds.
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
  , chordGlyph
  , glyphFromAlias
  , sessionAliasOf
  , ChipView
  ) where

import Prelude

import Data.Array (length)
import Rebus as Rebus

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
-- Rebus, under Triggerfish's names.
-- ---------------------------------------------------------------------------

-- | One rendered element of a glyph: an icon name + its CSS colour. The view
-- | turns the name into `fa-solid fa-<icon>`; Rebus itself draws nothing.
type GlyphIcon = Rebus.GlyphIcon

-- | A captured state's identity: its coloured icons (the picture) + the
-- | hyphen-joined `alias` (`"cow-ambulance"`), the single mini-notation token the
-- | `:`+Tab completion inserts.
-- |
-- | Rebus mints these at any width from one to five, and `Glyph.icons` is an
-- | array for that reason. Triggerfish stays at TWO, and that is a persisted-data
-- | fact rather than a preference: every alias already saved in a preset, a scene
-- | or a macro pattern is a pair, so widening `glyphOf` would re-identify all of
-- | them. A machine that one day needs to tell apart more than a few thousand
-- | things should say so with `Rebus.rebusOfTextWidth` and own the migration.
type Glyph = Rebus.Glyph

-- | What a machine reports up to the shell's six-machine status board: its parked
-- | glyph and whether the live state has diverged from it (`true` → render ghosted
-- | + MOD, `false` → solid/held). A machine with no parked identity reports
-- | `Nothing` (empty), so `Maybe ChipView` is the full per-machine chip state.
type ChipView = Rebus.ChipView

-- | The glyph for a canonical text (e.g. a `TriSnapshot`'s `printTri`).
-- | Triggerfish always hands over text it has already printed canonically, so
-- | this is Rebus's already-canonical entry point rather than `rebusOf`.
glyphOf :: String -> Glyph
glyphOf = Rebus.rebusOfText

-- | **The glyph of a chord sequence**, and the one that another program can
-- | arrive at independently.
-- |
-- | A progression's identity is its CHORDS. Not its rendered source, which
-- | carries a key label and comments that are context rather than content —
-- | two presets of the same voicings in differently-labelled keys are the same
-- | progression, and used to wear different pictures. Not its pitch classes
-- | either, which throw away the register that makes a voicing a voicing.
-- |
-- | `Rebus.chordsOf` is the shared normal form: each chord sorted, because the
-- | order notes are listed in is an accident of how they were read — bass-first
-- | from a voicing here, finger order from a MIDI capture in Quadrat — and the
-- | progression's own order left alone, because backwards is a different piece.
-- |
-- | That is the whole contract. Quadrat's sampler mints a chord set's rebus the
-- | same way, so a progression caught there and one saved here wear the same
-- | icons, and neither app needs to know the other exists for that to hold.
chordGlyph :: Array (Array Int) -> Glyph
chordGlyph = Rebus.rebusOf <<< Rebus.chordsOf

-- | Recover a glyph from its alias (`"cow-ambulance"`) — for rendering a token a
-- | macro-pattern already carries.
glyphFromAlias :: String -> Glyph
glyphFromAlias = Rebus.rebusFromAlias

-- | A SESSION's identity alias: three distinct deck icons from a seed
-- | (`"cat-rocket-anchor"`), rendered MONOCHROME by the view so a container never
-- | reads as a chord token.
sessionAliasOf :: Int -> String
sessionAliasOf = Rebus.sessionAliasOf

-- | The icon deck. Order is an identity, so this is Rebus's fixed, append-only
-- | `defaultDeck` — Triggerfish has no deck of its own and must not grow one, or
-- | it stops agreeing with Quadrat.
deck :: Array String
deck = Rebus.deckIcons Rebus.defaultDeck

deckSize :: Int
deckSize = length deck

-- | The per-icon colour palette — an accent, not part of the identity.
palette :: Array String
palette = Rebus.paletteColors Rebus.defaultPalette
