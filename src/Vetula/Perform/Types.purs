-- | `Vetula.Perform.Types` — the pure value types of the Perform surface, shared
-- | by the live component (`Vetula.App`) and the document serialiser
-- | (`Vetula.Lepidoptera`). Extracted from `App` so the serialiser can depend on
-- | these without importing the 6k-line Halogen module — and so `App` can import
-- | the serialiser (for a save button) without a cycle.
-- |
-- | Everything here is pure (Prelude + `Data.Array.reverse` only): the layer/fx
-- | algebra, the terminal sink, and the canonical ASCII vocabulary the document
-- | round-trip prints and parses (NOT the glyph labels, which are lossy display).
module Vetula.Perform.Types where

import Prelude

import Data.Array (reverse)
import Data.Maybe (Maybe(..))

-- | A function-stack LAYER on a Perform box — a uniform `Pattern (Array Int) ->
-- | Pattern (Array Int)` endomorphism (see `applyFx`), so any layer drags anywhere
-- | in the stack or between boxes. Two families under one type: pitch-shapers that
-- | `map` over each chord (Transpose · Octave) and Tidal combinators polymorphic in
-- | the value (Rate = `fast`/`slow`). Voice/Select (Harmonia) + more land next;
-- | arp/strum are the terminal REALISATION, not layers (they explode chord→time).
data PerfFx
  = Transpose Int    -- ± semitones
  | Octave Int       -- ± octaves
  | Rate Int         -- speed: n>0 `fast n`, n<0 `slow (-n)`, 0 = identity
  | Voice VoiceShape -- re-voice each chord (Harmonia VoicingStrategy)
  | Select PerfSel   -- thin each chord to some of its voices (Harmonia takeVoicing)
  | Arpg ArpDir Int  -- explode chord→time at a FIXED rate (notes per beat), a
                     -- direction; block = no arp. Rate-driven, so dense chords
                     -- don't rush (each note the same length regardless of count).
  | Strum Int        -- explode chord→time as a fast onset stagger (ms per note)

derive instance eqPerfFx :: Eq PerfFx

-- | Arpeggiation order of a chord's notes (low→high, high→low, or bounce).
data ArpDir = ArpUp | ArpDown | ArpUpDown

derive instance eqArpDir :: Eq ArpDir

arpDirGlyph :: ArpDir -> String
arpDirGlyph = case _ of
  ArpUp -> "↑"
  ArpDown -> "↓"
  ArpUpDown -> "↕"

cycleArpDir :: ArpDir -> ArpDir
cycleArpDir = case _ of
  ArpUp -> ArpDown
  ArpDown -> ArpUpDown
  ArpUpDown -> ArpUp

-- | A chord's notes in an arp direction's order.
arpOrder :: ArpDir -> Array Int -> Array Int
arpOrder = case _ of
  ArpUp -> identity
  ArpDown -> reverse
  ArpUpDown -> \ns -> ns <> reverse ns

-- | Chord re-voicings — Harmonia `Voicing -> Voicing` strategies.
data VoiceShape = Open | Rootless | Drop2 | Drop24 | Quartal | Cluster

derive instance eqVoiceShape :: Eq VoiceShape

-- | Voice selection — keep the low or high N voices of each chord (Harmonia
-- | `Selector`). `Low 1` = a bass line; `High 1` = a melody line.
data PerfSel = Low Int | High Int

derive instance eqPerfSel :: Eq PerfSel

-- | The "WHEN" clause on a layer — the flat form of Tidal's conditional combinators
-- | (`every` / `sometimesBy` / `within`). Rather than a layer that WRAPS a sub-stack,
-- | each layer carries a condition for when it applies, implemented with Tidal's own
-- | cycle-aware combinators. Prototype: `Always` or `Every n` (via `every`).
data When = Always | Every Int

derive instance eqWhen :: Eq When

whenLabel :: When -> String
whenLabel = case _ of
  Always -> "∀"
  Every n -> "e" <> show n

cycleWhen :: When -> When
cycleWhen = case _ of
  Always -> Every 2
  Every 2 -> Every 3
  Every 3 -> Every 4
  Every 4 -> Every 8
  Every _ -> Always

-- | A stack entry: a function `fx` plus the `when` clause gating it per cycle.
type Layer = { fx :: PerfFx, when :: When }

-- | A fresh layer from the palette applies every cycle until you dial its clause.
mkLayer :: PerfFx -> Layer
mkLayer fx = { fx, when: Always }

-- | What an HTML5 drag is carrying: a fresh layer FROM the palette, or an existing
-- | layer being moved FROM a box's stack (box index, layer index). Dropping onto a
-- | box appends; onto a layer chip inserts before it (reorder / precise placement).
data PerfDragSrc = FromPalette PerfFx | FromBox Int Int

-- | The box's terminal SINK — the fold's cap, one per box, swapped not stacked
-- | (docs/DESIGN-vetula-chyron-redesign §Perform, Model A "forked tail"). The seam
-- | where Solo and Atlantis diverge: `TMidi`/`TOdo` are Solo-capable (browser
-- | WebMIDI / feed Odonus locally); `TRig` is a rig-only destination (CV/OSC/ES-9/
-- | FH-2) the browser can't sound — so in Solo it GHOSTS (silent, greyed). Only
-- | the terminal forks; the whole layer body above it is shared across runtimes.
data PerfTerm = TMidi | TOdo | TRig

derive instance eqPerfTerm :: Eq PerfTerm

termLabel :: PerfTerm -> String
termLabel = case _ of
  TMidi -> "→ midi"
  TOdo -> "→ odo"
  TRig -> "→ rig"

-- | Short pill label for the terminal selector.
termShort :: PerfTerm -> String
termShort = case _ of
  TMidi -> "midi"
  TOdo -> "odo"
  TRig -> "rig"

nextTerm :: PerfTerm -> PerfTerm
nextTerm = case _ of
  TMidi -> TOdo
  TOdo -> TRig
  TRig -> TMidi

-- | A rig-only terminal has no local (browser) realisation.
termRigOnly :: PerfTerm -> Boolean
termRigOnly = case _ of
  TRig -> true
  _ -> false

-- ---------------------------------------------------------------------------
-- The canonical ASCII vocabulary for the Perform pipeline / document round-trip.
-- Round-trippable (NOT the glyph labels); `drop24` is a distinct token from the
-- display `drop2&4`. Shared by App's inline-field round-trip and Lepidoptera.
-- ---------------------------------------------------------------------------

printArpDir :: ArpDir -> String
printArpDir = case _ of
  ArpUp -> "up"
  ArpDown -> "down"
  ArpUpDown -> "updown"

parseArpDir :: String -> ArpDir
parseArpDir = case _ of
  "down" -> ArpDown
  "updown" -> ArpUpDown
  _ -> ArpUp

printVoiceShape :: VoiceShape -> String
printVoiceShape = case _ of
  Open -> "open"
  Rootless -> "rootless"
  Drop2 -> "drop2"
  Drop24 -> "drop24"
  Quartal -> "quartal"
  Cluster -> "cluster"

parseVoiceShape :: String -> Maybe VoiceShape
parseVoiceShape = case _ of
  "open" -> Just Open
  "rootless" -> Just Rootless
  "drop2" -> Just Drop2
  "drop24" -> Just Drop24
  "quartal" -> Just Quartal
  "cluster" -> Just Cluster
  _ -> Nothing
