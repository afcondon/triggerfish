# Vetula — the Tank model (north star)

Status: **build slices A–E landed** (2026-07-06), branch `vetula-tank`. Supersedes the
path-on-lattice definition of a progression (see DESIGN-vetula-workflows.md for the
states/objects/workflows this grew out of). Written to implement *against*, not to
improvise into `App.purs`.

**Implemented so far (A–E):**
- **A** — `Vetula.Tank` (`SpecimenId`/`Provenance`/`Specimen`/`specNotes`); tank state;
  `k` (or shift-click a pool chord) catches into the tank; tank strip with ×=delete.
- **B** — stage seeds: click a tank tile injects it as a centre chord and blooms its
  extensions (`spawn Extend`); toggle to unstage; `ClearStage`; staged tiles gold-framed.
- **C** — the lens frame: `StageLens` ADT + `allLenses` registry + `lensBar` selector;
  `surface` dispatches to `keyboardSurface` (the cloud) or `padGridSurface` (a sparse
  playable 4×4 board of the tank). Adding a lens = one constructor + branch + entry.
- **D** — sequence from the tank: shift-click a tank tile appends the chord as an
  `imported` (off-lattice, stable-id) snapshot to `path` — so the progression survives
  every context change (the original bug). Pool no longer builds progressions.
- **E** — transpose: per-tile ♭/♯ + whole-tank capo (`transposeSpecimen`, in-place).

**Current tank tile gestures:** plain-click = stage a seed · shift-click = append to
progression · ♭/♯ = transpose · × = delete. Pad grid: plain = play, shift = stage.

**Next session = styling & layout pass** (AC 2026-07-06): test the new workflow, refine
the tank/pool/rail layout, tile visual language, lens bar. Then: tank dedup (#98), a
concrete F lens, formal `Step`/`Sequence` types + full PathPick removal.

## Why this exists — the shift

Today Vetula has two things fused together: a **generated field** (key/scale/borrow →
the whole lattice in the pool) and a **path** (the chords you clicked, stored as lattice
*node ids*). Storing a progression as node-ids into a physics surface that reflows,
re-keys, and regenerates is the root of the "progressions are buggy w.r.t. display
changes" problem. And forcing every outside chord to enter by *reshaping the context*
(family reflavour, borrow-modes) is what makes the tool feel prescriptive.

The fix is to separate the **durable** from the **volatile**, into **three surfaces**:

```
┌ STAGE (centre) — transient bench ───────────┐   ┌ RAIL (accordion) ┐
│   a swappable LENS renders candidate chords  │   │  Scale (context) │
│   around 0..N seed specimens; audition,       │   │  Sequence        │
│   revoice, generate. Nothing here is precious.│   │  Library         │
│                                               │   │  Voices          │
└───────────────────────────────────────────────┘   └──────────────────┘
┌ TANK (bottom or LHS) — persistent, unordered ─────────────────────────┐
│  the specimens you've CAUGHT. ×=delete · click=stage · shift=sequence   │
│  · transpose. Persists until cleared; feeds many sequences.            │
└────────────────────────────────────────────────────────────────────────┘
```

- **Durable** = the **Tank** (a collection of frozen voiced chords) and the **Sequences**
  built from it. These never reference lattice nodes.
- **Volatile** = the **Stage** (a viewport that generates and auditions). It works on
  copies; the only way something leaves the stage alive is by being **caught** into the tank.

## Data model

Sketches, PureScript-flavoured (newtypes for domain ids, ADTs for closed alternatives,
per house style). Absolute MIDI everywhere so transposition is arithmetic.

```purescript
-- A frozen, voiced chord — the atom the whole tool now trades in.
newtype SpecimenId = SpecimenId Int      -- opaque, minted on catch; NOT a lattice id

type Specimen =
  { id         :: SpecimenId
  , voicing    :: Array Int      -- absolute MIDI, the actual sounding upper notes
  , bass       :: Int            -- absolute MIDI of the foot (own line)
  , label      :: String         -- descriptive display name ("Cmaj7", "F#m/A") — informational
  , provenance :: Provenance     -- where it came from — descriptive, not prescriptive
  }
  -- derived when needed: pcs = nub (map (_ `mod` 12) (bass : voicing))

-- Honest record of origin, so a specimen can say "I am an F# phrygian iv that AC caught",
-- without that ever constraining what it can sit next to. Pure metadata.
data Provenance
  = FromLens String Key        -- caught from lens <name> in harmonic context <key>
  | Transposed SpecimenId Int  -- a capo/shift of another specimen
  | Hand                       -- entered directly
  | Imported                   -- restored from a saved sequence/library

-- The persistent collection. Conceptually unordered; insertion order kept for stable
-- rendering. Survives every context change; cleared only on demand.
type Tank = Array Specimen

-- A candidate is a chord a lens is PROPOSING — same voiced shape, not yet caught.
-- Catching freezes a Candidate into a Specimen (mints an id, records provenance).
type Candidate = { voicing :: Array Int, bass :: Int, label :: String }

-- Fork #3 resolved: a sequence holds its OWN snapshots (copies), so transposing a tank
-- specimen never retroactively rewrites a progression. One tank → many independent seqs.
type Step = { from :: SpecimenId, chord :: Candidate }   -- snapshot + a back-pointer
type Sequence = { name :: Maybe String, steps :: Array Step }
```

Notes:
- `Specimen.voicing` + `bass` is exactly today's `ChordNode.voicing` + `bassPc*` content,
  lifted off the lattice. The revoice modal, `chordGlyph`, ladder, and the reef realiser
  all already consume "bass + upper voicing", so they port with minimal change.
- `transpose :: Int -> Specimen -> Specimen` shifts `voicing`, `bass`, relabels, and
  records `Transposed`. Trivial because everything is absolute MIDI.

## The Lens decoupling (explicit)

**The keyboard-of-triads is one lens, not the pool.** The Stage is a frame; the *view*
inside it is pluggable and enumerable. We don't know the full set yet, so the abstraction
must let lenses be added without touching the stage.

```purescript
-- A lens turns the current stage situation into something to look at and catch from.
-- StageContext is what every lens gets; a lens decides what to show and how to lay it out.
type StageContext =
  { seeds   :: Array Specimen   -- 0..N specimens dropped in as foci/origins
  , key     :: Key              -- the ambient harmonic context (Scale pane)
  , borrow  :: Maybe Mode       -- optional parallel-mode colour
  }

-- Each lens owns its own generation AND layout AND rendering; it only has to speak the
-- Stage's action vocabulary (audition / catch / seed). Registry is just `Array Lens`.
type Lens =
  { id     :: String
  , label  :: String
  , needs  :: LensBackend                       -- Static | Simulated (force layout)
  , render :: StageContext -> H.ComponentHTML Action Slots m   -- candidates + layout
  }

data LensBackend = Static | Simulated
```

Lens candidates (not exhaustive — the point is it's open-ended):

| Lens | What it shows | Backend |
|---|---|---|
| **KeyboardTriads** | today's keyboard + diatonic triad families on each root | Static |
| **Lattice** | voice-leading Hasse web around the seed(s) | Simulated |
| **Extensions** | grow a seed: 7 · 9 · 11 · 13 · sus | Static ladder |
| **ChromaticCircle** | 12-o'clock pitch wheel; chords as polygons (cf pitch-polygon idea) | Static |
| **RegisterScatter** | candidates plotted by register × tension | Static |
| **Neighbours** | just the one-note-different neighbours of a seed, audition-ranked | Static |
| **McMullen / Borrow shelf** | curated exterior sets | Static |
| **PadGrid** | a 4×4 (or N×M) grid of chords — from the tank or a recipe — each cell bound to a hardware pad | Static + pads |

Implementation wrinkle to settle in the slice, not here: `Simulated` lenses (Lattice) need
the force-sim handle, which is stateful app-level. Options — keep one sim owned by the
Stage and only `Lattice` uses it; or lenses that need it declare `needs = Simulated` and
the Stage wires the handle. Static lenses are pure `StageContext -> HTML`.

## The three surfaces & their jobs

- **Stage** owns: the active `Lens`, the `seeds`, a sim handle (for simulated lenses),
  transient audition state. Emits: `Audition c`, `Catch c` (→ tank), `SeedFrom specimenId`.
  A lens-selector (tabs/dropdown) switches the view; **clearing the stage** empties seeds.
- **Tank** owns: `Array Specimen` + selection. Renders via a **tank layout** (itself a
  lens family — grid first; later chromatic/register arrangements). Emits: `DeleteSpec`,
  `StageSpec` (drop onto stage as a seed), `ToggleInSeq`, transpose actions.
- **Sequence** (the rail's Progression section) owns: the ordered `steps`. Emits reorder,
  per-step revoice (mutates the snapshot only), play. Voices read the sequence's chords —
  the same interface today's `path` presents to the reef realiser, so Voices are unchanged.

## Interaction grammar (the verbs)

- **Catch** — from a lens candidate on the Stage, freeze it into the Tank (`f`/a catch
  button). The heartbeat of the loop. (Re-points today's `f`-keep at the tank.)
- **Stage / unstage** — click a Tank specimen to drop it on the Stage as a seed (audition
  + generate around it); click again to remove it as a seed. Staging is the *default* click.
- **Sequence** — shift-click a Tank specimen to append a snapshot to the current Sequence.
  Staged-ness and sequenced-ness are **orthogonal** (a specimen can be neither/either/both).
- **Transpose** — per-specimen, per-selection, or a whole-Tank **capo** (`+/- semitone`).
  Sequences already-built are unaffected (they hold snapshots).
- **Revoice** — on a staged chord (bench) or a sequence step; the shared Modal widget.
  On the bench, revoice edits a *copy*; **keep** commits the refined specimen back to the tank
  (so three voicings of one chord = three specimens).
- **Delete** — × on a Tank specimen.

## What's reused vs new

- **Reused, ported off the lattice**: `diatonicTriads`, `latticeChild`, `mcmullenChords`,
  extensions/suspensions, `revoiceModal`, `chordGlyph`, the ladder, the pitch-class colour,
  the reef realiser + Voices. These become lens internals + specimen renderers.
- **New**: `Specimen`/`Tank`/`Sequence` types + state; the tank grid UI + its verbs; the
  `Lens` frame + selector; transpose ops; the Sequence-as-snapshots rewrite of `path`.
- **Retired**: `path :: Array Int` (lattice-id references); the first-pick/PathPick machinery;
  family-reflavour picker (mechanism can stay for the Borrow lens). `focus`/poolSpine already
  dormant.

## Build slices (each builds + is auditionable)

- **A — Tank + Specimen, alongside the old world.** Add the types + state + a bottom/LHS
  grid. A **catch** gesture on the *existing* lattice adds a Specimen to the tank; ×=delete;
  click=audition. Old progression still works. *Proves the durable store + catch loop.*
- **B — Stage seeds from the tank.** Clear-stage + drop 1–2 tank specimens as seeds; the
  existing lattice/keyboard regenerates *around the seed(s)*. *Proves seed→generate.*
- **C — Lens frame.** Wrap keyboard-triads and lattice as two named `Lens` values behind a
  selector; Stage delegates render. *Proves the decoupling — adding a 3rd lens is additive.*
- **D — Sequence as snapshots.** Replace `path` with `Sequence` (shift-click to append
  snapshots); point Voices at the sequence. Retire PathPick. *Proves progressions survive
  context changes — the original bug, fixed.*
- **E — Transpose.** Per-specimen + whole-tank capo.
- **F+ — more lenses & tank layouts.** ChromaticCircle, RegisterScatter, Neighbours;
  browse/search projections of the tank. Open-ended.

## Playable lenses & control surfaces (idea, 2026-07-05)

**Lenses span a density axis.** At one end, exhaustive maps (the lattice — every
voice-leading neighbour, for *hunting*). At the other, sparse *playable* grids (a 4×4
of 16 curated chords, for *playing*). The tank is the durable reservoir; a lens is how
you pour it onto the transient stage at a chosen density. Keep the expansive views
(lattice) always available for exhaustive exploration, but most sessions live at the
sparse, playable end — the pool is an etch-a-sketch, precious nothing.

**PadGrid lens.** A 4×4 (or N×M) arrangement of chords, each cell bound to a hardware
pad — MidiFighter 3D, a region of a Push, a LaunchControl. Cells sourced two ways:
(a) 16 specimens straight from the tank, or (b) a **recipe** that generates 16 related
chords (the diatonic 7ths; neighbours of a seed; 16 voicings of one chord; a cycle).
Recipes are just Static lens generators — "many ways to seed, less full than a lattice."

**Why it matters — rhythm stays deferred.** Pressing pads auditions *orderings* (which
chord after which, in which voicing) without fixing time. That preserves the model's
spine: rhythm comes after progression, assigned later in the Voices / pattern layer.
You play possible sequences by hand, committing nothing; the ones you like get captured
as a Sequence (Slice D). Mouse shift-click and hardware pads become two inputs to the
same "commit an ordering" verb. (Optional later switch: capture pad-press *timing* as a
first rhythm sketch — off by default, since it deliberately breaks the deferral.)

**Architectural implications.**
- *MIDI input is a new capability.* Vetula only has `Midi.MidiOut` today; a control
  surface needs Web MIDI *input*. Small spike, but it doesn't exist yet.
- *The `Lens` type grows a pad facet.* A grid lens declares a cell→note map per
  controller layout (`PadMap`), alongside its `render`. Composes with the Slice C lens
  frame rather than fighting it.
- *This is the performance path from tank → Sequence.* The pad grid closes the loop
  between the durable reservoir and the ordered sequence, by hand, in real time.

## Known issue — the tank admits duplicates (AC 2026-07-06, to fix)

Catch the same chord twice and it lands twice. Wanted: catching an
already-present chord is a no-op. Two wrinkles to get right:

- **Dedup keys on CONTENT, not id.** Every catch mints a fresh `SpecimenId`, so a
  `Set Specimen` keyed on the record (or the id) never sees a dup. Dedup must key on
  the musical content — `bass` + sorted `voicing` (the existing `contentKey` helper
  already does this for lattice nodes). "Ensure the `Eq` for a chord takes voicings
  into account" = this content key.
- **`Set` would lose insertion order.** The spec chose `Tank = Array` for
  "insertion order kept for stable rendering"; a `Data.Set` (needs `Ord`) reorders the
  tiles by content-key. Likely cleanest: keep the `Array`, dedup at catch time (reject
  a catch whose content-key is already present). If a real `Set` is wanted, add an
  `Ord Specimen` on the content-key and accept content-sorted tile order.

## Little ideas (parking lot, AC 2026-07-05)

- **Keyboard-as-drawer.** Put the piano keyboard at the top, under the tank, and let it
  *slide out of the tank* like a drawer when needed — reclaiming vertical space when the
  active lens doesn't want a keyboard.
- **"Show all extensions" button.** Dispense with the progressive number/`e`-key stacking
  (2·3·4… climb) in favour of one button that lays out *all* the extensions of a chord at
  once. Simpler, less theory-exposing than the incremental climb; folds naturally into an
  Extensions lens (the seed-bloom already generates them — just show the whole family).

## Deferred / open

- **Persistence.** Tank is in-memory, session-lived (cleared on demand). Library still saves
  **sequences**; saving a **tank/palette** is a later want (#90 lands here — a "scene" =
  a tank + a sequence).
- **Layout of the three surfaces** (tank bottom vs LHS; how the Scale pane folds in) — decide
  when B/C make the spatial needs concrete.
- **Search lenses** (filter-by-tone, nearness, brighter/darker) — after the browse lenses.
- **Function-colour** (degree-in-scale hue) vs today's pitch-class colour — revisit as a lens
  option, not a global.
