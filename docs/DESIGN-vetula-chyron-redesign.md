# Vetula redesign-2 — the audition chyron and what it dissolves

**Status:** design, 2026-08-01. Follows `DESIGN-vetula-progression-building.md`
(the three-view redesign, all built). This doc is the *second* redesign, born
from a hands-on shakedown session auditioning through Continuo (Piano One + LABS
strings → ADAM monitors). Source snags/ideas: the session scratch list.

## The core realization

The three-view redesign gave Vetula good *surfaces* to explore harmony on. The
shakedown exposed that the **progression-building model underneath them is
wrong**. Today you build a progression by **shift-clicking a caught chord to
drop it into the progression, bridged** (`ArrangeSpec` → `Vetula.Between`
inserts `bridgeLen` cadence chords in front of it). Observed failure: the
progression grew **1 → 4 → 6** as bridge chords piled in, with no way to tell
anchors from bridges. That's snag #1, and it isn't a bug to patch — it's a
signal that *deciding to build a progression up front* is the wrong frame.

The fix is a single new primitive that **all** the good ideas from the session
turn out to be facets of:

### The audition chyron — a rolling harmonic capture buffer

A ticker along the bottom of the screen (ABOVE the output-destinations bar) that
**logs every audition, in order**, scrolling one notch per new chord (not
constant drift, not scrolling-away). Always on, in every view.

You bang on chords — on the fifths ring, the Tonnetz, the lattice, the
grow/explore bloom — and the trace accumulates.

> **BUILT (commit below, 2026-08-01):** `chyronArmed` (default true) guards
> `logChyron`; a ●/○ record toggle at the left of the bar (red ● = capturing,
> ○ = paused, auditions still sound). Default-armed keeps the always-on flow AC
> liked; disarm to explore off the record. Verified: disarmed → played chords
> don't log.
>
> **Revision (AC 2026-08-01, from playing with it):** "always recording" was too
> much — it sweeps up exploratory noodling and wrong turns. The chyron wants a
> **record-arm** toggle (capture *when you're ready*), while STILL permitting
> "leave record enabled" (always-on becomes a *choice*, not forced). Small
> Phase-1.5 refinement: a `chyronArmed :: Boolean` guarding `logChyron`, a
> transport-style record button + a "recording" indicator on the bar. Fork:
> default armed (keeps the retroactive "capture what I just played" feel) vs
> disarmed. Nice option: keep a short **pre-roll** capturing even when disarmed,
> so arming can retroactively grab the last few — honours both "record when
> ready" and "never miss a take." A progression is then something you **lift retroactively** from a
span of the trace, not something you assemble chord-by-chord in advance. The
connective tissue (cadence bridge / voice-leading) is applied **at lift time**,
to a run you already like the sound of — which is also where the long-parked
*cadence-vs-voice-leading* decision finally lives (it becomes a per-lift choice,
not a global mode).

This is the direct-manipulation thesis (CLAUDE.md) applied to composition:
**operate on the trace of what you did, not a blank score.**

## What the chyron absorbs

| Session item | How the chyron subsumes it |
|---|---|
| #1 bridge-on-catch grows 1→4→6 | Retired. No bridging on catch; you lift a span and bridge at lift. |
| #7 rename "grow" → "explore" | Part of the same pass; grow becomes a *jam surface* whose output is the trace. |
| #8 grow-view progression-gathering | The trace IS the path you walk through the satellite blooms. |
| #12 the chyron itself | The keystone. |
| #13 split Tank / Progression panels | Falls out: tank = caught-anchor pool; progression = a lifted span. Different lifecycles → different panels. |
| #14 collections of progressions | A saved unit becomes `{ pool, named progressions[] }` — many lifts off one pool. |

The remaining session items are independent of the chyron and ride along:

- **#2** ring orderings (chromatic *and* circle-of-fifths) + speculative spiral.
- **#3** Tonnetz register — pure pitch-class (clean modal-family shapes) vs a
  pitch-space/spiral variant with real octaves. Ties to #2's spiral.
- **#4/#9** unhide the cascade `Select`; reconcile the published widget package
  name (`halogen-ui` vs the pinned `halogen-widgets: 0.2.0`).
- **#5** grey out inert color layers (borrowed/McMullen/Butler/Stock) in the
  triads-only Tonnetz view.
- **#6** a persistent mini-Tonnetz in the selector panel using the modal-family
  shapes as an interface element.
- **#10** surface Harmonia's full 21-member `Mode` ADT (diatonic +
  harmonic-minor + melodic-minor modes) + hexatonic catalog in the scale picker.
- **#11** quartal harmony: `Harmonia.Voicing.quartal` already exists; a quartal
  "Tonnetz" collapses to the line/circle of fifths (P4 ≡ inverted P5), so it's a
  *reading* of the fifths axis, not a new lattice. Optional quartal-neighbor
  bloom in explore.

## Data model

Current relevant state (`src/Vetula/App.purs`):

- `tank :: Array Specimen` — the durable, unordered pool of caught chords.
- `path :: Array Int` — the current progression as chord ids on the live surface.
- `bridgeLen :: Int` — the cadence-length dial (0..`maxBridge`), consumed by
  `ArrangeSpec` via `Vetula.Between.bridgeNotes`.
- Audition choke-points: `playChord :: ChordNode -> …` and
  `playSpecimen :: Specimen -> …` (both schedule `playNotes`/`specNotes` on
  `previewChan`). `playPath` replays an existing path — must NOT re-log.

New state:

```purescript
-- one audition event: what sounded, when, and enough to re-sound + label it.
type ChyronEvent =
  { pcs   :: Array Int      -- pitch-class set (for glyph + dedup)
  , notes :: Array Int      -- absolute MIDI (for exact replay)
  , label :: String         -- chord name as shown
  , at    :: Number         -- capture time (ms, from Date.now via FFI/Now)
  }

-- the rolling buffer + the current highlight span.
, chyron     :: Array ChyronEvent   -- append-only within a session; capped length
, chyronSpan :: Maybe { lo :: Int, hi :: Int }   -- highlighted [lo,hi] indices
```

`playChord` and `playSpecimen` gain one line: append a `ChyronEvent`. That is
the entire capture mechanism — because those two functions are the only
single-chord audition paths, everything auditioned anywhere lands in the trace
for free.

### Lift targets (the three verbs off a highlighted span)

1. **→ tank** — every distinct chord in the span becomes a `Specimen` (loose
   anchors). De-duped by pcs.
2. **→ progression, ordinal** — the span in order, timing dropped ("de-quantise").
3. **→ progression, timed** — the span in order, keeping captured inter-onset
   gaps ("re-quantise" snaps those gaps to a grid; a third state is
   timeless-ordinal). Three timing states: *captured-free-time ↔ grid-snapped ↔
   timeless-ordinal*, two verbs (de-quantise / re-quantise) moving between them.

### Loop

A highlighted span with timing can **loop** (`playPath`-like, but scheduled to
repeat) so you can leave a run playing and audition *over* it — the natural way
to test a betweening flavor by ear.

## Chyron interaction spec (Phases 2–3) — resolved 2026-08-01

**Chip content — DECIDED: the Tank's mini stave-glyph**, not text. Name-free
(the interesting chords are the hard-to-name ones), same visual language as the
tank you lift *into*, narrower than a spelled-out pitch-set. Reuse
`chordGlyph [] 0 0 ev.notes` in a small `SE.svg` exactly like `specimenTile`.
Name/pcs live in a hover tooltip.

**Gesture model on the chyron:**

| Gesture | Effect |
|---|---|
| hover + `space` (pointer over a chip) | audition that chord — **no re-log** (a no-log audition path; avoids feedback) |
| hover a selection + `space` | audition the whole selection, with its **original captured timing** (from `at`) |
| click | set one selection endpoint; the **next click** completes the span; a further click starts fresh |
| shift-click a chip | add that chord to the **Tank** (as a `Specimen`, like `CatchNode`) |
| shift-click **inside the selection** | **lift the selection as a progression** |

Selection state: `chyronSel :: Maybe {lo,hi}` — `lo==hi` is a pending single
endpoint; a second click sets the span; a third click resets to a new endpoint.
Hover state: `hoveredChyron :: Maybe Int`.

**Lift-to-progression — DECIDED: every lifted progression is NAMED.** The name
**auto-generates** (from the chords, or a glyph-name) and is editable, so it's
not a friction modal — accept the default and move on. It is saved to the
library AND loaded as current. Losing a *transient unnamed* current is fine —
the durable unit is always a named progression (this is exactly the shape
collections #14 will save). No separate "protect the unnamed current" logic.

**Phasing within 2–3:**
- **2a** — glyph chips; `hoveredChyron`; no-log audition path; `space` auditions
  the hovered chip; click-selection; shift-click → Tank.
- **2b** — `space` over a selection plays it with original timing;
  shift-click-in-selection → lift as an auto-named, saved, loaded progression;
  **remove `ArrangeSpec`'s auto-bridge** (snag #1 retired). Bridging moves to a
  lift-time option.
- **3** — de-quantise / re-quantise verbs on a lifted progression; span loop.

## Panels (snag #13)

Split the current unified "TANK & PROGRESSION" panel:

- **Tank** — stays where it is (top-left family), collapses UP.
- **Progression** — becomes a **bottom-left floating panel**, collapsible but
  **collapsing DOWN**. Build collapse-direction as a *parameter* on one reusable
  panel widget and **extract it to `halogen-ui`** (do this alongside #4/#9 so
  the package touch is one pass, not three).

## BUILT — save-to-token + hybrid pinned layout (2026-08-01)

The chyron-as-ledger, first slice. A selected span → **⏎ save** (Enter key or a
gold button that appears only while a span is selected) → compresses into a
pinned **2-glyph token** (`glyphOf` over the span's canonical pc-content → the
identity glyph-pair, rendered as the FA icon pair — visually distinct from the
live stave-glyphs, so "named unit" reads at a glance). Saving REMOVES those
events from the live trace (the compression reclaims space). `SavedSeq` carries
the full events, so a token replays with timing (click it) and can later
`split` into (Progression, Timings). × deletes a token.

**Hybrid single-line layout:** `[AUDITION | ⏎save/clear] [saved tokens →]
[…live chips → newest]`. Saved tokens pinned left (natural width, accumulate
rightward); live region `flex:1 min-width:0` shrinks as saved grows (unsaved
chips clip off its left). ~10–15 tokens fit before you run out of live room —
enough for a working set; overflow home is collections / the between-sessions
modal. Abandons "grow up into the display" (keeps the thin single line + the
composition surface fully intact).

**Two-bar fallback (AC, if the single line ever feels tight):** move the saved
tokens to their OWN thin bar ABOVE the chyron — more room, still bounded, still
no growing up the screen. The saved region is already its own flex child, so
this is a move, not a rewrite.

Also: `playChyronSelection` refactored onto a shared `playEvents` (block chords —
all notes of a chord together, no per-note roll; that roll had read as an
unwanted arpeggio on playback vs the block chords heard live).

Still TODO in 2b: shift-click INSIDE a span as an alt save gesture; remove
`ArrangeSpec`'s auto-bridge (retire snag #1); typed name / collections tier.

## Progression panel rendering (AC 2026-08-01 — do with 2b/4)

Render progression steps with the **same compact glyph-chip** the chyron uses —
the mini stave-glyph is small enough to fit several steps across a row, far
denser than today's per-step dot-matrix. Reserve the larger per-step view for
**re-voicing**, or drop it. This unifies the visual language: the chyron trace,
the tank, and the progression all read as the same glyph.

## Collections (snag #14 — deferred)

Once lifts are cheap, one chord pool spawns many progressions. The saved unit
becomes `{ pool (= tank), named progressions[] }` — the shape of song form
(verse / pre-chorus / chorus / bridge / coda reuse the same chords in different
orders). Kin to the already-built **scene grid** (sections ≈ scenes) and the
richer target for the **library/preset tier + between-sessions modal**
(task #16). Compose it from tank+chyron+preset machinery; don't build a silo.
**Not now** — bridge too far mid-refactor.

## Implementation phases

Ordered so each phase is independently buildable, committable, and verifiable in
the browser. The chyron substrate first because everything hangs off it.

- **Phase 0 — rename.** `LensGenerate` label "grow" → "explore" (+ any user-
  facing "grow" strings). Trivial, isolates the rename from behavior. (#7)
- **Phase 1 — chyron substrate (keystone).** `ChyronEvent`, `chyron` state,
  capture in `playChord`/`playSpecimen`, a `Now`/`Date.now` FFI for `at`, the
  bottom ticker render (above the output bar; scroll one notch per event; cap
  length). Read-only: it just shows what you played. (#12, part)
- **Phase 2 — highlight + lift.** `chyronSpan` selection on the ticker; the
  three lift verbs (→ tank / → progression ordinal / → progression timed);
  wire lift-time bridging (reuse `Vetula.Between`) so the old `ArrangeSpec`
  auto-bridge on catch is removed. Retires snag #1. (#12 + #1)
- **Phase 3 — timing + loop.** de-quantise / re-quantise verbs; span loop
  playback. (#12, timing axis)
- **Phase 4 — panel split.** Tank vs Progression panels; the collapse-DOWN
  floating panel widget, extracted to `halogen-ui`. Bundle with #4/#9 package
  reconcile. (#13)
- **Phase 5+ — ride-along view/theory items**, independently: #10 (Mode ADT),
  #11 (quartal), #2 (ring orderings), #5 (grey inert layers), #6 (mini-Tonnetz),
  #3 (Tonnetz register). Each its own small commit.
- **Deferred:** #14 collections.

## The chyron as LEDGER — saving = compression (AC 2026-08-01, "from swimming")

A bigger reframe that mostly *unifies pieces already built* rather than adding
new ones. The chyron stops being a ticker that scrolls into the void and becomes
the **composition ledger**: it grows UP, line by line, and **saving a run
compresses it to its 2-glyph token** with a single keystroke — the abstraction
becomes a spatial reward, and reclaimed space is the incentive to name.

- The **2-glyph token is the existing identity glyph-pair** (the preset-chip
  substrate, already built + rolled to Vetula). A saved progression IS a preset
  chip. So **the "Progression panel" dissolves into the chyron** — this REPLACES
  the Phase 2b/4 "lift into a separate panel" plan with "select run → keystroke →
  collapse to a token in place." Tokens already have downstream homes (macro-Tidal
  lanes + scene grid already speak glyph-tokens).
- Pairs with record-arm (#17) + a **Clear split**: **Clear unsaved** (prune
  noodling) vs **Clear all** (reset). Saving earns room; armed capture keeps the
  ledger intentional. These are the GOVERNORS that keep the ledger from eating
  the composition surface — the board must stay primary (bounded/scrollable
  ledger, compression as pressure-release), NOT unbounded growth.

**Functions over progressions — gated by the type system.** Transpose is the
only *universally* meaningful op: voice-leading / re-harmonization need a tonal
reading that borrowed / cross-scale / stacked-triad chords lack. But Harmonia's
`Anchor` already encodes this — `Located` (has a key reading) vs `Free`. So the
function menu gates itself: **universal** (transpose, retrograde, rotate) always
on; **tonal** (maximise voice-leading, re-cadence) light up only when every
chord is `Located`. Named, transformable units → deployable into the macro-Tidal
sequencer (their 2-glyph rep fits the lanes).

**Naming asymmetry:** an individual progression is **glyph-auto-named**
(recognized by sight); a **collection** takes a **typed mnemonic name**
(referred to — "verse-ideas"). Collections = the organizing tier (payload for
the between-sessions modal #16), so a growing library lives there, off the live
surface. ("Huge collection = user's problem" — fine.)

**Dispatch = material × realiser, on the transient/committed spine.** The
"send a progression somewhere to play" idea untangles into two axes:
- *Material* = progressions (tokens); *Realiser* = plays material in a voice
  (Odonus quantiser / MIDI / the new pad-arp-strum machine).
- **Sequencing** = material×realiser over TIME (scene grid / macro-Tidal);
  **quick-compare** = fire one material→realiser ONCE (no timeline).
- Same op, two time-scopes — the SAME transient↔committed spine the chyron
  already has (audition → lift): **drag a token onto a realiser = live-preview
  binding (transient); drop into a scene cell = commit to the sequence.**
- Routing UI: NOT a Sankey (that encodes quantity-flow); a **patch/assignment
  graph** (which token → which player) — i.e. the drawn routing-graph already
  parked in the routing FUTURE note. Drag-drop lands naturally there.

**Roadmap effect:** reshapes Phase 2b/4 → lift becomes compress-in-place, no
separate panel. Realiser layer = the bigger later move (the inversion below).
Nail the TIMING first (span playback) — "compress a run to a token" only feels
good if the run plays back cleanly.

## The realiser pipeline as types (AC 2026-08-01)

AC's formulation — the spine of the realiser layer:

```purescript
f :: Progression -> Timings -> TimedProgression       -- give it rhythm
g :: TimedProgression -> PlayerConfig -> OutputStream  -- give it a voice
-- e.g. C-D-G + [4,4,4] bars → 4 bars each; + [4,2,2] → 4 of C, 2 each D,G.
-- g arpeggiates / holds / strums / quantises Odonus.
```

Why it's right: it's the classic **content / time / voice** separation (what
notation and DAWs both rest on). C-D-G is pitch content; `Timings` is rhythm;
`PlayerConfig` is orchestration/articulation. Three composable stages.

It **absorbs the timing work** — Phase 3 isn't separate. `f`'s second arg IS the
de-quantise/re-quantise axis generalized: the ledger emits the ordinal
`Progression`; the three timing states (ordinal / captured-free-time /
grid-snapped) are values or sources of `Timings`; "4 bars each" / "4+2+2" are
hand-set ones. `Progression` = the saved 2-glyph token's payload (untimed).

Wrinkles that shape the design:
1. **`Timings` tempo-relative, not absolute** (Link/clock rig → cycles, not ms).
   Deeper: a `TimedProgression` is essentially a **Tidal `Pattern` of chords**,
   and the macro-Tidal engine already exists. DECISION: is `TimedProgression`
   its own type or literally a chord `Pattern`? If `Pattern`, `g` + downstream
   inherit the Tidal machinery for free.
2. **Split realization from routing** — don't fold both into the sound-producer:
   `g :: TimedProgression -> PlayerConfig -> EventStream` (musical: arp/held/
   strum/quantise) and `route :: EventStream -> Sink -> Effect Unit` (MIDI ch /
   Odonus / string machine). Lets the same realization hit two sinks, or one
   TimedProgression be realized two ways to compare (the earlier A/B wish = vary
   one arg).
3. **Odonus breaks `OutputStream = notes`** — usefully. Arp/held/strum GENERATE
   notes; Odonus IS a generator (feed it the chord/scale, it makes its own
   rhythm). So for Odonus, `g` emits **quantiser-context changes** timed by the
   progression, not notes. → `EventStream` must be abstract over BOTH note events
   AND "set the harmonic context" events. Not `Array MidiNote`; a stream of typed
   musical events, some notes, some "constrain that other machine."
4. **"Drop a progression on a function" = a typed dataflow = ShapedSteer.**
   `Progression` token → Timings node → `TimedProgression` token → Player node →
   sound is nodes-are-computations / edges-are-typed-dependencies. AC already has
   that workbench. FORK: bespoke Vetula drag-drop vs borrow the ShapedSteer
   pattern — but same shape confirms the factoring.

Two decisions shape everything downstream: **is `TimedProgression` a Tidal
`Pattern`** (reuse the engine), and **is `EventStream` abstract over
notes-vs-context** (so Odonus sits alongside arps).

### Capture already yields a TimedProgression — split gives a groove pool (AC 2026-08-01)

The loop closes: `ChyronEvent.at` means **a captured span is already a
`TimedProgression`** (chords + timing), implicitly. So the ledger's primary
product is `TimedProgression`; the ordinal `Progression` is a *projection*. The
algebra:

```purescript
capture  :: … -> TimedProgression                     -- at-deltas ARE Timings, for free
quantise :: TimedProgression -> TimedProgression       -- probably necessary; keep BOTH raw + snapped
split    :: TimedProgression -> (Progression, Timings) -- keep both, deploy separately
f        :: Progression -> Timings -> TimedProgression  -- recombine, any × any
```

Consequences:
- **`Timings` becomes a first-class saveable token — a GROOVE.** Lift the rhythm
  of a take off its chords and apply it to a different progression, or vice
  versa. Harmony and rhythm = independently reusable materials. (Timings wants
  its own glyph/identity + a place in collections alongside progressions.)
- **A groove pool, for free.** Un-quantised `at` is the *feel* (rubato,
  hesitations) — extracting it is exactly Ableton's Groove Pool / MPC-swing.
  Quantise for the grid; keep the raw as the "human" version; both savable.
- **Reinforces record-arm (#17):** raw `at` is only a *groove* if played
  deliberately — noodling gaps are thinking-time, not rhythm. Armed capture is
  what makes the extracted `Timings` musically meaningful. Record-arm turns `at`
  from a timestamp into music.

split/recombine is a clean little algebra → the idea is provably free of
internal contradictions (AC's confidence check).

**Keep duplicate chords — they're the rhythm (AC 2026-08-01).** Since we capture
timing from the user, ACCEPT consecutive duplicate chords (C-C-C-G-G): the
repeats ARE a comping/strum rhythm, not redundancy. Principle: **capture
lossless, project lossy.** The raw `TimedProgression` is the source of truth
(every hit); `split` recovers the harmonic `Progression` by collapsing
**consecutive** duplicates (run-length, NOT global — C-G-C keeps both Cs), while
`Timings` retains the full hit-pattern → the extracted groove is a strum/comping
pattern, not just change-points (bank a comp, apply to new harmony).
- Reinforces `TimedProgression` ≈ Tidal `Pattern`: with duplicates, `Timings` is
  a pattern of *references* into the deduped chord-set + onsets — `"c c c g g"`
  over `{c,g}` — which the macro-Tidal engine eats directly.
- **Retroactively settles the early "consecutive-dedup the chyron?" question:
  NO — never dedup at capture.** The current chyron already appends every
  audition (lossless), so no change is needed.
- Flag for `split`: "same chord" = same pcs (dedup harmony) or exact voicing (a
  re-voice may be intentional)? Lean pcs for the Progression reading, keep
  voicing in the capture. Decide when building `split`.

## The PERFORM view — the realiser pipeline as a Vetula lens (AC 2026-08-01)

AC's vision: a **4th lens** beside Fifths / Tonnetz / Lattice — **Perform**. NOT a
new machine; a performance surface always available in Vetula. This is the
realiser pipeline (`f`/`g`) as a direct-manipulation board, and scoping it as a
`StageLens` collapses the earlier "new machine + inter-machine routing"
complexity — it lives where the material is made.

- **A box = a player** = `g`'s realiser + `route`'s sink, fused. Drag a saved
  **sequence glyph onto a box** → the glyph sits on top and the voice plays.
- **Function stack on a box = the pipeline, composed visually** (functions stack;
  maybe dragged out of a palette onto the box/voice). transpose (universal op on
  the Progression), arpeggiate/strum/hold (`g`'s `PlayerConfig` — realization),
  retime/quantise (`f`'s `Timings`). Reads `token |> transpose 3 |> retime |>
  strum → sink`. Very Tidal (`#`-chain), very FP; ORDER matters.
- "Blurs routing responsibilities" — deliberately: this IS live-performance
  routing (material → output) with transforms on it.

Structural decisions as it forms:
1. **Boxes = the rig's real outputs.** Seed from `Triggerfish.Rig`/RigConfig
   (MIDI channels, Odonus, ES-9/FH-2 targets, future string machine). The Perform
   view IS the routing surface for the live case (subsumes the routing modal's
   live job).
2. **Output mode = the terminal `Sink` in the stack, not a box type** (AC refine
   2026-08-01). "Quantise (Odonus) vs play (MIDI)" is just another — the LAST —
   box: `render :: EventStream -> Sink -> Effect`, MIDI-play and Odonus-quantise
   are two `Sink` values. The huge functional gap (emit notes vs set the
   quantiser's harmonic context) lives inside those two `render`s; the
   performer's gesture stays uniform (operationally the same, functionally
   worlds apart — hide the machinery). Consequences:
   - **The terminal drives which upstream functions are live**: arp/strum/hold
     shape notes → inert above an Odonus sink (dim them). Cleaner than
     "box-type gates palette" — it falls out of the last element.
   - **A/B for free**: swap the terminal sink to re-point the same
     material+transforms MIDI↔Odonus in one gesture (the "arp vs Odonus"
     comparison wanted since the start).
   - **Reconcile with "boxes per output":** a box comes PRE-SEEDED with its
     natural terminal sink (dropping is instant), and that sink is also just the
     bottom of the stack (swappable). Immediate by default, composable when
     reached for.
3. **Drop = LOOP, not one-shot** (live performance): dropping starts the token
   looping through that player (tempo-relative, Link-locked); pulling it stops.
   Token's `at` gives the loop feel; a retime function overrides.
4. **A Perform configuration IS a scene** → closes the loop with the scene grid
   (#11): Perform view = live editing face of ONE scene; the scene grid sequences
   Perform configs over time. Also where Vetula's PLAY-role migrates — the bottom
   voice bar doesn't get deleted, it BECOMES this (AC's "rip play out of Vetula").

Type gating carries: transpose universal (borrowed/cross-scale/stacked fine);
voice-leading/re-cadence only light up for `Located` material — palette greys
itself honestly.

It's the ShapedSteer typed-dataflow (nodes=computations, edges=typed deps) as a
live board — the third appearance of this shape ⇒ factoring confirmed. **Build
guidance:** resist over-generalizing the function system early; start with a
fixed small palette (transpose / arp / strum / hold / retime) wired to the
existing `f`/`g`, let the drag-compose UI grow from there.

## Future direction — Vetula as a composition window, + realiser machines (AC 2026-08-01, NOT NOW)

The chyron makes something conceptually clear that was implicit: Vetula is
becoming a **live composition surface** whose product is *material* — chord
shapes and progressions — not sound. That invites an **architecture inversion**:

- Today Vetula both composes AND plays: it owns a voice bar that routes per-voice
  MIDI + an Odonus chord-quantiser feed. Playing is Vetula's job.
- Inverted: Vetula emits only **material** (the tank's chord shapes, lifted
  progressions). Downstream **realiser machines** *interpret* that material, each
  in its own voice. Vetula sheds the player role; the **output/voice bar gets
  ripped out** and playing is delegated.

The motivating new machine: a **Harmonia-driven string player** that takes a
chord-set / progression and makes its **own sequenceable decisions** —
arpeggiation, counterpoint, voice-leading, register, rhythm. "Sequenceable" is
load-bearing: those decisions are themselves patternable (they belong in the
macro-Tidal lanes + the scene grid), so the realiser is an instrument you
*perform*, not a fixed renderer.

Why it fits what's already here:
- Vetula's internal `Voice` type is already "its own read-head into the loaded
  progression" — this generalises that idea *across machines*: shared material,
  many read-heads.
- Harmonia already carries the realiser machinery — `Voicing` (incl. `quartal`),
  `Graded` anchors, `Voice`, `Quantise`.
- Odonus is already one consumer of Vetula's chord feed; a string-player is just
  another consumer of the same material. The chyron/progression becomes the
  shared score; machines become **realisers/voices** over it.
- **Two empty nav slots already exist** — Sufflamen and Stellatus are `— TBD —`
  in the shell today (both triggerfish genera, matching the naming). The new
  machine(s) can fill them.

Sequencing: **finish the chyron redesign (through Phase 4) before starting
this.** It's an architecture-level move (a new machine + retiring Vetula's voice
bar) and wants the composition surface settled first. Likely a new Marginalia
project when it begins.

## Open decisions (resolve as we reach them)

- **Chyron cap + persistence.** Session-only rolling buffer, or persisted? Start
  session-only, capped (say 128 events); persistence can come with collections.
- **Grid source for re-quantise.** System BPM (shell `bpm`) is the obvious grid.
- **Lift-time bridging default.** Cadence vs voice-leading as the default verb —
  the ear decision, now made *in situ* on a real span rather than abstractly.
- **Does the chyron go global** (all machines) eventually? Design it Vetula-local
  but keep the event type machine-agnostic.
