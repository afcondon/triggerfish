# Vetula progression-building — findings & redesign directions

*Design note, 2026-07-30. Marginalia #240 (Triggerfish), branch `macro-tidal`.
Written before a compaction; this is the grounding for a redesign discussion
(task #10). The trigger: verifying Vetula's preset chip required INJECTING a
library entry because there was no obvious way to build a progression by hand.*

## The core problem

Vetula is a rich harmonic **explorer** (Tonnetz lattice, chord families,
voicings, the pitch ladder, the TANK of caught chords, lenses:
keyboard/pad-grid/fifths/tonnetz/lattices/grow) **and** it has a **progression**
(the `path` — the ordered chords you perform). But **the bridge between
exploring and composing is missing**: there is no direct gesture to grow a
progression by picking chords on the primary (Tonnetz) surface — the one thing
the on-screen hint ("Shift-click chords on the lattice to grow a progression
here") literally tells you to do.

## How the `path` is actually built today (4 ways; none is "click the lattice")

Every mutation of `st.path` in `src/Vetula/App.purs`:

1. **`PathPick pid`** (handler at ~line 1107) — append a lattice chord to the
   path, **auto-bridged**: it runs `Path.shortestPath` over the Tonnetz web from
   the last path chord to the clicked one and inserts the intervening
   voice-leading chords; clicking the last chord again clears the path. This is
   clearly the *intended* progression-builder — **but it has NO UI binding.**
   Orphaned code.
2. **Lattice shift-click is bound to catch-to-TANK instead** — `CatchTriad` /
   `CatchNode` / `CatchChord` (onClick handlers at ~lines 3460, 3634, 3735,
   4448: plain = audition/play, shift = catch). So the primary surface builds
   the tank, not the path. This is what displaced `PathPick`.
3. **Generate flow** — shift-click existing progression **rows** (`StepClick i
   shift`, ~1287) selects 1–2 positions into `genSel`; the "grow" lens generates
   candidates; **`PickCandidate cid`** (~1308) inserts / substitutes / prepends /
   appends one into the path. Needs an existing path to select positions within.
4. **`LoadProg i`** (~1327, load a library entry) or **`LoadSource`** (paste
   Tidal `note "<[..] [..]>"` text and press Load). `parseProgression` +
   `importChord` (in `Vetula/Tidal.purs`) turn text → note-lists → chords.

## The two disconnected collections

- **TANK** (`st.tank :: Array Specimen`) — caught chords; state comment says it
  is "the durable, unordered collection … will feed the Stage + Sequences."
  Clicking a tank specimen (`seedChord`) injects it into the pool as a centre
  chord for **lattice exploration** — NOT into the path. So the tank is
  currently a **dead-end** w.r.t. composing a progression.
- **PATH** (`st.path :: Array Int`) — the progression (chord ids, in order).

Gather (tank) and arrange (path) never connect.

## Redesign directions (react to these)

- **A — Un-orphan `PathPick` (minimal restore):** re-bind lattice shift-click (or
  a dedicated modifier, or a "compose" mode) to append-to-path with the
  auto-bridging; move tank-catch to its own gesture (the `k` key already
  catches). Smallest fix; restores the intended flow.
- **B — Tank *is* the palette, path is arranged from it (most coherent):** catch
  chords freely (explore), then a gesture (drag / click-in-order) lays tank
  chords into the path. Makes the two collections one workflow: gather → arrange,
  and gives the tank a real job.
- **C — Explicit "compose mode":** a mode where every chord click builds the path
  (auto-bridged), distinct from an "explore mode" where clicks audition/catch.
  Removes the modifier ambiguity.
- **D — Something more dramatic** — AC is open to rethinking the whole metaphor
  (said so 2026-07-30 and earlier).

Claude's instinct: **B** (tank-as-palette) is the most coherent; **A** is the
cheap restore. Open question for AC: *what progression-building experience do you
want when you sit down with Vetula?* — answer that first, then pick the shape.

## Related

- The Vetula preset **chip** (capture/recall of the progression source) already
  works — see the session's `Roll the unified Preset chip to Vetula` commit. This
  note is only about **building** a progression, which the chip doesn't address.
- Vetula's LIBRARY (auto-capture stack) is the named tier, kept until the
  between-sessions modal exists (see `docs/DESIGN-scene-modal.md`).
