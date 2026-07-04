# Slice 4 — unify Lab + Performance into one surface

Slice 3 separated the three objects cleanly (pool → progression → playheads).
Slice 4 puts them on **one screen** so you never switch tabs to go from *building*
a progression to *performing* it — the tab-switch friction you flagged in Slice 1.

## The real problem: two progressions, not two layouts

The tabs aren't just two views — they hold two DIFFERENT progression models:

| | Lab | Performance |
|---|---|---|
| progression | `pathSteps st` (`st.path` : chord ids, resolved vs `st.chords`) | `perfChords st` (`st.perfProg.chordIds`) |
| built by | shift-clicking lattice chords | **loading a library entry** into a working copy |
| read by playheads | — | `perfChords` (the loaded copy) |

So today: build a path on Lab → save to library → switch to Performance → **load**
it → it fans to voices. Two hops and a copy. The playheads read the *loaded copy*,
not the thing you're building.

**Unification decision (proposed): the live path IS the progression the playheads
read.** Drop the separate load step. `perfChords` becomes `pathSteps` (one source of
truth). The library/stack (Slice 1) stops being a "load slot" and becomes what it
already half-is: **snapshots** — "restore" drops a stack entry back INTO the path
(the drag-back-to-explore gesture), it doesn't fork a parallel working copy.

This is the honest version of "pool → progression → playheads": one progression,
sourced from the lattice, snapshotted to the stack, read by the playheads.

## Layout (proposed — needs your eye)

One surface, top-to-bottom, no tabs:

```
┌──────────────────────────────────────────────────────────────┐
│ topNav: Vetula · KEY · SCALE · family · drops/borrow · MIDI   │
├──────────────────────────────────────────────────────────────┤
│ POOL — the lattice/cloud (collapsible "hunt" region)   [▸/▾]  │
│   collapsed: a thin strip / "＋ find chords" affordance        │
│   expanded : today's Lab cloud (focus beam, extensions,        │
│              McMullen, borrow, revoice modal)                  │
├──────────────────────────────────────────────────────────────┤
│ PROGRESSION — pathSteps as chord ladders + active-chord glow   │
│   (Slice 3's table; click to select, Tab/↑↓ revoice live)     │
├──────────────────────────────────────────────────────────────┤
│ PLAYHEADS — Slice 3's per-voice Tidal read-heads + commit      │
├──────────────────────────────────────────────────────────────┤
│ STACK — the auto-capture library (Slice 1), horizontal;        │
│   ★ keepers + ◦ ephemerals; click restores into the path       │
└──────────────────────────────────────────────────────────────┘
```

"The lattice **expands only when you're hunting chords** — killing the opening blank
canvas." Pragmatic reading for now: a **collapse/expand toggle** (hunt mode), NOT
full pan/zoom. True drag-pan/zoom of the lattice is explicitly *later* (the doc says
"deeper Lab-lattice ergonomics are later"). Collapsed-by-default when the path is
non-empty (you're performing); expands when you go hunting.

The **shape-library strip** (LHS movable-shape vocabulary) is **Slice 5**, not here —
it slots in as a second POOL source next to the lattice once this surface exists.

## Sub-slices (each builds + is testable)

- **4a — model unify.** `perfChords := pathSteps`; playheads/`buildPerf`/`brushMsg`/
  `AskHarmonic` read the live path. Library "load" → "restore into path". Remove the
  `perfProg` working-copy indirection. *Audio-testable* (build a path, hear the
  playheads read it with no load step). The riskiest change — do it first, in
  isolation, while the two-tab layout still stands, so it's verifiable before the
  layout moves.
- **4b — one surface.** Collapse the `Tab` split: render POOL + PROGRESSION +
  PLAYHEADS + STACK on one page (reusing Slice 3's renderers as sub-regions). Retire
  the Lab/Performance nav.
- **4c — hunt toggle.** Collapsible POOL region (state + toggle); collapsed default
  when the path is non-empty. The "kill the blank canvas" win.
- **Later** — the shape-library strip (Slice 5), colour = nearness/availability, true
  lattice pan/zoom, drag-back-to-explore semantics (explode a stack entry onto the
  lattice as editable nodes vs just re-arm the path).

## Why design-first here

4a rewires the progression source that Slice 3's playheads just started reading, and
4b/4c are layout the author has strong taste about and I can't see rendered. So: lock
the model decision (4a) and the layout shape (above) with Andrew, then implement 4a in
isolation (audio-verifiable), then compose the surface.

## Open questions for Andrew

1. **Model unify** — agree the live path IS the progression (drop the load-a-copy
   step; library becomes snapshots you restore into the path)? Or keep a distinct
   "loaded/committed progression" separate from the scratch path?
2. **Stack placement** — horizontal rail at the bottom, or a LHS column (leaving room
   for the Slice-5 shape strip)?
3. **Hunt default** — POOL collapsed whenever the path is non-empty, or a remembered
   manual toggle?
