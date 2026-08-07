# Milestone — 2026-08-07, going to the rig

The point at which Triggerfish stops being tested against itself and starts being
tested against the real modular rig with the full Atlantis group running.

Roughly **MVP − n, n < 10**.

Tagged `rig-test-2026-08-07` in both `triggerfish` and `reef`, since this run
spans both.

## What landed in this stretch

**Balistes — three brains, one surface.** GRIDS · RYTM · TIDAL all on screen at
once, the two inactive ones ghosted rather than hidden. Each band carries its own
live 2-glyph, so the machine a snapshot came from is encoded by POSITION rather
than by a badge. Snapshots are lane atoms; `laneScore` renders a macro-tidal lane
as glyphs, with unresolvable atoms shown as `name ?` rather than silently blank.

**The Euclid clock became one shared control.** `Triggerfish.Ui.Euclid` — click
to select, arrows to edit, ←/→ steps, ↑/↓ pulses, shift for a four-step stride.
Selene and Odonus are now the same instrument rather than two idioms; Odonus's
four corner ± clickers are gone. `ring` / `cell` / arithmetic are separable, and
the arithmetic comes in two shapes off one definition so a caller that transmits
RELATIVE edits (Odonus, under lockstep) can't drift from one that applies
absolute ones (Selene).

**The Euclid ceiling moved, 16 → 64.** Named `maxEsteps` in reef and READ by the
UI rather than copied. `esteps` is independent of `len`, so E(7, 24) against a
16-cell grid is now expressible — which is the point, not an accident. All 15
conformance goldens pass unchanged; raising a clamp is behaviour-preserving at or
below the old value.

**Vetula's Perform and Review surfaces.** The river runs full height from the
shell nav, with the CONTEXT and AUDITION bars squashed beside it (mirrors
Odonus); the Review seam is closed the same way; bar chips no longer break their
labels across two lines. Key and scale are now HUNT-only — see below.

## Watch for these on the rig

**Solo and Atlantis can disagree about Vetula's chords.** This is the one open
fault worth knowing before a rig session, because it presents as "it sounded
different through the rig."

Perform resolves chords two ways:

- `boxPattern` → local MIDI **and** the Odonus feed. Reads `box.seq.events`, a
  snapshot taken when the token was dropped.
- `buildPerf` → the rig. Reads `perfChords` = the LIVE path.

Anything that moves the path without moving the snapshot makes the two diverge.
The old key control did exactly that, which is part of why it's gone — but
removing the lever is not fixing the fault. If a Vetula voice sounds right in
Solo and wrong on Atlantis, look here first.

**A mixed Balistes lane has never been heard.** The mechanism is built and the
score renders, but nothing has confirmed by ear that a lane actually switches
machine as it plays. Needs the macro clock plus transport — a good rig-session
task.

**Odonus's Euclid rings go to 64 now.** Past about 40 steps the unfilled dots
merge into a band on an 82px ring. Legible, but you can no longer count it.

## Deliberately not built

- **Euclid chains and resets** (vpme.de Euclidean Circles). Planned in
  `DESIGN-euclid-chains-and-resets.md`. Chaining is sequential concatenation, so
  it is per-head and stays inside `step`'s `map` — cheap. Not exposed anywhere.
- **Transposition for Vetula.** Tidal-style over all voices, or MAPPED over
  selected ones. Both are pattern functions, so both keep the glyph identity
  honest — unlike the key control they replace. Would need applying at
  `boxPattern` AND `buildPerf`, or the two-sources fault bites again.
- **A large Odonus** (more knobs, pitches, steps). The other cluster of 16s —
  `cells`, `replicate16`, `len`, `offset`, and the Twister's 16 encoders mapping
  1:1 onto 16 cells. Structural, and unrelated to the Euclid ceiling.

## Still open, smaller

- RYTM's cell strip has no prompt when nothing is selected — an invisible feature.
- Odonus's PER CELL panel overflows by one knob row.
- Routing modal on launch (now a one-liner: make it the default route).
- Beat SESSIONS for Balistes, as a peer of Vetula's harmonic sessions.
- Glyphs vs number-names for drum patterns — an open experiment. `laneScore` is a
  rendering layer over stable aliases, so swapping is cheap.
