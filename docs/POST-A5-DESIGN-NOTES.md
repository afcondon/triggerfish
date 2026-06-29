# Post-A5 design / UI notes

*AC's review after the Lepidoptera A-series landed (2026-06-29). Two items done
immediately; two flagged for discussion before building; the rest deferred.*

## Done this pass

- **Odonus — Source pane deleted.** The per-instrument eDSL SOURCE pane is gone;
  a patch's source now lives only on the shell's TIDAL page. `AskSource` /
  `patchText` still answer the aggregate, so nothing downstream changed. (Module
  `Triggerfish.Odonus.View.Source` removed.)
- **Tidal — two columns.** Column 1 = the LIBRARY manager; column 2 = the SOURCE
  aggregate, **full-length / uncapped** (read the whole rack if you want).

## No changes wanted

- **Balistes** — looks good as-is.
- **Selene** — no notes.

## Deferred

- **Vetula** — the whole page wants "Triggerfish brand restyling" (it still wears
  its standalone white-oracle look, not the Hainbach/Braun rack dress). **Not
  now** — a dedicated styling pass later.

## Open for discussion (do NOT build until talked through)

### Odonus — scenes become the recallable-preset store

AC: *"scenes needs re-thinking — isn't that where we should be saving our
recallable presets from?"* This answers the long-open
`scenes-vs-content-library = one mechanism or two` question: **ONE mechanism —
scenes ARE the Odonus preset library.**

Today scenes (`Scene = {name, odo}`) capture only the `Odonus` core, are
ephemeral (not persisted), and serve live performance (phase-preserving recall +
bar-boundary chaining). A3 added live-patch persistence; A5 exposes only the one
"live" entry to the manager. The proposed unification:

- **Scene captures the full `OdonusPatch`** (not just `odo`) — so a recalled
  scene restores the gen matrix / swing / velHumanize / stepDiv too.
- **Scenes persist** (Lepidoptera-serialised, via an extended Store), replacing
  the single-live-patch persistence A3 shipped.
- **A5 exposes scenes** as Odonus's library entries: `AskLibrary` returns the
  scenes, `LoadEntry i` recalls scene `i`, `ImportText` adds a scene.
- The live roles stay: **recall still phase-preserves** (carry the playhead
  cursor/seqPos/accumulator across an `applyPatch`), and **chaining** still
  auto-advances scenes at bar boundaries. Two entry points, one store: live
  recall (phase-preserving) vs a cold library load (hard reset) — same scenes.

Open question: is a cold load from the A5 manager phase-preserving or a hard
reset? (Proposed: hard reset from the manager, phase-preserving from the
in-instrument SCENES strip.)

### Odonus — Key pane re-order (AC has an idea; talk it through)

AC: *"the scale at the top is no longer king and has no role at all if Vetula is
providing the quantisation targets … I have an idea how we can reorganise this,
but we'll have to talk it through a bit."*

Framing (mine, as a starting point for AC's idea): under the single
`quantize :: PitchSource` model (scale | chords | vetula), the **pitch SOURCE is
the top-level choice**, not the scale. When the source is Vetula or a chord
progression, the scale plays no part in the final snap. So the Key pane's
hierarchy should flip:

- **Top = the pitch-source selector** (Scale · Chords · follow-a-Vetula-voice) —
  the new king (this is where the FOLLOW-VETULA pane already lives; fold it in).
- The **scale controls** (root / type / mask / SPREAD) become a sub-section that
  matters only when source = Scale — grey/collapse it when Vetula or chords drive.
- **Octave / scalar-transpose / distribution** apply regardless (they shape the
  chromatic knob-value *before* the snap), so they stay put.

This re-order is the natural moment to also land the **deferred `renderCell`
single-snap change** (no scale pre-snap before a chord/Vetula snap) — when the
source isn't the scale, the scale genuinely has no role, which is exactly the
re-order's premise. AC to share their own reorganisation idea before we build.
