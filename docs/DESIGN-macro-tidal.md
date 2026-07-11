# macro-tidal — the arrangement layer, and the harmonic-authority decision

macro-tidal is a text arrangement language on the Triggerfish TIDAL page. Where
the instruments each play ONE saved form at a time, macro-tidal SEQUENCES those
forms over macro-time and TRANSFORMS them in place. It is the composition
altitude above the workbench (which curates and previews individual forms).

Proceeds directly from the Amphora load/save work: once forms (Odonus scenes,
Balistes beats, Vetula progressions, Selene racks) are named, content-addressed,
and easily swapped, the natural next step is to compose *with* them — and to do
it in Triggerfish's own idiom (Tidal-like control structures over saved forms)
rather than an Ableton-style swim-lane. Saved arrangements become Amphora
content too, which opens meta-macro (arrangements of arrangements).

## The language

Domain-AGNOSTIC by design: the parser (`Triggerfish.Macro`) yields `(verb, arg)`
string pairs and does not know what any verb means; the shell interprets each
verb per target instrument. This keeps one language reusable across every lane.

A lane is `step := form (# verb arg)*`:

- **steps** — space-separated, dividing the macro-cycle equally.
- **`~`** — a rest; the instrument falls silent for that step.
- **`<a b c>`** — alternation: a different form (or modifier arg) each macro-CYCLE.
- **`"quoted names"`** — a form name that contains spaces (Amphora labels do).
- **`# verb arg`** — a transform stack on the atom; the arg may itself be `<…>`.

```
"contemplative" # scale <"F# lydian dominant" "G major">
"bopping along" ~ <midnight drift>
```

Time is **bar-quantized**: a step is a whole number of bars (`bars/step`, default
4), tempo-relative off the shared free-run epoch. A form fills its step by
looping. (Rig-locked timing — reading the Link anchor instead of `freeTempo` —
is a deferred slice; Slice 1 drives the Solo/standalone case.)

### Built (branch `macro-tidal`, commit d107a42)

- `Triggerfish.Macro`: tokenizer (respects `<>` and `"…"`), `parseLane` (steps +
  `#`-mods), `resolveStep` (form + mods, cycle-resolved), `Cell = Quiet | Load
  name mods`, `laneFormNames`, `stepLabel`.
- `Main`: the Arrangement panel (Odonus lane) — lane input, `bars/step`,
  run/stop, live step readout, scenes palette; a `MacroTick` bar clock;
  `applyCell` loads + arms Odonus at step boundaries.
- `# scale`: re-quantise to a named scale via a new `SourceQuery.SetScale`
  (root pc + `Reef.Scale` scale-type), reusing Odonus's KEY-pane actions
  (`SetSource Scale` / `SetRoot` / `PickScale`) — the injected-realize seam.
  Balistes/Selene ignore it (no quantiser).

This merged the deferred *workbench-transforms* idea INTO the arrangement layer:
a transform is a modifier on a lane atom; `fast`/`bass`/`transpose` slot into the
same `applyMod` seam next. A bench transform control is just a live edit of a
lane modifier.

## The harmonic-authority decision (settled 2026-07-11)

Building `# scale` surfaced a dual-authority problem: Odonus owned a scale, but
so did the Vetula chord-follow and now the macro layer. The resolution is by
DELETION, not reconciliation:

- **Vetula is the SINGLE harmonic authority for the rig.** It emits a harmonic
  context (a PitchSet) at ALL times:
  - progression playing → the current chord/scale (exists: `FeedVoiceChords`);
  - no progression → the diatonic scale of Vetula's chosen key (`st.key` +
    `Mode`; `harmonia` owns `Key → PitchSet`).
- **Odonus (and every pitched voice) becomes a pure follower.** It already works
  in index-space with an injected `realize :: Index → Pitch` reconstructed from a
  PitchSet; that PitchSet now ALWAYS comes from Vetula. Odonus's KEY-pane scale/
  root ownership and its Scale/Chords/Vetula source radio go away; KEY becomes a
  read-only display of the inherited context.
- **`# scale` re-targets from Odonus to Vetula.** It sets Vetula's resting scale,
  so it becomes a rig-GLOBAL, instrument-independent harmonic-context verb —
  every pitched voice follows. The parser/grammar are unchanged; only the target
  moves (`SetScale` on Odonus → `SetRestingScale` on Vetula).

### Load semantics (the settled invariant)

- **Default load = always into Vetula's LIVE harmonic context.** A recalled
  Odonus scene follows whatever the rig's harmony currently is. There is no
  "Odonus scale" to restore.
- **The original sound is recoverable ONLY as a compound gesture:** load the
  Odonus scene AND simultaneously load its saved context into Vetula. Opt-in,
  never automatic — because the whole point of one authority is that you can play
  any line in any context; silent restore would throw that away.

### The provenance note

A scene keeps a POINTER to the harmonic context it was captured in — a note, not
a law. One datum with three faces:

- stored as Amphora's existing `label.harmonic { root, scale, chord }` (no new
  storage);
- the DEFAULT ARG of that scene's `# scale` transform (recover = `"scene" #
  scale <its-captured-context>`);
- the MIGRATED Odonus `scale:` field — under the new model it stops being
  authoritative playback data and becomes exactly this note.

Recall surfaces it as a chip: "captured in F# lydian → load into Vetula".

## Build slices

1. **V — Vetula emission. DONE (commit 4735716).** `AskContextScale` returns
   `{root, offsets}` (the key's diatonic set, or a `# scale` override held in new
   `restScale` state); `SetRestingScale` installs the override; picking a
   key/scale clears it. The shell polls it each `PollVetula` and, on change
   (deduped via `ctxScaleKey`), installs it as Odonus's `pitchSet` through the
   lockstep-safe `RI.SetPitchSet` (new `SourceQuery.SetContextPitchSet`).
2. **O — Odonus follower. DONE (commit 4735716).** `FeedVoiceChords` always
   adopts the Vetula feed (auto-follow first voice; empty → overlay off → just the
   scale); the source-radio gate is gone. `View.Key` rewritten read-only: a
   HARMONIC CONTEXT display (root + recognised scale + lit pitch-classes +
   firing/resting) reading the effective pitchSet.
3. **M — re-point `# scale`. DONE (commit 4735716).** `# scale` → Vetula
   `SetRestingScale` (the Reef scale's intervals); rig-global verb, grammar
   unchanged.
4. **P — provenance. TODO.** Capture writes `label.harmonic` (thread the field
   through `Triggerfish.Amphora` publish + fetch and Odonus's `PublishScene`);
   recall shows a "captured in X → load into Vetula" chip; compound restore =
   load scene + push its context to Vetula.
5. Follower scope: Odonus first (done); Stellatus / Sufflamen / future pitched
   voices adopt the same authority afterward. TODO.

## Try it (V/O/M)

- Odonus follows Vetula's key (C major default) — its KEY pane is a read-only
  HARMONIC CONTEXT display. Change Vetula's key/scale and Odonus re-quantises;
  play a Vetula progression routed to an odo voice and the chord overlay fires
  over the scale.
- `"scene" # scale <"F# lydian dominant" "G major">` in the macro lane now sets
  the whole rig's harmonic context per cycle (via Vetula), not an Odonus-local
  scale.

## Open / deferred

- Rig-locked macro timing (Link anchor).
- Other lanes: `bal` / `vet` / `sel`; weights (`@`), replication (`*`),
  subdivision (`[ ]`).
- Meta-macro: arrangements of arrangements (kind-polymorphic form references —
  the resolver was designed for it from the start).
- Accepted commitment: standalone-Odonus scale changes route through Vetula
  (Vetula is always mounted, so always available even in Solo).

## Pointers

- `src/Triggerfish/Macro.purs` — the language.
- `src/Triggerfish/Main.purs` — `macroPanel`, `MacroTick`, `applyCell`,
  `applyMod`, `parseScaleArg`.
- `src/Triggerfish/SourceQuery.purs` — `SetScale` (→ becomes Vetula-targeted).
- `src/Triggerfish/Odonus/Grid.purs` — `SetScale` handler (→ removed when Odonus
  becomes a follower).
- Vetula harmonic model: `src/Vetula/App.purs` (`st.key :: Harmonia.Chord.Key`,
  `Mode`, key/scale pickers, `familyScale`).
