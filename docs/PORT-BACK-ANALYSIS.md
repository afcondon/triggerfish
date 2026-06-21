# Porting Triggerfish back to the live-coding engine

> Analysis, 2026-06-21. How to make the Odonus features we've built in
> Triggerfish (GUI) "live-coding enabled" — i.e. runnable on the BEAM
> Atlantis/purerl-tidal substrate, and expressible in cell text.
> Companion to `BRIEF.md` (Triggerfish design) and purerl-tidal's
> `docs/north-star.md` (the substrate contract).

## TL;DR

The port lands in an architecture already shaped for it. Three facts decide everything:

1. **The eDSL is PureScript records, not a text grammar** (north-star §1). Triggerfish's
   SOURCE panel already emits `odonusWith { … }` — that IS the cell language. The
   authoring port is mostly making that text match the BEAM `OdonusConfig` field names
   and `Pattern`-wrap the values.
2. **The seam is two channels, and we want both:**
   - **Authoring** (GUI → cell text): a Triggerfish patch becomes a real, type-checked,
     hot-loadable Calypso cell. Durable, front-end-independent (north-star §7).
   - **Performance** (GUI ↔ live-control bus): `set-control odonus.notes.K v` streams
     knob moves to a running voice with no recompile. This is "drive the rig live now."
3. **The BEAM Odonus is already more capable in places than the Triggerfish twin** —
   everything is `Pattern a` per cell (notes/skip/gate/glide/vel/ratchet/probability/
   mod1–4), plus per-head transp/speed/direction/mute and scale+distribution. So most of
   what we built already has a home; only a few things are genuine gaps.

The chord quantizer, in particular, is the *cleanest* port of all — the north-star's own
worked example is literally `odonusOver (live "chord1.currentVoicing") [...]`.

## The substrate, in one paragraph

A cell is a PureScript expression evaluating to `Voice = Notation >> Emitable`. `Odonus`
is a `Notation` yielding `Pattern Pitch`; sinks (MIDI, CvRouter, Fh2Daemon) are `Emitable`.
Per-cell PureScript compile + hot-load means each voice is its own module; cross-cell
references go through the **live-control bus** (`set-control` writes, `live "slot"` reads),
and each voice **publishes an output record** under its name (`chord1.currentVoicing`,
`odonus1.currentPitch`). The WebSocket server (`:3012`) dispatches verbs; `tidal_control_bus`
(ETS) holds the values; a version counter lets voices rebuild their control map only when it
changes. **The engine is the durable interface; editors (Calypso / VS Code / AI agent /
*Triggerfish*) render and drive it.**

## Feature-by-feature port matrix

| Triggerfish feature | BEAM Odonus status | Port strategy |
|---|---|---|
| notes / skip / gate / glide (per cell) | **Exists** (`Array (Pattern _)` in `OdonusConfig`) | Authoring: emit arrays. Perf: `set-control odonus.{notes,skip,gate,glide}.K` |
| per-head transpose / speed / direction / mute | **Exists** (per-playhead arrays) | Authoring + `set-control odonus.{transp,speed,…}.H` |
| scale + distribution + SPREAD | **Exists** (`Scale`, `Distribution`, `set-scale` verb) | Authoring + `set-scale c-minor`; SPREAD = pick a preset |
| **per-cell LENGTH** (`dur`) | **Gap** — only a scalar `durMs` at bind time | Add `noteLengthMs :: Array (Pattern Int)` to `OdonusConfig`, thread to `odonus_voice:emit_step` |
| **per-head OFFSET / LEN** | **Gap** — engine length is fixed-16, no per-head offset | Add per-playhead `offset`/`len` to config + cursor math in `odonus_engine.erl` |
| **per-head PATTERN** (René orders) | **Partial** — BEAM has global `navMode`, not 5 per-head orders | Add a per-head order array (the René library) to the engine traversal |
| **Reichian FAN / STAGGER / PHASE** | **Gap** (macros) | Once per-head offset/len exist, these are bulk control-bus writes — an app-side gesture or a `phase-*` verb |
| **chord-progression quantizer** | **Designed** ("Thread 2.5"; north-star `odonusOver`) | A Vetula chord-voice publishes `currentVoicing`; Odonus reads `live "chord1.currentVoicing"`; chord-seq advance = a small clock. *Cleanest port.* |
| **randomisation matrix** (freq × depth) | **Gap** — BEAM has per-cell `probability` + `shred-mod`, not the slow-drift model | Either (A) run app-side, stream resolved values, or (B) port `Gen` to a BEAM generative process writing the bus on per-source clocks |
| **per-voice independent clocks** (Euclidean) | **Gap** — one master clock; per-head `speed` only | New on both sides; the next big design piece |

## The deepest structural insight

**Triggerfish's `Step` handler ≅ a BEAM gen_server that owns a control-bus namespace and
mutates it on its own clock.** Our `runGen → tickChord → stepEmit` loop is *exactly* the
shape of a small BEAM process that, each tick, fires per-source mutations and writes
`set-control odonus.notes.K …`. So:

- The **randomisation matrix** "wants to be" an `odonus_gen` process (one PRNG, per-source
  period + per-fire depth → recurring bus writes).
- The **chord-sequence clock** "wants to be" a tiny process advancing a published
  `currentVoicing` every N steps.

Both are the same pattern the substrate already uses for autonomous emitters (PortClaim +
bus publication). The port isn't a rewrite — it's relocating a loop we've already written and
*proven* (see the harness) from the Halogen component into a gen_server.

## The one real decision: where does the generative brain live at performance time?

- **Option A — Triggerfish is the brain.** App runs the model, streams resolved values over
  the bus. *Pro:* zero BEAM work, immediate, you SEE exactly what plays. *Con:* needs the GUI
  running to perform; the generativity isn't in the durable cell text — against "the engine is
  the durable interface" (§7).
- **Option B — the cell/engine is the brain.** Port `Gen` into a BEAM process / Pattern
  encoding; Triggerfish authors the config (periods, depths, chord picks, phasing) into cell
  text; the rig runs autonomously and hot-loads edits. *Pro:* matches the north-star, runs
  without the GUI, live-codeable. *Con:* more work; needs a faithful BEAM encoding of the
  freq×depth slow-drift.
- **Recommended — hybrid, A-first.** Static structure + chord + phasing go via the authoring
  channel immediately (most already exist on BEAM). The randomisation matrix runs **Option A**
  first to get playing on the real rig fast, then migrates to **Option B** (`odonus_gen`) so it
  survives without the GUI. The chord quantizer goes straight to **Option B** because the
  architecture already has the exact shape.

## Impedance mismatches to respect

- **`Int` vs `Pattern Int`.** Static port is just `pure n`. The randomisation is the *only*
  place the two models genuinely differ — Triggerfish mutates concrete state; BEAM samples
  Patterns / the bus.
- **Two randomness philosophies.** Triggerfish = one PRNG, "one element per fire," deterministic
  slow-drift. BEAM = per-cell independent `probability` rolls + `shred-mod`. These are *different
  instruments*. Porting must preserve the freq×depth *behaviour*, not adopt BEAM's per-cell
  probability.
- **The harness is the conformance oracle.** `test/Harness.purs` already measures the invariants
  that must survive the port — chord adherence 100%, skip equilibrium ~2.3, heads never all-off.
  Run it against the BEAM port the way `go-conformance.sh` validates backend-go: same config →
  same statistics. This is the differential-conformance method applied to a sequencer.
- **One chord realize, not two.** We vendored `Tidal.Vetula` into Triggerfish and added
  `quantiseToChordPCs`. The BEAM side has Vetula + `Tidal.Scales.applyDistribution`. The port
  should converge on **one** chord-realize + multi-octave snap so the fork can't drift. (See
  open question 2.)

## Recommended staged path

- **Stage 0 — Performance spike (days).** Triggerfish opens a WS to `:3012` and streams
  `set-control odonus.*` for the static grid + heads. Goal: one Odonus voice plays from
  Triggerfish on the real rig (16 trig + 24 CV). Proves the wire. *(Option A.)*
- **Stage 1 — Authoring fidelity.** SOURCE panel emits a real BEAM `odonusWith { … }`
  expression (correct field names, `Pattern`-wrapped). Paste into a Calypso cell → compiles →
  plays. A Triggerfish patch is now a durable, hot-loadable cell. Covers every *existing*
  feature.
- **Stage 2 — Close the model gaps.** Additive `OdonusConfig` fields + engine threading:
  per-cell `noteLengthMs`; per-head `offset`/`len` (unlocks Reichian macros); per-head pattern
  orders.
- **Stage 3 — Chord quantizer, the right way.** A Vetula chord-voice publishes `currentVoicing`;
  `odonusOver (live "chord1.currentVoicing")`; a small chord-seq advance clock. Matches
  north-star §3/§5 verbatim.
- **Stage 4 — Generative layer.** Port `Gen` (freq×depth) to an `odonus_gen` process writing the
  control bus on per-source clocks. Harness as the oracle.
- **Stage 5 — Expressivity + per-voice clocks.** Express gen/chord/phasing in cell text; design
  per-voice Euclidean clocks (new on both sides).

## Open questions for Andrew

1. **Performance brain:** Option A (Triggerfish streams) vs B (BEAM autonomous) vs hybrid —
   priority order? (Recommend hybrid, A-first.)
2. **Vetula fork:** keep the vendored Triggerfish copy and hand-sync, or make Triggerfish
   `extraPackages`-path-depend on purerl-tidal's `Tidal.Vetula` so there's one source of truth?
   (The latter couples the build but kills drift.)
3. **What does "live-coding enabled" mean first** — (i) drive the real rig live from Triggerfish
   *now*, or (ii) emit durable Calypso cells, or both? Shapes Stage 0 vs Stage 1 ordering.
4. **Per-voice clocks** — Euclidean-per-voice is new on both sides. In scope for this port, or a
   separate effort after the matrix lands on BEAM?
5. **Should the SOURCE panel become bidirectional** eventually (text → GUI), or stay GUI→text and
   let Calypso/VS Code own the text-editing half? (north-star §6 suggests the leaf round-trips;
   the whole-Odonus round-trip is the `Show` instance.)

## Where the gaps actually live (files)

- `purerl-tidal/src/Tidal/Odonus.purs` — `OdonusConfig` (add `noteLengthMs`, per-head
  `offset`/`len`, per-head pattern order).
- `purerl-tidal/src/odonus_engine.erl` / `odonus_voice.erl` — cursor math + step emission
  (per-head len/offset, note-length thread).
- `purerl-tidal/src/Tidal/LiveControl.purs` + `Tidal/WebSocket/Handler.erl` — the bus + verbs
  (the `set-control odonus.*` surface Triggerfish drives; a `phase-*` verb if Reichian macros
  go engine-side).
- `purerl-tidal/src/Tidal/Vetula.purs` — chord realize (the shared one; resolve the fork).
- `purerl-tidal/src/Tidal/Scales.purs` — `applyDistribution` / `shiftDegreesInScale` (reuse for
  quantize; extend for multi-octave chord snap).
