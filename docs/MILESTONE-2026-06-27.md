# Milestone — the rack becomes a rack (2026-06-27)

Triggerfish went from "one instrument visible, one clock running" to a true
four-module rack you can play together and export whole.

## Landed this session

- **Vetula is the 4th instrument** (`ODONUS | BALISTES | SELENE | VETULA`). Its
  whole app is vendored under `src/Vetula/**`; the switcher mounts it like the
  others.
- **Background-safe scheduling.** `Binnacle.Ticker.startWorkerTicker` runs the
  scheduler poll in a Web Worker, so a backgrounded tab (you switch to Ableton)
  no longer throttles the clock and stalls MIDI. All four instruments + Vetula's
  Performance transport use it.
- **Keep-all-mounted.** The selector only toggles which instrument is *visible*;
  all four stay mounted and running, so they play together as a rig jam (each on
  its own MIDI channel / CV). Default `running:false` → silence on load.
- **Dedicated TIDAL tab.** A read-only one-stop aggregate of all four modules'
  current source (Query-pull → assembled document), with COPY / REFRESH, for
  pasting into Calypso or an editor. Odonus's own SOURCE pane is collapsed by
  default; Balistes/Selene keep theirs (driven bidirectionally).

## Known issues / deferred

### 1. Free-run timing alignment (only without the rig)

**On the rig there is no problem.** When link-spike forwards the Ableton Link
anchor, every module's `Binnacle.Clock` ingests the *same* absolute affine map
(`beat(t) = beat + (t − unixMicros)·tempo/60e6`), so all four compute identical
beats — phase-locked to Ableton and to each other, tempo changes (even from an
iPad on the LAN) included. This is the Link sync we already proved.

The gap is **browser-only free-run** (no rig forwarding anchors). Each module
calls `Binnacle.connect` separately, so each clock's free-run beat-0 is *its own
mount instant* (`freeStartMicros`). Four modules mount a few ms apart → a
**constant phase offset**, not accumulating drift (same `freeTempo` ⇒ parallel
maps). It only becomes real drift if two modules free-run at *different* tempos.

**Fix (reuses the Link mechanism):** give the four a *shared* anchor even in
free-run, so they share one beat-0 + tempo. Two ways:

- **Shared clock** — create one `Binnacle` at the shell (`Main`) and pass its
  `Clock` to all four; they stop each `connect`-ing their own. Cleanest; a
  refactor (instruments currently self-connect).
- **Synthetic anchor broadcast** — the shell mints a free-run anchor (a fixed
  affine map from one chosen t0 + tempo) and `ingestAnchor`s it into all four
  clocks, exactly mirroring the rig. Re-broadcast on tempo change. Minimal —
  the shell becomes a local "Link" for the no-rig case, and the existing
  lock/free-run code path is untouched.

Either way the shell owns one tempo for free-run (no per-module free-run tempo).

### 2. Hidden instruments keep their animation timers

Each instrument runs a ~33 ms frame timer for its UI animation; while it's the
hidden tab that work is wasted (the scheduler must keep running — only the
*animation* is pointless). Pause the frame timer when `display:none`. Harmless,
just wasteful. Deferred.

### 3. Phase 2 — the Vetula → Odonus quantizer bridge

The original integration target: Vetula's current progression feeds Odonus's
KEY·CHORDS quantizer socket (quantize-to-chord-PCs vs. heads-play-the-literal-
voicing). Both apps vendor the same `Tidal.Vetula`. Not started.

### 4. Duplicated voicing engine

`Triggerfish.Vetula` (theory, for Odonus's quantizer) and `Vetula.Theory.Voicing`
(the app's) are two copies of the same vendored engine. Deliberate for now;
reconcile when Phase 2 wires them together.
