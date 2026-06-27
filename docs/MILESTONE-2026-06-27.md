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

## Master transport + Vetula on the shared clock (later, same day)

Two more landed, both verified build-clean + headless:

- **Master transport.** One **PLAY / STOP** at the shell (top-left). Every
  module's own run button is now a sticky **ARM / cue** toggle; a module sounds
  iff `master && armed`. So arming a stopped rack is silent (`◆ CUED`); PLAY
  starts every armed module together on the shared downbeat (`❚❚ PLAYING`);
  toggling arm mid-play drops a module in/out live. Wired by a `SetMaster Bool`
  query broadcast to all four (Selene's is a no-op; Vetula has its own). Held
  notes are silenced on every sounding→silent transition. *Verified:* Balistes
  reads `◆ CUED` + frozen step while armed-but-stopped, advances only under PLAY,
  freezes (still armed) on STOP — no page errors.
- **Vetula's Performance transport is now on the shared Binnacle clock.** It was
  free-running its own `startWorkerTicker` at its own BPM; now it
  `Binnacle.connect`s (free-run 120 → Link-lock) and runs `Scheduler.startGrid`
  like Odonus/Balistes, gated on `armed && master`. The tick's absolute grid
  **index** is the pulse (so every voice — and every module — shares one
  downbeat), note durations read the live clock tempo, and notes schedule at the
  tick's lookahead `delayMs` (`stepVoice` gained a `baseDelayMs` arg). The shell
  now broadcasts `SyncFree` to Vetula too; the bpm field nudges the free
  baseline (so it still works standalone) and tracks the live tempo. *Verified
  structurally:* three rig-WS connections now (Odonus + Balistes + **Vetula**),
  renders clean, master broadcast reaches it across its own query type with zero
  JS errors. **Audible "locks tight" is the rig/MIDI test — yours to confirm.**

The standalone Vetula app (`/vetula`, a separate vendored copy) is untouched and
still plays via its own default `master:true`.

## Known issues / deferred

### 1. Free-run timing alignment — ADDRESSED (shared baseline broadcast)

**Done (binnacle `b7fdcee`, triggerfish `ae5c4d9`).** The shell now mints one
free-run epoch at startup and re-broadcasts `{freeT0, 120}` to the clocked
sequencers (Odonus + Balistes) every 1.5 s via a `SyncFree` query; each adopts
it with `Binnacle.Clock.setFreeBaseline`, so they share one free-run timeline
and downbeat with no rig. Idempotent, catches late-mounters, and a no-op on any
module currently Link-locked (the rig anchor still wins). **Vetula is now on this
baseline too** (see "Vetula on the shared clock" above). Remaining follow-on: a
**shell BPM control** to drive `freeTempo` (today fixed at 120).

<details><summary>original analysis (kept for context)</summary>

#### Free-run timing alignment (only without the rig)

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

</details>

### 1b. Static public deployment (no Atlantis) — checklist

Goal: ship as a static webpage people play on their own machines with their own
MIDI sound generators, with none of the rig. Free-run is now tight (§1), so the
remaining gaps are:

- **MIDI output picker (the real blocker).** Every module hardcodes
  `Midi.requestAccess "IAC"`; a stranger's machine has no IAC bus, so it makes
  no sound. Need a port dropdown (enumerate `WebMidi.outputs`, let the user
  pick, default to the first) — at the shell, shared by all modules, or per
  module. Without this the page is silent for most visitors.
- **Manual BPM at the shell** — drive `freeTempo` (see §1) so there's one tempo
  knob for the rack with no rig.
- **Graceful no-Web-MIDI** — a clear message when the browser/permission denies
  Web MIDI (Safari, no-HTTPS) instead of a silent failure.
- **Static bundle + deploy** — already a single `public/bundle.js` + page; point
  Cloudflare Pages at `public/`. Serve over HTTPS (Web MIDI needs a secure
  context).

### 2. Hidden instruments keep their animation timers

Each instrument runs a ~33 ms frame timer for its UI animation; while it's the
hidden tab that work is wasted (the scheduler must keep running — only the
*animation* is pointless). Pause the frame timer when `display:none`. Harmless,
just wasteful. Deferred.

### 3. Phase 2 — the Vetula → Odonus quantizer bridge

**First cut landed** (`30f89aa`): the "◄ Vetula chords" snapshot button feeds
Vetula's whole progression into Odonus's `feed`/`currentChordPCs` quantizer.

**Next (designed, NOT yet built) — MIDI / Odonus voice destinations.** Replace
the snapshot with a live follow: each Vetula Performance voice gets a destination
**MIDI** (today's behaviour) or **Odonus** (a block chord-conductor — no MIDI,
always on, reuses the 1–16 channel field as an Odonus id). The shell polls Vetula
~100 ms (`AskVoiceChords`) for each Odonus-dest voice's *current* block chord and
feeds it to Odonus (`FeedVoiceChords`); Odonus stores `follow :: Maybe Int` and
its KEY pane selects one-or-zero of those voices. **Remove the McMullen picker**
(ii7/V7/Imaj7/vi9 chips + `⟳ chords` + STEPS/CHORD) and the "◄ Vetula chords"
button entirely. Reuses the existing `feed → currentChordPCs → quantiseToChordPCs`
engine (push a single-element feed each poll). This is the last of the three
queued features; master transport (done) and Vetula-on-shared-clock (done) were
the first two.

### 4. Duplicated voicing engine

`Triggerfish.Vetula` (theory, for Odonus's quantizer) and `Vetula.Theory.Voicing`
(the app's) are two copies of the same vendored engine. Deliberate for now;
reconcile when Phase 2 wires them together.
