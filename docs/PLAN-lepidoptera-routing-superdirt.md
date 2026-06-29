# Plan — Lepidoptera, the routing/backend switch, and the SuperDirt instrument

*Plan, 2026-06-29. Turns the synthesis in `PRESETS-AND-THE-EDSL.md` into ordered
work. Read that note first — this plan serves its invariant, it doesn't restate
it. Scope spans two repos: the Triggerfish webapp (A, B, D) and purerl-tidal /
the Atlantis backend (C).*

## The four workstreams

### A — Lepidoptera (the preset / interchange format + round-trip)

**Lepidoptera** = the transferable eDSL-as-preset format: the existing `Tidal.*`
records *blessed as an interchange contract*, canonical in purerl-tidal, vendored
into Triggerfish and Calypso. A saved preset is a named `…With { … }` value; a
collection is a versioned bundle of them. Acceptance constraints (from the note):
(1) the mini-notation *inside* it stays OG-Tidal-compatible; (2) the whole thing
is valid PureScript eDSL.

- **A0 — Spec + version.** Define the collection/file shape (a versioned bundle
  of named `…With {…}` values) and the on-disk form (eDSL text). Decide the
  version tag that travels with a file. Canonical record home = purerl-tidal
  `Tidal.*`.
- **A1 — Selene first** (cheapest: it already round-trips its rack doc). Wrap
  save / load / a named library around what it already parses. Proves the format
  end-to-end with almost no new surface.
- **A2 — Converge Balistes onto Lepidoptera.** Add the new `Tidal.Balistes`
  records for fixed-rhythm patterns + the per-cell overlay (vel / prob / cond /
  ratchet as record *fields*, not bespoke string syntax — constraint (1)).
  Faithful `balistesWith` print/parse; **migrate the localStorage Store from the
  current bespoke JSON to the Lepidoptera rendering.**
- **A3 — Odonus round-trip.** It has the printer (the SOURCE pane); add the
  *parser* side; persist its scenes/state as Lepidoptera.
- **A4 — Vetula round-trip.** Define its eDSL form (chord/pitch sets +
  progressions as `Tidal.*` values); persist; replace the in-memory library.
- **A5 — The library-manager surface** on the Tidal page: browse / name / load /
  export / import the collection, with text/file export giving cross-app
  transfer for free (Calypso speaks the same records). Quick per-instrument
  recall stays in-instrument (the two-altitudes split).

Decision points: the per-cell overlay grammar; version discipline (Lepidoptera is
now a contract across three codebases — drift becomes a bug).

### B — Routing layer + capability switch (Triggerfish)

**Goal:** the emit target becomes a per-source *routing* choice, and one global
switch gates a backend *capability layer*. Today the target is hardwired per
instrument (Balistes → drum MIDI channel, etc.).

- **B1 — Extract a shared routing layer.** Target = `MIDI ch | CV bus |
  SuperDirt orbit/sound`, per source, possibly more than one (a trigger can layer
  a modular kick under a SuperDirt kick). Selene's destinations-bound-to-targets
  model is the template.
- **B2 — The capability switch** (one master control, shell-level — no per-app
  config). It toggles whether the backend is engaged, which *augments* rather than
  merely redirects:
  - **Browser-only:** Web MIDI out, free-run / manual clock. The portable mode.
  - **Atlantis engaged:** adds the Link clock + the SuperDirt and CV/es9 targets
    (which need OSC the browser can't speak). The per-source routing **auto-greys
    targets that the current mode can't reach** — that auto-adaptation is *why*
    there's no per-sub-app config.
- **B3 — Route existing instruments through the layer**; make their clock /
  transport backend-aware.

Decision point: in Atlantis mode, does MIDI stay Web-MIDI-direct or go through the
BEAM for sample-accuracy? (Default: keep Web MIDI direct; backend adds only what
the browser can't do.)

### C — The SuperDirt backend path (purerl-tidal / Atlantis)

**Goal:** a working browser → BEAM → real-SuperDirt audio path. **Recon
(2026-06-29) grounds this:**

- purerl-tidal **already owns OSC-out** (`tidal_dispatcher.erl`: OSC + MIDI bridge
  sockets; `OscClient` opened against `gatePort 57120` via
  `tidal_oSC@foreign:startClient`). But it's a **singleton client → 127.0.0.1:57120
  = es9-daemon** (`CvRouter` in `Studio.purs`), and **`s` is reduced to typed
  MIDI** ("Tidal/SuperDirt per-orbit `s`-keyed lookup, *ported to typed MIDI*").
  So no real-SuperDirt audio path exists yet.
- **es9-daemon is the OSC *receiver*** on 57120 (a Dirt-target impersonator for
  CV/gate), not a SuperDirt sender. **No new daemon is needed — SuperDirt is its
  own SuperCollider server.** The hunch ("OSC out hidden in es9-daemon, give it
  its own daemon") resolves to: the *sender* is purerl-tidal; the work is routing.

Tasks:
- **C1 — Per-alias OSC client map.** purerl-tidal's code already flags this
  (planned "PR 2c.2"): replace the singleton OSC client with a map so one alias
  targets es9-daemon (CV) and another targets real SuperDirt (audio).
- **C2 — Emit full Dirt-protocol messages** (`/dirt/play` with the param bag: `s`,
  `n`, `gain`, `cutoff`, `speed`, `begin`/`end`, `orbit`, …) for the
  SuperDirt-targeted alias — not the typed-MIDI reduction.
- **C3 — Deconflict 57120.** es9-daemon binds it on the MBP; SuperDirt defaults to
  it. Put SuperDirt on another port (or host), selected per-alias in C1.

This is the dependency that lets the SuperDirt instrument (D) actually sound, and
"Balistes/Selene → SuperDirt" (via B) actually reach audio.

### D — SuperDirt as an instrument (Triggerfish)

**Goal:** a peer module — the classic OG-Tidal SuperDirt experience with the
visual affordances it never had (it has no native UI; this is the "give it the
knobs it lacks" move, like Grids).

- **D0 — Pick the heart + name + aesthetic.** Recommended heart: the
  **waveform-slice** (begin/end/chop/striate/speed/reverse) — most visual, most
  SuperDirt-distinctive, and it leans on the msm sample library + msm-web's
  existing waveform-player component. (Alternative heart: the FX-desk.) Needs a
  creature name of its own.
- **D1 — The core surface** (the chosen heart), built to render Lepidoptera (A),
  route via B, sound via C.
- **D2 — Sample-vocabulary browser** — connect to the msm library; audition.
- **D3 — Accrete:** the per-event FX chain as a Hainbach rack, param-pattern
  automation lanes (Selene-polysignal-flavoured), orbits-as-sends.

Bonus property: this instrument's output is the *closest to real Tidal* of any
(`s "bd*4" # cutoff "800 400"` is valid upstream), so its presets are maximally
portable — constraint (1) satisfied almost for free.

## Ordering

Dependency-driven, with the one unknown eaten early:

1. **C1–C3 — the SuperDirt OSC path** (backend). It's the gating capability and
   the work is now concrete (extend the existing dispatcher, don't build a
   daemon). Doing it first means an early audible taste and de-risks D.
2. **B — routing layer + capability switch** (webapp infra). Makes every
   instrument backend-aware and makes "SuperDirt as a target" fall out for
   Balistes/Selene. Prerequisite for D sounding.
   - **In parallel: A0–A2 — Lepidoptera on Selene, then converge Balistes.**
     Largely independent of B; proves the format and retires the bespoke JSON.
3. **D — the SuperDirt instrument.** The big new visual module, on top of B
   (routing) + C (audio) + A (save).
4. **A3–A5 — finish the format coverage** (Odonus parser, Vetula form, the
   library-manager surface). Brings every instrument under the one invariant.

## Out of scope / parked (do not let ride along)

- Balistes **sequence-across-patterns** and the **morph experiment** — a separate
  Balistes thread, unrelated to this arc.
- A **controllers** category (a virtual instrument that *drives* Arbhar/Lubadh via
  MIDI API — a *controller*, not a generator; overlaps Selene's CV path, so only
  for what needs the MIDI API specifically). Good idea, its own future thread.
- A backend-hosted *shared* Lepidoptera library (auto-sync between apps). Text/file
  export already gives transfer; shared-store is a later nicety that couples the
  library to backend uptime.
