# Sufflamen — the SuperDirt instrument. D0 decision record + build plan

**Status:** Design complete (D0 decided by AC, 2026-07-02), ready to
build. Written in `atlantis-site-planning/` because another Claude was
in flight on the code repos; **move to `triggerfish/docs/` when the
repo is quiet — the builder should read it from there.**

**Reader:** the builder Claude (Opus 4.8 is expected to be adequate),
once current Balistes/Selene lockstep work concludes. Andrew for
review before build.

**Lineage:** this is **workstream D** of
`triggerfish/docs/PLAN-lepidoptera-routing-superdirt.md` (read it
first — this doc makes D concrete and records the D0 decisions; C and
B remain specified there). Serves the invariant of
`PRESETS-AND-THE-EDSL.md`; respects the one-idea-per-instrument
discipline of `BALISTES-SELENE-RETHINK.md`. Companion:
`sample-buffer-instrument-2026-07-02.md` (the hardware-mangler sibling
and the buffer/heads abstraction Sufflamen realises in software).

---

## Name

**Sufflamen** — a real triggerfish genus (bridled triggerfish), and
Latin for a **brake** (the drag-shoe on a wagon wheel). The instrument
whose culture is chopping **breaks**, named *brake*. Nameplate:
`TRIGGERFISH · MODEL SUFFLAMEN`. The pun stays invisible until looked
up, per the naming family's chosen property. (Rhinecanthus/Picasso
remains an uncommitted candidate for the hardware-mangler instrument —
not consumed here.)

## The one idea

> **The voice rack: a named sample voice = buffer + read policy +
> per-event param bag — the thing triggers land on.**

Sufflamen is **primarily a receiver**. It defines named voices
("bd" = this sample, this slice window, this envelope, this orbit)
that Balistes drums, Selene trigger lanes, and Odonus notes reach
through the B routing layer. It also hosts exactly **one rack-level
native pattern line** (OG-Tidal mini-notation transport) — its
heritage and its portability guarantee (`s "bd*4" # cutoff "800 400"`
is valid upstream Tidal). The pattern line is one client of the rack;
the rack is the essence. This mirrors reality: SuperDirt is a
receiver, Tidal is a sender.

**What Sufflamen is NOT** (rethink discipline): not a sequencer (no
per-voice pattern lanes — Selene owns trigger lanes, Odonus/Balistes
own sequencing); not an FX-desk-first instrument (FX accrete in D3);
not a sample *editor* (msm owns the library).

## D0 decisions (AC, 2026-07-02)

1. **Name: Sufflamen.** Confirmed.
2. **Voice addressing: by NAME, not slot** — try name-first since
   names are less memory burden on the user than numbers. The rack is
   a flat namespace; semantics in §Voice-name addressing.
3. **No Web Audio audio path at all.** Sufflamen is **rig-only**: in
   browser-only mode the instrument is disabled/greyed, exactly like
   Selene (and like the future hardware-mangler instrument for
   Arbhar etc.). The B2 capability switch's auto-greying covers this —
   Sufflamen simply requires Atlantis mode. Do not build any Web Audio
   approximation.
4. **One rack-level pattern line** (not per-voice lanes) — hard line
   against sequencer creep.

## Architectural consequence of decision 3

With no browser audio and no browser OSC, the browser side is a
**pure editor + visualizer**:

- **Generation/emit authority: BEAM only.** The rack + pattern line
  are authored in the browser, pushed to the BEAM (Lepidoptera text
  over the Binnacle WS, push-to-rig idiom), and the BEAM emits
  `/dirt/play` OSC to real SuperDirt.
- **Deterministic cursors without a feedback path:** the browser runs
  the same shared PureScript pattern code (reef-style) for
  *visualization only* — read-head sparks sweeping the waveform are
  computed, not reported. Cursor drift is only ever visual, so this
  needs the phase-aligned push stamp but NOT the full Odonus-P4c
  flam rigor.
- No lockstep audio contract is needed. (The earlier
  "event-identical, sound-approximate" framing is moot — there is
  one sound realiser.)

## The heart: waveform-slice

`begin`/`end`/`speed`/`chop`/`striate`/`n` is **the read-head
abstraction made visible** (see the sample-buffer note): begin/end =
splice window; signed speed = read-head rate; chop/striate =
decomposition into ephemeral read-heads; n = buffer select. Sufflamen
is the software realisation of buffer/heads — the buffer visible
instead of trapped in a hardware module — buildable now because
SuperDirt does the audio.

Two signature UI moves:

- **Draw chop vs striate.** Slice grid overlaid on the waveform;
  render the *read order* as arcs above it (chop = contiguous within
  an event, striate = interleaved across events). The distinction
  becomes a picture — the Hylograph thesis applied to Tidal's
  most-confused function pair.
- **Live read-head cursors**, computed browser-side from the shared
  pattern code (above). The Odonus playhead idiom over audio.

Control idioms per the BRIEF: detented knob for chop count (1–32);
begin/end as draggable splice handles directly on the waveform;
2-D pad where two params are read together (candidate: speed ×
begin, scrub-like).

## Aesthetic

Within the Hainbach × Braun family: the **tape/film editing bench** —
Steenbeck flatbed, splicing block, razor, china marker. Begin/end
handles as splice marks, chop as the razor, the waveform as tape on
the bench, cue-wheel scrubbing, amber VU meters on orbit strips.
Odonus got the test-gear panel; Sufflamen gets the cutting room.

## Voice-name addressing (decision 2 made concrete)

- The rack is a **flat namespace of voice names**, unique within the
  rack. Loading a preset whose names collide with the live rack
  prompts rename-or-replace (no silent shadowing).
- Routing (layer B) resolves names **at push time** against the
  pushed rack.
- A routed trigger addressing an absent name is **dropped with a
  visible "no target" pilot lamp** on the rack header (aesthetic:
  a red lamp, unlit = dim dot) — never a crash, never silent.
- Names double as the SuperDirt `s` value where the voice doesn't
  override it, so `"bd sn"` routes read naturally.

## Lepidoptera

`Tidal.Dirt` records in purerl-tidal, printed/parsed as
`dirtWith {…}` (rack = named voices + the rack pattern line;
authored config only, no runtime state — per the presets note's
clean line). The A-series machinery (A1–A5 complete) makes this
routine; register Sufflamen with the TIDAL-page library manager
(AskLibrary/LoadEntry/ImportText) like the other four. Constraint (1)
is nearly free here: the mini-notation inside stays OG-Tidal-valid.

## Build plan

Prerequisites first (both already specified in the lepidoptera plan):

- **Gate 1 = C1–C3** (purerl-tidal): per-alias OSC client map; full
  `/dirt/play` param-bag emission for the SuperDirt alias; 57120
  deconfliction (es9-daemon holds it on the MBP — SuperDirt goes on
  another port/host, selected per-alias).
  *Acceptance:* a `s "bd*4" # cutoff "800"` source from Calypso makes
  audible, correct SuperDirt sound.
- **Gate 2 = B1–B3** (triggerfish): routing layer with
  `SuperDirt voice-name` as a target kind beside MIDI ch / CV bus;
  capability switch greys Sufflamen (and SuperDirt targets
  everywhere) outside Atlantis mode.
  *Acceptance:* Balistes BD routed to a named SuperDirt voice sounds
  through the rig alongside (or instead of) its MIDI drum.

Then Sufflamen proper:

- **D1 — the voice rack + waveform heart.** Voice list (name, s/n,
  slice window, speed/reverse, envelope/legato/sustain, gain/pan,
  orbit); waveform pane with slice grid, chop/striate read-order
  arcs, live computed cursors; the one rack-level pattern line;
  `Tidal.Dirt` round-trip + library-manager registration; push-to-rig.
  *Acceptance (rig-anchored):* author a 2-voice rack in the UI, push,
  native line plays through SuperDirt; dragging begin/end audibly
  moves the slice; chop/striate arcs match the audible read order;
  a Balistes drum route lands on a voice by name; save/load/COPY of
  the rack via the TIDAL page round-trips.
- **D2 — the sample browser.** msm library integration; audition
  (audition is itself a one-shot event through the rig — fine, the
  instrument is rig-only by decision 3); load-into-voice. Plus the
  **shadow-capture shelf**: a watched directory where ES-9
  shadow-tap captures (see the sample-buffer note) appear
  automatically. Build it as "watch a directory" so it degrades
  gracefully to empty until the hardware instrument's tap exists.
  *Acceptance:* browse msm, audition through the rig, assign a
  sample to a voice, play it from the native line.
- **D3 — accretion.** Per-event param bag as Selene-polysignal-
  flavoured pattern lanes (cutoff, shape, crush, room, delay…);
  orbits as a mixer of Braun channel strips with per-orbit sends
  (room/size, delay) — the FX-desk that wasn't chosen as the heart
  becomes the rack here.
  *Acceptance:* a patterned cutoff lane audibly sweeps; two voices on
  different orbits get audibly different reverb sends.

## Out of scope (do not let ride along)

- Any Web Audio audio path (decision 3).
- Per-voice pattern lanes (decision 4).
- Sample editing/conversion (msm's job).
- A backend-hosted shared Lepidoptera library (parked in the
  lepidoptera plan).
- The hardware-mangler instrument (its own thread —
  `sample-buffer-instrument-2026-07-02.md`), except the passive
  watched-directory shelf in D2.

## The SuperDirt daemon under Bosun (AC, 2026-07-02: in scope)

AC wants SuperDirt launched and supervised by Bosun/Bosun's Chair
rather than hand-started. It does have to run inside SuperCollider —
SuperDirt is a quark interpreted by `sclang`, which boots `scsynth`
(the audio server) as a child — but that does NOT prevent daemonizing:
`sclang` runs headless from the CLI with a startup file, no IDE
involved (the standard headless-Pi Tidal-rig recipe). Shape:

- **Start command:** `sclang /path/to/superdirt-startup.scd`. The
  `.scd` sets server options, `s.waitForBoot { ~dirt = SuperDirt(2, s);
  ~dirt.loadSoundFiles(...); ~dirt.start(port, [0, 0]) }`.
- **Port-from-env** (per the bosun-daemon skill): sclang reads env via
  `"SUPERDIRT_PORT".getenv` — the startup file honors it, defaulting
  to the rig-chosen port. NOT 57120 on the MBP (es9-daemon owns it);
  57121/57123 are the Link-anchor listeners. Candidate 57122 —
  confirm against the Marginalia ports registry at build time.
- **Readiness check:** UDP port bound (`lsof -i UDP:<port>`), plus
  optionally a `/dirt/handshake` OSC ping.
- **Drain-on-signal — the one real wrinkle:** killing `sclang` can
  orphan its `scsynth` child. Handle it in the wrapper: trap TERM,
  ask sclang to `Server.killAll` / quit cleanly, and kill the process
  group as backstop. This is exactly the bosun-daemon skill's
  drain-on-signal item, not a research problem.
- **Host:** MBP (the rig machine). Register the tested command as a
  server entry on Marginalia #245 (host `mbp`) once it works —
  never register untested.

**The "just another daemon" wish, long-run:** the desire for a plain
daemon instead of an interpreter-hosted quark is legitimately
satisfiable later — the planned custom Rust recorder/looper (the loop
instrument's realiser, see the sample-buffer note) can grow a
`/dirt/play`-compatible OSC surface and become a drop-in Dirt-protocol
sampler that Bosun manages natively. (Prior art: the original
pre-SuperDirt `Dirt` was exactly this — a small C/JACK daemon speaking
the same protocol.) v1 stays headless-sclang; the protocol boundary
means swapping realisers later costs nothing upstream.

## Open questions for build time

- ~~Where does SuperDirt actually run?~~ **Answered (AC 2026-07-02):
  the MBP**, supervised by Bosun (see §The SuperDirt daemon under
  Bosun). Port still to confirm against the registry at build time.
- `unit`/`loopAt` semantics in the voice record: expose in D1 or
  defer to D3 with the param lanes?
- Does the native pattern line support `#`-style param patterning
  from day one (it's OG-Tidal, so probably yes and nearly free via
  the existing parser) or triggers-only in D1?
- Collision policy detail: is rename-or-replace a modal, or
  inline-rename in the library import flow A5 already built?

## One-line summary

Sufflamen: a rig-only, receiver-first SuperDirt voice rack — named
voices with visible slice windows on a cutting-bench waveform, one
OG-Tidal pattern line, triggers routed in by name from the rest of
the family; BEAM is the only authority, the browser only edits and
visualizes; build order C → B → D1 → D2 → D3.
