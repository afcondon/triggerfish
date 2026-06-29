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

### Odonus — scenes become the recallable-preset store ✅ DONE (`dd533c4`)

AC: *"I like 'Capture current' taking a name from a form and storing the whole
Odonus config under that name, then switching them easily."* Answers the
long-open `scenes-vs-content-library` question: **ONE mechanism — scenes ARE the
Odonus preset library.** Built:

- **`Scene = {name, text}`** — the full authored patch rendered to Lepidoptera
  eDSL (parsed back on recall; lossless). Replaces `{name, odo}` (which dropped
  the State-side gen/swing/etc.).
- A **name input** in the SCENES pane; "＋ Capture current" stores the whole
  config under it.
- **Persistence**: the Store holds `{live, scenes}` (v2), restored on init — the
  scene library AND the live working patch survive reload.
- **A5** exposes the scenes (`AskLibrary` → scenes; `LoadEntry i` cold-loads;
  `ImportText` adds a scene). Cold load from the manager **hard-resets** the
  playheads; the in-instrument SCENES strip **phase-preserves** (and chaining
  uses the phase-preserving path) — two entry points, one store.
- The **per-instrument sequencer stays** and now sequences the named scenes
  (AC's "continue in that vein" — see the locality note below).

**Flagged future — monoidal loading (AC's aside, elegant):** if a patch were
*every field a `Maybe` + a defined empty `Odonus` + a `Monoid`*, loading would
be LAYERABLE — a preset that sets only some fields (just the scale, just the
heads) overlaid on the current state; stack presets by `<>`. Today's scenes are
full-config (every field set). Worth doing once there's a reason to partial-load;
it's a real generalisation of "preset" toward "patch diff".

**Open architectural tension AC named (NOT resolved) — sequencer locality:**
*"sequencing is composition, all sequencers should be on one page"* (Odonus +
Balistes + Selene + Vetula sequencers coalescing into a new arrangement page)
**vs** *"each instrument's sequencing needs to be right there so you don't bounce
panes for a simple structure."* AC: we already have sequencing in Odonus,
Balistes, Vetula → *"continue in that vein, but I'm really not sure."* Per-
instrument for now. Note: scenes-as-named-persistent-Lepidoptera is the BRIDGE —
if every instrument's presets are named + persistent + Lepidoptera, a future
unified arrangement page could sequence across instruments without rework.

### Odonus — Key pane re-order ✅ DONE (`decd2f5`)

Built exactly as locked below — OCTAVE common at top, the Scale·Chord·Vetula
selector, scalar-transpose folded into the Scale section, inactive sources greyed
(opacity + pointer-events:none). The Chord source re-exposes the internal McMullen
progression. **The `renderCell` single-snap change landed with it** (chord/Vetula
source → chromatic value snaps straight to the chord tones, no scale pre-snap / no
scalar transpose) — **AUDIO-SENSITIVE, AC to audition on the rig.** A model
conflict surfaced + fixed: `recomputeFollow` used to force the overlay off when
`follow=Nothing`, which the 100ms voice poll used to clobber the Chord source back
to Scale; it now no-ops when not following (the source selection owns the overlay).

AC: *"the scale at the top is no longer king … I think just making sub-sections
and graying out the inactive ones would be enough."* Locked layout:

- **OCTAVE at the very top, COMMON to all sources** — octave transpose is always
  safe regardless of where the pitch-set comes from (AC).
- **The source selector** — *Scale-Key · Chord · Vetula* (AC: *"your idea
  Scale-Key / Chord / Vetula is good"*). The existing FOLLOW-VETULA pane folds in
  here. The selected source's sub-section is active; the others grey out.
- **Scale sub-section** (root / type / mask / SPREAD) — and **SCALAR TRANSPOSE
  lives HERE, not common**: AC — *"it's not safe to transpose by a scalar unless
  you know the scale, which you won't if it's from Vetula."* So scalar-transpose
  greys out with the Scale section when Vetula/chords drive.
- **Distribution** stays (shapes the chromatic knob-value before the snap).

Pairs with the **deferred `renderCell` single-snap change** — when the source
isn't the scale, the scale gets no say, which is exactly the re-order's premise.
(Audio-sensitive — AC should audition.)

**Refinement (AC, while testing) — the Vetula source is a SETTING, not a gated control (`ac588ff`).** First cut DISABLED the Vetula button until a voice was bound. AC: *"merely not receiving a signal shouldn't disable the choice — it should accept the input setting, but the pane below should have a clear callout that it's not active."* Done: State now carries an explicit `source :: SourceTag` intent (not derived from the overlay), so "Vetula selected, no signal yet" is a real selectable state; the button always sticks, and the FOLLOW VETULA section shows ● live chord / ◌ not active + a bordered callout. Binding a voice later auto-adopts it (the 100ms poll), so the callout resolves itself. General principle to carry: **a source/destination CHOICE is a setting the user owns; live-signal availability is feedback shown alongside, never a reason to disable the choice.**

**FLAGGED (AC, while testing) — the Vetula→Odonus get-going WORKFLOW is too many steps.** To make Odonus follow Vetula you must: Lab → grow a progression → save → library; Performance → Load it; toggle a voice → odo; (Odonus) select Vetula + pick the voice. AC: *"we clearly need to work on the workflow on the Vetula side."* A dedicated Vetula-UX pass — streamline build→perform→bind:
  - **A direct define→play path.** AC: loading a saved progression into Performance *"is also a wasteful step with only one sequence — should be a path direct from defining a progression to playing it."* (i.e. play the current Lab progression in Performance without the save→library→Load round-trip.)
  - A one-click "send to Odonus"; a default Performance voice already → odo; fewer Lab↔Performance hops.
  - Pairs with the deferred Vetula brand-restyling.
- **BUG FIXED on the way (triggerfish `e88cce5` + vetula `bfb0bee`):** the Lab progression was WIPED on a tab switch (SetTab cleared `path`; startWith rebuilt `chords` to seeds + the loaded-perf copy only). Now `path` + its chords survive a Performance round-trip (and a key/scale change).

**New source idea (AC) — Chord from live MIDI input (KeyStep 37).** *"Perhaps we
can replace or enhance Chord by taking notes from, say, my KeyStep 37 too."* The
generalisation: the Chord/external source is just a **pitch-SET fed from
somewhere** — a static McMullen progression, a followed Vetula voice, OR **live
held notes on a MIDI input device** (play a chord on the KeyStep → Odonus
quantises its playheads to those notes). Needs a NEW capability: **Web MIDI
INPUT** (Triggerfish is output-only today; `Midi.requestAccess` already exposes
inputs). Slots into the same selector as a fourth option (or the Chord source's
"from MIDI" mode) — the same "external pitch-set" socket the Vetula feed already
proves. Build after the re-order + the `renderCell` fix; design it as
`PitchSource` gaining a `MidiIn`-style case.
