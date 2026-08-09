# Milestone — 2026-08-09, the router and the envelope surface

Two days that started with *"Balistes drums aren't reaching the FHX-8GT"* and
ended with a unified router, an envelope machine with real expressive range, and
four device-level bugs fixed at their source rather than worked around.

Functionally the best place this has been. What follows is what a fresh reader
needs to know, then what is still open.

## What landed

### The unified router

**One table.** `Source → Array Leg`, where a leg is a destination plus an offset
plus an on/off. Sources are Odonus heads, **drum lanes** (indexed by `canonKit`,
not by brain — all three brains are laid against the same 16-lane kit, so a kick
routed to jack 1 survives a brain switch), Vetula voices, Selene banks.

Destinations are **per-jack and shaped per device**, deliberately not flattened
to "a channel number". That flattening is what made the morning's FH-2 bug
possible: same channel *number*, different device, completely different meaning.

**The lowering that made it tractable:** every destination a browser can reach
today *is* a MIDI note on some port. An FH-2 envelope is a note at
channel = slot; an FH-2 gate is a note at the MCV listen channel whose *pitch* is
the selector. So `wireOf` reduces all of them to (port, channel, optional note
override) and one emit primitive serves every machine. The ES-9 kinds return
`Nothing` and report `NeedsRig` rather than failing quietly.

**⌥1 is the editor.** A row per source, a line per destination: mute dot, kind,
that destination's editable numbers, an ms trim, remove, reachability, and live
traffic. Odonus's four heads; Balistes' sixteen lanes by name and note.

**It replaced six scattered homes** for placement: two hardcoded port names in
Odonus, a channel constant in Balistes, three functions in `Midi.Routing`, the
shell's own map, Selene's doc, and a hardcoded `KIT` table *in another repo*.

### The MIDI monitor

Taps `MIDIOutput.prototype.send`, so it sees everything the page emits —
including code that never consulted the table. **Traffic the table cannot
account for is the most valuable thing on the panel**: it is the answer to
"something is sending it MIDI but I can't see what".

Counts note-**offs** separately, because an envelope that never comes back down
is either a gate held open or a note-off never sent, and from the rack those are
indistinguishable. Velocity is kept as a **range**, since "is velocity actually
varying" is the question the envelope work keeps asking.

### The envelope surface

**Working end to end**: Odonus voice I drives an FH-2 polyenv envelope at jack 1,
velocity-scaled, through the router.

**A 36-shape library** in four groups of nine, curated for *coverage* — spanning
all eight time buckets, depths 0..127 including the inverted half, velDepths
32..127 including inverse response, and 8 ms to 139 s of duration. `[` / `]`
walk it as a continuum: percussive → gated → long-tailed → expressive.

**Small multiples, and the encoding is mostly depiction** — the mark shows the
thing rather than standing for it, so there is almost nothing to learn:

| fact | how it reads |
|---|---|
| shape | the curve |
| depth | curve height; 64 is flat, because 64 does nothing |
| **inversion** | drawn *below* the baseline, which is what it does |
| **velocity** | a band — the shape at velocity 1 and at 127, filled between |
| duration | horizontal extent, log-scaled |
| time bucket | line colour (ordered ramp), redundant with extent |

**Breakpoint dragging.** Three handles — peak, corner, tail — give four
parameters across two axes with no vocabulary. Delta-based from the grab point,
so dragging back restores exactly what you had.

## Four bugs fixed at the source

Worth listing because each was silent, and each would have been "the feature
doesn't work" rather than "this specific thing is wrong".

**polyenv wanted `bipolar5v`, not unipolar.** `depth` is an attenuverter (64 =
zero, below inverts), so its rest sits at the *centre* of the output window —
only the bipolar window puts that at 0 V. On any unipolar range the envelope
pulses on a DC pedestal and never closes. The range is a WINDOW, not a
description of the signal. Determined on-rig by sweeping all four.

**Selene was overriding the FH-2's own range policy.** `familyDefaultRange` is
consulted only when `outputRange` is omitted; Selene always sent one, hardcoded.
Fixed by Selene stating no opinion, not by copying the policy.

**The envelope text round-trip destroyed velocity and depth.** The printer elided
fields at the *musical* default while the parser folded onto the *silent* one, so
a full-depth velocity-sensitive envelope came back as one that fires and moves
nothing. One shared `defaultEnvSlot` is now both bases.

**polyenv zeroed the device's undecoded bytes.** `EnvShapeState` has five bytes
we have not decoded, and "Env zero st" most plausibly lives there — so setting it
on the front panel and republishing would have silently reset it. Now carried
through untouched. **The rule: carry unknown bytes through when round-tripping
someone else's format. "We haven't decoded it" is not "it is unused."**

## Two rig facts that cost time

**polyenv and the drum breakout share MCVs — but not jacks.** Both are
`mcv = jack - 1`, so FH-2 envelope N and drum trigger N are one MCV: mutually
exclusive, last writer wins, and publishing a polyenv silently takes the drum
gates away. The router now reports this; it had been reporting zero conflicts for
a default table containing four.

*Corrected 2026-08-09 (later):* this section originally added "…driving the same
jack", which is wrong and shipped as a bug. A drum trigger is routed out the
**FHX-8GT** (`output = jack + 64`), precisely so the FH-2's CV-capable main jacks
stay free — `apply-drum-breakout.mjs` says so in its header. Only the MCV is
contended; the outputs are different hardware. `claims` now issues both a MCV
claim and an output claim per FH-2 leg, because they catch different collisions:
the MCV catches polyenv-vs-drums, the output catches two legs aimed at one jack
on *different* MCVs, which the MCV claim cannot see.

**A port that enumerates is not a live device.** With the modular switched off
the FH-2's USB port still appears, so every FH-2 route read `ok`. `Reachable`
only ever meant "a port with that name exists". `device-status` is a real round
trip and answers correctly in both states — folding it in is step 0 of the
backward view.

## The recurring shape, named

Twice in two days, in different clothes: **a paired edit where one half is
verified and the other assumed.** The printer/parser defaults; then a monitor
whose reader was updated while its writer was not, so `undefined` crossed the FFI
and read back as 0 — reporting exactly the bug the counter existed to detect.

Both compiled. **A successful build proves nothing about a pair**, and across an
FFI boundary nothing is checked at all. Prefer making the pair one value; where
that is impossible, exercise both ends before believing the result — and be most
suspicious when a fresh diagnostic reports the dramatic answer you were hoping
for.

## Still open

- **The listening test for the whole drum path.** Test C never ran; it costs
  envelopes 1–4 while applied.
- **5 V is the envelope ceiling**, confirmed against the manual. The only
  headroom paths are summing two MCVs on one jack (the output stage is a mixer —
  **untested**) or moving envelopes to the ES-9.
- **A route went missing once** during testing and could not be reproduced.
  "restore default routing" exists in the traffic panel partly for that.
- **Per-destination offsets** are plumbed but always 0; they should default from
  DeepStar's calibration tables. Until then, doubling a kick will flam.
- **`fh2-drumkit` never co-restarts with `fh2-daemon`** (issue #6). Durable fix
  is `apply-drum-breakout.mjs --save`, which flashes it.
- **Selene and Vetula still route through their own surfaces** — steps 4b/4c of
  `DESIGN-routing.md` — and `reef_voice`'s hardcoded base channel 12 still
  disagrees with Triggerfish's `1 + h`.

## Queued design

- `DESIGN-routing-backward.md` — the same table read from the jack. Contention
  only exists on the output side, so every conflict is invisible in the forward
  view by construction. Includes the loopback column and why provenance
  (observed / predicted / dead) must be a required visual channel.
- `DESIGN-envelope-machine.md` — what to take from Zadar, Tides and Ceis, and
  the open question of one machine or two (FH-2 and ES-9 may deserve different
  designs rather than one lowered onto both).

## Known tidy-up, diagnosed not done

`svgEl`/`svgAttr` exist in **six** places (five local, one already in
`Halogen.Widgets.Knob`). `Odonus.Grid.Widgets` has 19 importers across every
machine and is neither Odonus's nor grid-specific. `tabBtn` / `stepperRow` /
`miniKnob` look general but are welded to Odonus's `Action`. `Ui.Euclid` and
`Ui.Pointer` are genuinely portable; `Ui.Knob` is a pure view where the library
only has a component, so it is a contribution rather than a duplicate. 444
distinct hex literals want a palette module.
