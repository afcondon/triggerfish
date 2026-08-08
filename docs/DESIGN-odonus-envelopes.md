# Odonus envelopes — a note for the morning

**Status: design sketch. AC, evening of 2026-08-07.**

> "we have envelopes in the FH2 control surface but nothing at all about it in,
> for example, Odonus which is really where it would be more useful. All we have
> is length, which probably just surfaces as a long gate"

Correct, and the code agrees. Worth reading before starting, because the work is
probably not the work it looks like.

## What Odonus has today

Every duration control scales exactly one number — how long the note is held:

```purescript
gateMsFor f = stepMs / max 1.0 spd * (toNumber r.odo.gatePct / 100.0) * toNumber f.dur
```

- `f.dur` — the per-cell LENGTH knob
- `gatePct` — a rig-wide gate-length percentage
- `spd` — the head's speed, so a note breathes with its playhead's rate

That is a **rectangle**: on for N milliseconds, off. There is no attack, no decay,
no release, and nothing that can differ between the start and the end of a note.
Through an FH-2 MCV in gate mode a longer `dur` is literally a longer gate;
through an MCV in envelope mode it is a longer *sustain*, which is why it can
sound like it is doing something without being an envelope control at all.

## The work is routing, not building

**Two envelope generators already exist and Odonus reaches neither.**

**FH-2 MCV envelopes.** `envelopeSpec` sets `ve` on a voice, and `set-adsr-ccs`
binds that voice's A/D/S/R to four MIDI CCs. There is even a convenience form:
`set-envelope-with-ccs v o c` auto-assigns CCs `70 + 4v` … `73 + 4v`, so voice 0
lands on 70–73, voice 1 on 74–77, and so on. The shape becomes drivable live over
CC with nothing stored per note.

**ES-9 `polyenv`.** A Selene family — an autonomous envelope per output, applied
as config rather than played.

So building envelope generation inside Odonus would make a third. Don't. The job
is a route to one of the two.

## Recommended shape: per PLAYHEAD, not per cell

Odonus has four playheads and sixteen cells. Sixteen cells × four envelope
parameters is a control surface nobody would use, and it is not how the hardware
thinks either: a voice has an envelope, and the pattern modulates it.

So:

- **The envelope belongs to the head.** Four knobs per playhead — A/D/S/R —
  written to that head's ADSR CCs. Odonus already sends CCs on a head's channel
  (it does portamento with CC 65 / CC 5), so the emit path exists; this is knobs
  plus a mapping, not new plumbing.
- **The cell keeps modulating, not shaping.** `dur` and velocity stay exactly what
  they are — how long, how hard — and now they modulate a shape rather than being
  the whole of it. Nothing in the grid has to change.
- **It stays live.** ADSR over CC means the envelope is a performance control you
  can ride while the sequence runs, which is the point of putting it here rather
  than in the FH-2 config surface.

That also keeps the artefact honest: a head's envelope is rig-facing state, not
part of the pattern, so a saved Odonus pattern does not silently carry a timbre
with it. (Same split as today's name-vs-content work: the artefact holds the
musical fact, the envelope holds the placement.)

## Settle this first: what does "length" mean once there is a release?

The one question worth answering out loud before writing code.

`dur` currently means gate length, and a gate stops when it stops. An envelope's
**release outlives the gate** — so as soon as there is a release stage, "how long
is this note" has two answers, and the grid shows one of them. A short cell with a
long release sounds long; the LENGTH knob will look like it is lying.

Three things need to agree on the answer — Odonus's `dur`, the FH-2 MCV envelope
path, and Balistes' cells (which have their own `durMs`). If each decides
separately you will have three notions of duration to reconcile later, and they
will disagree only in the interesting cases.

This is the same open language question already recorded as "machines need a
trigger-vs-note duration story". Envelopes are what force it.

Possible answers, none obviously right:

1. **`dur` means gate; release is extra.** Simple, honest, and the grid stops
   predicting the sound.
2. **`dur` means total sounding time**, with the gate shortened to compensate.
   The grid tells the truth, at the cost of the gate no longer being directly
   settable.
3. **`dur` means gate, and the head shows its release as a tail on the grid** —
   the display absorbs the difference rather than the semantics. More work, keeps
   both facts visible.

## Which path first

**FH-2 first.** It is closer to what Odonus already does: Odonus emits MIDI notes
on a head's channel, and MCV envelopes plus ADSR CCs sit directly on that path.
ES-9 `polyenv` is autonomous config in the Selene idiom — a different mental model,
and it drags in the claims/panic story we spent today fixing (an autonomous
envelope keeps running when the transport stops, and only the daemon's `panic`
verb stops it).

## Rough order

1. Decide the duration semantics above. Cheap on paper, expensive in code.
2. One head, hand-wired: `set-envelope-with-ccs` for its voice, four knobs sending
   its CCs, and listen. Confirms the CC path end to end before any UI investment.
3. Generalise to four heads.
4. Decide whether envelope settings are per-session rig state or travel with a
   saved Odonus scene — and if they travel, they are envelope metadata, not part
   of the pattern.

## Watch for

- **`set-envelope-with-ccs`'s CC block is computed, not configurable** (`70 + 4v`).
  Four heads is CCs 70–85. Check nothing else on that channel already uses them —
  the Twister sends CCs 1–16, so no collision there, but the FH-2's own mapping
  table may.
- **The MCV envelope is per FH-2 voice, and heads are per Odonus playhead.** Those
  need a stable correspondence, or a head's knobs will shape someone else's note.
- **`gatePct` is rig-wide.** With envelopes in play it becomes a global that
  quietly interacts with every head's shape; may want demoting to per-head.
