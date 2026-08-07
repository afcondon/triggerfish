# Euclid chains and resets

**Status: planned, deliberately not built and not exposed.** Written up
2026-08-07 so it can be pulled out and implemented later. Nothing here should
reach the Odonus or Selene UI until AC says so.

Prompted by vpme.de's **Euclidean Circles**, which does two things Triggerfish
doesn't: it chains Euclidean patterns, and it has resets.

## What chaining is

Two or three Euclid figures **follow on from one another** to make one longer
composite cycle:

```
E(5,12) → E(6,12) → E(7,13)
```

That is a 37-step cycle (12 + 12 + 13) with 18 pulses in it, whose internal
structure changes three times before it repeats. The point is a phrase with
*shape* rather than a single figure looping.

**This is sequential concatenation, not a clock cascade.** The distinction
matters enough that the rejected reading is recorded at the bottom, because it
costs an order of magnitude more and it would be easy to re-derive it wrongly.

## Why it is cheap

`Reef.Odonus.advanceEuclid` already makes the Euclid a **clock**: `etick`
advances every base tick and samples the rhythm; `seqPos` moves only on a pulse.
Chaining does not add a concept — it replaces the single figure that `etick` is
sampling with a list of them.

Critically, a chain is **per-head**. No head needs to know about any other, so
this stays inside:

```purescript
step o = o { heads = map (advanceHead o.cells) o.heads }
```

The `map` survives. There is no dependency graph, no ordering constraint, no
cycle rejection. Everything happens inside one head's `advanceHead`.

## The model change

`Head` currently carries the figure as two scalars plus a phase:

```purescript
, etick :: Int    -- 0..esteps-1
, pulses :: Int   -- k
, esteps :: Int   -- n
```

Chained, a head carries a list of links and which one it is in:

```purescript
type EuclidLink = { beats :: Int, steps :: Int }

, etick :: Int          -- phase WITHIN the current link, 0..link.steps-1
, link :: Int           -- which link of the chain, 0..length chain - 1
, chain :: Array EuclidLink
```

The rule in `advanceEuclid`: sample `euclidHit` against the *current* link; when
`etick` would wrap past that link's `steps`, set `etick = 0` and advance `link`
(mod chain length) instead. Composite period is the sum of the links' `steps`.

**A chain of one is exactly today's behaviour**, so the change is
behaviour-preserving by construction for every existing patch.

`anyPulse` / `pulsedThisStep` need the same treatment — they walk the tick
window ahead of the state, so they have to walk the link boundary too.

## Wire and conformance

`Reef.Protocol.encodeOdonus = writeJSON`, so the wire shape is **derived from
the record type**. Any new required field on `Head` changes the JSON and breaks
the conformance fixture `handoffJson` (`Reef/Conformance.purs:298`), which pins
whole head records as a literal string and is decoded at line 304.

Two ways through, in preference order:

1. **Keep `pulses`/`esteps` as link 0** and add the tail as a separate field.
   Old saved state and the existing fixture keep meaning exactly what they
   meant; a head with an empty tail is a one-link chain. Least disruption to a
   format that two runtimes and the stored scenes all agree on.
2. **Replace them with `chain`** and regenerate the golden. Cleaner type, one
   representation, but it invalidates every stored Odonus patch and scene unless
   a migration reads the old shape.

If an optional field is used, note that simple-json handles `Maybe` properly —
this is NOT the raw-`JSON.stringify` hazard that bit `MidiClip` — but whether
purerl's simple-json agrees byte-for-byte is exactly what the conformance suite
exists to answer. Verify, don't assume.

## Resets

Separate feature, separate value, and cheaper still. A reset means
`link = 0, etick = 0` (and possibly `seqPos = 0`) at some moment.

- **Global sources** — every N bars, on the downbeat, or manual — are pure
  per-head state and stay inside the `map`. This is the version to build.
- **Head-sourced resets** (head A's cycle resets head B) DO need heads to know
  about each other, and so carry the whole DAG cost described below. Treat as a
  different feature; do not let it ride along.

Resets matter more with chains than without: a 37-step composite against a
16-cell grid drifts a long way from any downbeat, and a reset is what makes the
result hearable as structure rather than merely long.

## The step-count ceiling

Independent of all the above, and a prerequisite for chains being interesting:
`esteps` is clamped to 1..16 in six places — `setHeadPulses`,
`nudgeHeadPulses`, `setHeadEuclidSteps`, `nudgeHeadEuclidSteps`, and the two in
the engine at `advanceHead` and `pulsedThisStep` — plus the default in
`defaultHead`.

Raising the ceiling is backward-compatible: nothing at ≤16 behaves differently,
so existing goldens stay valid. `esteps` is explicitly **independent of `len`**,
so this does not touch the cell grid — E(7,24) against 16 cells is already
expressible, the clamp is the only thing refusing it. See
`Triggerfish.Ui.Euclid`, where the widget's bounds are already the caller's:
Odonus's ceiling is one number in `Playheads.euclidBounds`.

This is separate from a future **"large Odonus"** (more knobs, pitches, steps),
which is the other cluster of 16s — `cells :: Array Cell`, `replicate16`, the
`len` clamps, `chord.period`, `HeadOffset 0..15`, and the Twister's 16 encoders
mapping 1:1 onto 16 cells. Structural, and unrelated to this.

## The rejected reading, recorded

The other thing "chaining Euclidean clocks" could mean is a **cascade**: head
A's pulses become head B's ticks, so B advances only when A fires. It produces
multiplicative periods and very sparse output.

It is not what Euclidean Circles does, and it is far more expensive: heads would
have to advance in dependency order with A's pulse count feeding B, so
`map` becomes an **ordered fold over a DAG** with cycle rejection. Written down
so the cost is not re-derived and mistakenly attached to the cheap feature.

## Scope

**Odonus only.** Selene's Euclids are not clocked by Triggerfish at all — they
are installed as envelopes in es9-daemon, which generates the CV autonomously.
Chaining there is a daemon change, not a reef one.

## UI

Deliberately unspecified. One thing is already true: `Triggerfish.Ui.Euclid`
takes its bounds from the caller and draws a single figure, so a chain UI would
be a strip of the existing `ring`s plus a way to say where a link ends — the
control does not need rewriting to accommodate this.
