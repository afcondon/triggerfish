# Envelopes as a machine — what to steal from Zadar, Tides and Ceis

**Status: design sketch. AC, 2026-08-09, after the first working polyenv.**

> "I suspect we'll revise this and maybe even just put direct controls of some
> kind on it… perhaps we should take inspiration from some of the modular world's
> envelope generators like XAOC's Zadar and Mutable's Tides and Instruo's Ceis,
> most of which I have. Mashing up the designs of others has worked well for us
> on the sequencing front."

It has, and the precedent is the argument: Balistes is Grids, Odonus is René
crossed with Fugue Machine, the NOTES pad is Marbles. In each case what was taken
was **the control surface and its conceptual model**, not the algorithm — the
algorithm was reimplemented to fit the rig. Envelopes should work the same way.

## What the current surface is, and its ceiling

Eight `EnvSlot`s of eleven numbers, edited by nudging one field at a time, with
an 18-shape starter library to jump between regions. That is a *parameter editor*
with a preset list bolted on. It works — walking the library is already the most
effective part — but it has a hard ceiling: **every one of the eleven fields is
equally prominent and equally inert.** There is no gesture, no morph, no
relationship between the eight slots, and no way to say "like that one, but
longer".

The three modules AC names each solve exactly that, and they solve it three
*different* ways. That is what makes the mash-up worth doing rather than picking
one to copy.

## The three, and what each is actually for

**XAOC Zadar — a curated 2-D space of complete shapes.**
Four channels, each picking a shape from a large library organised as
*category × variation*, then bent by a handful of global controls (time, level,
and two shape-warping parameters). The insight is that the library is the
instrument: you do not build an envelope, you *travel* to one. Crucially the
variation axis is continuous and ordered, so scanning it is musical rather than
combinatorial.

This is the closest to what already exists — and it says the starter library
should become **two-dimensional and ordered**, not a flat list. Today `[` / `]`
walk a single ordered continuum, which is already better than a bag; the Zadar
reading is that a second axis (character within a family) is where the real
range lives.

**Mutable Tides — one shape, continuously deformed.**
A single generator with *slope*, *shape* and *fold* as continuous morphs over one
underlying contour, plus a mode axis (AD / looping / AR) that reinterprets the
same controls. The insight: **a small number of continuous parameters that each
change everything** beats a large number that each change one thing. Tides has
far fewer controls than our eleven fields and reaches far more useful shapes.

This is the strongest argument against the current surface. `attackShape`,
`decayShape` and `releaseShape` are three separate 0..127 curve bytes; a Tides
reading would collapse them to one **slope/symmetry** control and one
**curve** control, and derive the three.

**Instruo Ceis — envelopes as a modulation *system*.**
Multiple related outputs from one gesture — inverted, offset, scaled — so one
trigger produces a family of related modulations rather than a single voltage.
The insight is about the *bank*, not the shape: eight envelopes are not eight
unrelated settings, they are one gesture seen eight ways.

This is the one that fits the rig best and is completely absent today. Selene's
`freshBank KEnv` already gestures at it — "eight envelopes walking from plucked
to swelling" — but it is a static default, not a live relationship. Ceis says the
bank should have **its own controls**: spread, skew, inversion pattern.

## The synthesis

Three axes, deliberately mapping onto the three modules:

1. **A 2-D library** (Zadar) — family × character, ordered on both axes, replacing
   the flat 18. Cycling stays the primary gesture because it already works.
2. **Two or three continuous morphs** (Tides) — over the *whole* shape, not per
   stage. Candidates: `time` (already the single most consequential field),
   `slope` (attack↔release symmetry), `curve` (exponential↔linear↔logarithmic,
   deriving the three shape bytes).
3. **Bank-level relationships** (Ceis) — one gesture, eight related envelopes:
   spread the times across the bank, invert alternate slots, skew the curve
   across the eight. These are exactly the "Reichian" controls Odonus already has
   (FAN, STAGGER, SPREAD) applied to a different quantity, which is a good sign
   the idiom is right for this codebase.

Note what this deletes: eleven per-slot numbers stop being the surface. They
remain the *model* — the wire format is fixed by the FH-2 and unchanged — but the
player would touch perhaps five controls, two of which act on all eight slots at
once.

## Hazards

- **The FH-2 is not the only target, and should not shape the design.** Its
  envelope is a 5 V bipolar attenuverted ADSR with three curve bytes, and its
  ceiling is documented (see `DESIGN-routing-backward.md` and
  `familyDefaultRange`). A generic envelope machine that then *lowers* onto the
  FH-2 keeps the door open for the ES-9 version, which is where the range and
  resolution actually are. Designing to the FH-2's eleven fields would nail the
  machine to its weakest available target.
- **Do not build a third generator.** The rule from
  `DESIGN-odonus-envelopes.md` still holds: FH-2 MCV envelopes and ES-9 polyenv
  already exist; this is a *control surface* over them, not a new engine. The
  moment it computes an envelope in the browser it has become one.
- **The derived-vs-stored question.** If `slope` and `curve` derive the three
  shape bytes, the derivation is the truth and the bytes are output — so a shape
  edited on the FH-2's own front panel cannot round-trip back into the morphs.
  That is probably acceptable (the same is true of Zadar) but it should be a
  decision, not an accident.
- **Keep placement out.** A shape carries no range and no jack, which is what
  makes it portable and worth collecting. That separation already exists and is
  easy to lose when adding bank-level controls.

## Not yet

This is a sketch, and the current surface should get some live use first — the
starter library plus per-field keys is enough to find out *which* of the eleven
fields actually get reached for. That answer should drive which morphs are worth
having, rather than the module comparison alone.
