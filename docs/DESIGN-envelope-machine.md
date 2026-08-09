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
2. **Two or three continuous morphs** (Tides) — *deferred; AC: "there's probably
   something there" but not the first move.* — over the *whole* shape, not per
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

## The display: small multiples, and depiction over convention

**AC, having read the above:** start with small multiples for the Zadar-like
options; colour the ADSR line to show the time scale; badge "Env zero st";
velocity response an open question. And: *"let's not be limited to 2D matrix"* —
Zadar's category × variation grid is a **two-encoders-and-an-OLED workaround**,
not a design. Inheriting it would be copying the constraint instead of the idea.

The thing Zadar can never exploit is that **an envelope is self-describing** — it
*is* a shape, so a wall of drawn curves needs no labels, no category names and no
legend. You recognise the one you want the way you recognise a face. Odonus's
parameter-major small multiples are already this move; this is the same idiom
applied to a library rather than to a parameter.

### The encoding

Worth stating as a whole, because almost all of it is **depiction rather than
convention** — the mark shows the thing rather than standing for it, so there is
nearly nothing to learn:

| fact | how it reads | kind |
|---|---|---|
| the shape | the drawn curve | depiction |
| depth magnitude | curve height (a depth-64 envelope is flat — it does nothing) | depiction |
| **inversion** (depth < 64) | the curve drawn *below* the baseline, which is what it does | depiction |
| **velocity response** | the shape drawn at velocity 1 AND at 127, with the area between filled | depiction |
| time scale | line colour | convention |
| "Env zero st" | a badge | convention |

**Velocity as a band is the answer to the open question.** Velocity response
*is* the range of shapes an envelope can take, so draw that range: a static
envelope (velDepth 64) is a single line, a fully velocity-scaled one is a wide
band, and an inverse response (velDepth < 64) is a band opening the other way.
No new vocabulary, and it is quantitative rather than indicative — you can see
*how much* velocity does, which is exactly the question that sent us to the
manual in the first place.

That leaves only two arbitrary encodings, which is a good ratio. Time colour
should be ordered (a single-hue ramp, fast→slow), not categorical, since the
buckets are ordered.

### Beyond the grid

Small multiples first, because they are cheap and immediately useful. But the
grid is a layout, not a limit, and two things follow once the shapes are drawn:

- **Arrange by similarity, not category.** A grid asserts that column 3 means
  something. A force/beeswarm layout over the parameter space makes neighbours
  genuinely neighbours and lets clusters appear where the space is dense. The
  ecosystem already has beeswarm, circle-pack, force and hierarchy layouts.
- **Landmarks, not cells.** Zadar makes you pick from a list because it cannot
  show you the space. Showing the space makes library entries *landmarks* in a
  continuous field, so you can land **between** two shapes — which dissolves the
  "18 presets or 40?" question entirely, because the gaps become reachable.

Draw them **to a shared time axis** while you are at it: a matrix cell cannot say
that one shape is ten times longer than its neighbour, and `time` is the field
that most determines whether something reads as snappy.

**Counterweight, worth keeping honest:** Zadar's constraint buys something real —
two encoders means eyes-free selection mid-performance, and a beautiful map you
have to *look at* is worse than a list you can flick through blind. The `[` / `]`
walk already works; keep it as the fast path and let the map be where you go to
*find* something, not the only way to reach it.

## The interaction: direct manipulation, keys as the precision fallback

**AC:** *"i'd drop all the keyboard shortcuts and go to direct drag and drop on
the ADSR points for precise shaping OR select ADSR point and use arrow keys."*

Right, and it is a correction worth naming: the letter-selects-a-parameter scheme
shipped on 2026-08-09 was **a workaround for not having direct manipulation**.
`a`/`d`/`s`/`r` name abstract fields because there was nothing on screen to grab.
Once the curve has handles, the handles *are* the names.

The drawn polyline already has exactly the right breakpoints:

| handle | drag | edits |
|---|---|---|
| peak | horizontally | attack |
| sustain corner | horizontally / vertically | decay / sustain |
| tail end | horizontally | release |

Four parameters, three handles, two axes, no vocabulary at all.

Keys do not disappear — they **re-anchor**. Select a breakpoint, then arrows
nudge it; shift for coarse. That keeps precision and repeatability (a drag cannot
reliably hit exactly 30) without asking anyone to remember that `p` means depth.

`time` cannot be dragged — it is a 0..7 bucket, not a position — so it stays a
separate control. Since colour already encodes it, clicking the colour to cycle
is the obvious gesture: the indicator and the control become one thing.

## Hazards

- **One machine or two?** This note originally argued for a generic machine
  lowered onto the FH-2, to keep the ES-9 door open. AC's reading cuts the other
  way and may well be right:

  > "looking forward to a later push where we'd leverage the extra power of ES9's
  > free control of the envelope … perhaps at that point we might even decide to
  > have two different Selene envelope machines and do a very different design
  > there."

  The FH-2 gives a fixed ADSR with three curve bytes and a 5 V ceiling; a
  free-running ES-9 generator gives arbitrary contours, and Tides/Stages
  concepts (segments, looping, morphing) only make sense on the second. Forcing
  one surface over both would be the wrong kind of generality — the FH-2 machine
  would carry controls it cannot honour, and the ES-9 machine would inherit an
  ADSR shape it does not need. Two machines that share the *library* and the
  *drawing*, but not the parameter model, is the likelier answer. Decide when the
  ES-9 side is real, not before.
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

## Order of work

1. **Small multiples of the library**, drawn, with the encoding above. This is
   the Zadar idea minus its screen, and it makes everything after it easier to
   judge.
2. **Direct manipulation of the breakpoints**, keys re-anchored to a selected
   handle. Replaces the letter-shortcut scheme.
3. **Bank-level controls** (Ceis) — one gesture, eight related envelopes, drawn
   as a single figure so the relationship is what you manipulate.
4. **Similarity layout / continuous field**, once there are enough shapes for a
   grid to feel arbitrary.
5. **Tides morphs** — deferred until the surface has had live use and it is clear
   which of the eleven fields actually get reached for.
6. **The ES-9 machine** — a later push, probably its own design (see Hazards).
