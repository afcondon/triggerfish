# The output-backward view — routing read from the jack

**Status: design. AC, 2026-08-08 evening, after the unified router landed.**

> "I think from my experience with the terrible HTML interfaces of the Expert
> Sleepers devices one lesson we might take is to actually work backward from the
> port where the output appears, perhaps allowing an origin-forward as well as
> output-backward view."

> "it would also put the user in a position to resolve it by disconnecting one of
> them."

The router built today is **origin-forward**: a row per source, listing where it
goes. That is the right shape for the question "I am editing Odonus head II —
where does it come out?" It is the wrong shape for the question you actually ask
standing at the rack, which is:

> **What is supposed to be driving jack 3, and is anything?**

Both views are over the same table. Neither is a mode of the other; they are two
projections of one relation, and the relation is many-to-many in both directions.

## Why backward is the stronger of the two

**Contention only exists on the output side.** Two sources can want one jack; one
source wanting two jacks is not a conflict, it is the feature. So every conflict
is invisible in the forward view *by construction* — you would have to hold four
rows in your head and notice that two of them name the same hardware. That is
exactly the failure that shipped this evening and had to be found by reading
allocation code:

- polyenv allocates MCV 0–7, envelope N on MCV N−1, output at jack N
- the drum breakout's note-filtered triggers use MCV 0–3 for jacks 1–4
- both are `mcv = jack - 1`

So an FH-2 envelope on slot 3 and an FH-2 gate on jack 3 are **the same MCV
driving the same jack**, mutually exclusive, last writer wins. The forward view
showed four healthy Odonus rows and four healthy drum rows and said nothing. The
backward view would have shown jack 3 with two claimants and no further argument
needed.

**And it is where a conflict can be resolved.** This is AC's point and it is the
decisive one. In the forward view you can remove a leg from a source you happen
to be looking at; you cannot see what you are freeing it *for*. At the jack, both
claimants are present, so "drop this one, keep that one" is a local edit at the
point of contention. The forward view can express the *consequence* of a
decision; only the backward view can express the decision.

**It is also the honest boundary of what Triggerfish knows.** The router can name
the jack a signal leaves by. It cannot know what patch cable is in it. So the
jack is the last thing this application can say anything true about — which makes
it a natural edge for the model, and a natural place to let the *user* write down
what the software cannot know (see "patch notes" below).

## The coordinate system already exists

`Triggerfish.Rig` models the rig as negative space: how many 8-wide blocks each
expander family contributes, so a destination is only offerable if the hardware
that would receive it is patched in.

```purescript
defaultRig =
  { es9: Just { gtBlocks: 2, cvBlocks: 1 }
  , fh2: Just { banks: 2 }
  , midiChannels: 16
  }
```

That is precisely the axis an output-backward view needs, and it is already
written. The backward view is not new modelling — it is `Rig`'s enumeration
rendered as rows, with the routing table joined onto it. Expanding each block
into its jacks (`ES-9 GT 0` → gates 1–8) gives the row set directly.

`Routing.Model.claims` is already an output-indexed projection — it groups legs
by the hardware slot they consume and reports who consumes it. It currently sits
as a summary line at the bottom of a source-indexed panel, which is the wrong
place for it. **This view is what `claims` grows into.**

## What a row shows

One row per physical output the rig actually has:

| jack | claimed by | traffic | patched to |
|---|---|---|---|
| FH-2 1 | Odonus I (env) **·** Drums BD (gate) ⚠ | ● 631 on / 629 off · v40–100 | *"Maths ch1 rise"* |
| FH-2 2 | Odonus II (env) | ● 402/400 · v52–118 | |
| FH-2 3 | — | — | |
| ES-9 GT0 4 | Selene euclid 4 | (needs rig) | |

Four columns, four different kinds of fact, and keeping them distinct is the
whole value:

- **claimed by** — what the table says should drive it. Two entries is a
  conflict; zero is a free jack, which is its own useful answer.
- **traffic** — what the monitor actually observed. Declared and observed fail
  independently: a route can be perfectly reachable and never carry a note
  because the machine never armed, and from the rack those look identical.
- **patched to** — free text the user writes. Software cannot know this, and it
  is the thing you most want when you come back to a rig after a week.

## Free jacks are a first-class answer

Worth stating separately because the forward view cannot express it at all.
"Which outputs are unspoken for?" is a question you ask constantly while
patching, and today it requires mentally subtracting the union of every source's
legs from the rig's inventory. In the backward view it is the rows with an empty
claim column, and adding a route from there — *"give this jack to Odonus III"* —
is the natural gesture at that moment.

That also inverts the add flow usefully: the forward view adds a destination to a
source, the backward view assigns a source to a destination. Same edit, and the
one that fits depends on which you are holding fixed.

## Encoding — the other half of AC's note

> "we should colour code the MIDI, FH2, ES9 output paths and Gate/CV so that the
> structure of the complex config is more apparent. Doesn't have to be colour
> actually, there may be other data viz techniques we could use."

Right, and the trap is that these are **two independent axes**, not one:

- **device** — MIDI / FH-2 / ES-9 / continuo
- **signal kind** — note / gate / CV / envelope

Colour alone would conflate them, and the conflation is exactly the confusion
worth removing: "FH-2 gate" and "ES-9 gate" are the same *kind* on different
*devices*, and "FH-2 gate" and "FH-2 envelope" are the same device with different
kinds — and, as tonight proved, the same MCV.

So: **colour carries device** (it is the coarser grouping, and the backward view
already blocks rows by device), and **signal kind is carried by form** — a glyph,
a rule weight, a cell shape. Redundant encoding on the row grouping is fine;
overloading one channel with two variables is not.

Palette should follow the house style — restrained, Swiss, hue doing categorical
work rather than decorative work, and it must survive being the only thing
distinguishing two adjacent rows.

## Both views, one table

Not a replacement. The forward view is right while editing a machine (it lives in
the machine's own mental frame); the backward view is right while patching or
diagnosing. Same `Routing.Model.Table` underneath — this is a rendering decision,
not a model change, which is the main argument for doing it at all: the model
built today already supports it, and if it did not, that would be evidence the
model was wrong.

A toggle between them, or side-by-side if the width allows. Selecting a source in
one should highlight its jacks in the other; selecting a jack should highlight
its claimants.

## Sequencing

1. **Render the backward table read-only**, from `Rig` × `claims` × monitor. This
   alone would have caught the MCV collision, and it is nearly free — every input
   already exists.
2. **Move the claims summary into it** and delete it from the forward panel. One
   home for output-indexed facts.
3. **Make it editable**: assign / unassign a source at the jack, which is the
   conflict-resolution gesture that motivated the whole thing.
4. **Add the encoding** (device colour, kind glyph) once the rows are stable —
   design it against a real populated table, not a mock.
5. **Patch notes** — free text per jack, persisted with the routing table. Small,
   and probably the feature that gets used most.

## Watch for

- **MIDI channels are not scarce and do not belong in the same table the same
  way.** Sixteen channels × N ports is a large, cheap space where sharing is
  legal and often wanted; FH-2 MCVs and ES-9 gates are small, contended, physical
  things. Listing them identically would imply a scarcity MIDI does not have and
  bury the rows that matter. Probably: hardware jacks are the table, MIDI is a
  compact appendix.
- **`Rig.defaultRig` is hand-maintained.** The backward view makes the rig
  inventory load-bearing UI rather than a filter on a menu, so a wrong block
  count becomes a visibly wrong table — rows for hardware that is not there. That
  is an argument for the discovery handshake `Rig` already contemplates, and
  until then for the view stating plainly that the inventory is declared, not
  detected.
- **The FH-2's MCV indirection must be shown, not hidden.** The row is the jack,
  but the contended resource is the MCV, and the two are related by
  `mcv = jack - 1` only for the families we currently emit. Showing the MCV
  alongside the jack keeps the reason for a conflict legible instead of making it
  look like a coincidence.
- **Do not let it become a second editor of routing truth.** Same table, two
  projections. The moment either view holds state the other cannot see, this is
  the bank-coherence problem again, and the whole point of today was removing one
  of those.
