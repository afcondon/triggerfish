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

## The loopback view — show the ports, driven by the daemons

> "we should add a loopback destination for all the ports so that you can open
> the modal and see the 5 groups of 8 ports and the colours that should be on
> them (approximately) being driven by the exact same daemons that are driving
> the ES9 and FH2. […] there's a big difference between the ES9 whose output we
> can directly show and the FH2 where we would be impersonating the FH2 and that
> runs the risk of modelling IT wrong leading to more confusion. but we could at
> least show what we're expecting to be output."

This is the backward view with a live signal column, and the caveat AC raises
is the thing that decides whether it helps or hurts.

**Five groups of eight is already the model.** `Rig.defaultRig` says ES-9 main
(8) + two gate blocks (16) + one CV block (8), FH-2 two banks (16). The row set
falls straight out of it.

### Three provenances, and they must not look alike

The critical distinction, which is AC's caveat generalised:

| class | source | what it means |
|---|---|---|
| **observed, authoritative** | es9-daemon | it *generates* the signal (polylfo / polyclock / polyeuclid run inside it), so it knows the instantaneous value. Ask and it tells you. |
| **observed, at the wire** | the WebMIDI tap (`Routing.Monitor`) | notes actually sent. Not the voltage, but a true observation of traffic. |
| **predicted** | FH-2 | the hardware generates from a config we pushed. We know the CONFIG. We do not know the output, and computing one means reimplementing the FH-2's envelope engine. |

**A predicted display that looks like an observed one is the exact failure this
whole effort exists to remove.** A panel showing a confident FH-2 envelope while
the module is powered down would be worse than no panel — it is the "rig reports
healthy while a load-bearing thing is dead" shape, rebuilt deliberately and in
colour.

So provenance is a **required visual channel**, not a nicety: observed values
solid, predicted values ghosted/hatched and labelled *expected*. If only one
thing survives from this section, it is that.

### Liveness is separate from prediction, and cheap

This morning (2026-08-09) makes the case. The modular was switched off; the
FH-2's USB port still enumerated; the router happily showed every FH-2 route as
`ok`. `Reach = Reachable` only ever meant "a port with that name exists".

But the daemons can answer better:

- **fh2-daemon `device-status`** — a SysEx round trip. It answered `OK device-ok
  firmware=v2.0.0` with the rig on, and timed out with it off. That is a true
  liveness test and it costs one socket call.
- **es9-daemon** already knows its generator state and its claims.

So the backward view gets a **liveness row per device**, polled, and a dead
device greys its whole block. That is worth building *before* any signal
rendering: it would have saved this morning outright, and it is a few lines.

### What to show per class

- **ES-9** — ask the daemon. Real values, solid. This is the part with no
  modelling risk at all, and on its own it justifies the view.
- **MIDI** — the tap already gives on/off counts, velocity range and recency per
  destination. Solid.
- **FH-2** — show the **config**, not a simulated waveform: "env 1: A0 D30 S0
  R20, time 200ms, bipolar ±5V, velDepth 96". A little ADSR *sketch* drawn from
  those numbers is fine and useful, provided it is unmistakably a diagram of the
  settings rather than a scope trace. Never claim a value.

That last line is the honest reading of AC's "we could at least show what we're
expecting to be output" — the expectation is worth showing precisely because
comparing it against the rack is how you find the disagreement. It just has to
be labelled as an expectation.

### Why this is the right shape for the backward view

The forward view asks "where does this go". The backward view as first sketched
asks "what claims this jack". The loopback column adds "and what is on it right
now" — which is the question you are actually holding when you put a probe on a
jack, and the one that closes the loop between the routing table, the daemons and
the rack.

It also makes a whole class of bug self-evident: a jack with a claimant, a live
device, and no signal is a different fault from a jack with no claimant, and both
are different from a jack whose device is dark.

## Encoding — the other half of AC's note

> "we should colour code the MIDI, FH2, ES9 output paths and Gate/CV so that the
> structure of the complex config is more apparent. Doesn't have to be colour
> actually, there may be other data viz techniques we could use."

Right, and the trap is that these are **two independent axes**, not one:

- **device** — MIDI / FH-2 / ES-9 / continuo
- **signal kind** — note / gate / CV / envelope
- **provenance** — observed / predicted / dead (see the loopback section)

Colour alone would conflate them, and the conflation is exactly the confusion
worth removing: "FH-2 gate" and "ES-9 gate" are the same *kind* on different
*devices*, and "FH-2 gate" and "FH-2 envelope" are the same device with different
kinds — and, as tonight proved, the same MCV.

So: **colour carries device** (it is the coarser grouping, and the backward view
already blocks rows by device), **signal kind is carried by form** — a glyph, a
rule weight, a cell shape — and **provenance by fill**: solid for observed,
hatched or outlined for predicted, greyed for a device that is not answering.
Redundant encoding on the row grouping is fine; overloading one channel with two
variables is not.

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

> **Superseded in part — see `DESIGN-selene-companion.md` (2026-08-09).** Step 0
> below is wrong: Triggerfish has no path to the daemons at all. `device-status`
> lives on `~/.fh2/control.sock`, a browser cannot open a Unix socket, and the
> only rig channel here is the BEAM WebSocket on `:3012`. Liveness needs a
> server-side proxy, which is one of the arguments for extracting Selene as a
> companion app with its own API. **Build step 1 first**; it needs nothing new.

0. **Device liveness first.** Poll `device-status` on fh2-daemon and the
   equivalent on es9-daemon; grey a dark device's block and downgrade its routes
   from `ok` to unknown. Cheapest item here and it would have saved a confused
   morning on 2026-08-09.
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
6. **The loopback column** — ES-9 live values from the daemon, FH-2 config sketch
   marked as expected, MIDI traffic from the tap. Provenance encoded in fill.

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
