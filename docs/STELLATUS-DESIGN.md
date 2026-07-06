# Stellatus — a circular, pattern-placed, stochastic sample re-sequencer

**Provisional name:** *Stellatus* (Abalistes **stellatus**, the *starry*
triggerfish) — a dark ring scattered with sample-stars; the stochastic walk
traces constellations between them. Alt candidate: *Xanthichthys* (the
crosshatch triggerfish — echoes the transition matrix). Rename is cheap.

**Status:** design sketch, 2026-07-06. The sixth Triggerfish instrument.
Jumping-off point: **Kymatica's Sector** (Jonatan Liljedahl) — take its key
ideas, *re-think them in Tidal/SuperDirt terms*, do not clone. Sector never got
Link and languished; ours is Link-native + reef-lockstep from birth.

**Lineage:** kin to **Odonus** (generative, N read-heads, reef co-sim), not to
Sufflamen's linear bench. Shares only the *substrate*: a buffer sliced into
arcs, the reef generative/lockstep engine, and Vetula as pitch authority.
Serves the same invariants as the rest of the family: **Lepidoptera** textual
save, drop-in to other PureScript programs, **Tidal mini-notation where it is
the best choice**.

---

## The one reframe that changes everything: the ring IS a Tidal cycle

Sector slices a buffer into *even* segments on a circle. We don't. **The ring is
one cycle; the samples are placed on it by a pattern.** Every Sector idea then
falls into Tidal's lap:

| Sector | Stellatus reframe | Tidal / suite |
|---|---|---|
| even slices on a circle | **placement = a pattern**; arc onset = event time, arc length = event span | `s "bd sn hh cp"`, `s "bd(3,8)"`, `chop n` is the degenerate even case |
| advance clocks (the staircase icons) | **polyrhythmic read-heads** over the ring | Odonus's N-heads / Selene polysignals; rate/curve/gate per head |
| WRP page — 4 weighted warp slots (%, curve, direction) | **a probability-weighted bag of per-hit transforms** (reverse/ratchet/gain/pitch) | `sometimesBy`, `someCyclesBy`, weighted `wchoose`; deterministic under a reef seed ⇒ reproducible glitch |
| MAP page — a transition *matrix* | **a first-class directed graph over arcs**; a run is a **random walk**; jumps drawn as chords across the circle | music-as-graph / walk-as-composition; the Vetula circle-jump-line idiom; the music↔dataviz bridge |

Even-slicing becomes the *degenerate* placement; chop/striate (from Sufflamen)
are just two placements among infinitely many. **The visual is the semantics** —
you read the mini-notation by looking at the ring (the Hylograph thesis).

## Pitch — the musicality seam (Morphagene × Vetula)

Sector glitches but rarely *sings*. Borrow Morphagene's signed vari-speed for
per-arc pitch, drive it with a pattern (`# speed` / `# note`), **then quantize to
Vetula's live chord** via the existing `realize :: Index -> Pitch` /
honour-the-source machinery. Glitch that stays *in key with whatever Vetula
holds* — the difference between a noise toy and something you play a set on.

## Aesthetic — its own room in the house

Liljedahl's design is dark, minimal, high-contrast, geometric. The family
tolerates per-instrument identities (Odonus's test-gear panel, Sufflamen's tape
bench). Stellatus gets the **dark radar/oscilloscope**: rainbow arcs and a hairy
radial waveform on near-black, heads as sweeping rays, jumps as bright chords.
A deliberate departure from the Hainbach beige, earned by the circular surface.

## Lepidoptera shape

`stellatusWith { placement, heads, warpBag, jumpGraph, pitch, seed }` — a named
authored-config value:
- `placement` — the source pattern (mini-notation) → arcs.
- `heads` — read-heads: `[{ rate, curve, gate }]`.
- `warpBag` — weighted `[{ weight, reverse, pitch, ratchet, gain, curve }]`.
- `jumpGraph` — transition weights over arcs (+ a global jump probability).
- `pitch` — scatter amount + `quantizeTo` (Free | VetulaChord).
- `seed` — the reef seed; a whole performance is reproducible from it.
Round-trips, drops into other PS programs, registers with the TIDAL library
manager like the other instruments. The inner mini-notation stays OG-Tidal-valid
(constraint 1 near-free).

## Lockstep

Heads, warp choices, and the walk are all **tick-tagged deterministic inputs**
(seed + Link phase) → reef co-sim, byte-identical node↔BEAM. A glitch sequence
is reproducible and shareable — the exact capability the Link-less original
lacked.

## Build order

- **S0 — the ring (this prototype).** Dark circle; placement presets (Even n /
  Euclid k n) → rainbow arcs; hairy radial waveform; one advancing head (+ an
  optional second at a ratio, to show poly-clocks); arcs flash on crossing; a
  basic **jump** (seeded probability → a chord drawn across the circle — the
  money shot); per-arc warp glyph (reverse/pitch/ratchet, seeded); shake =
  re-seed. Pure visualizer, no audio (rig-only, honest chip). Reuses Sufflamen's
  timer/knob/hash/chip idioms; the circular geometry is the new part.
- **S1 — the read-order engine as data.** Placement from real mini-notation
  (wire the `Tidal` parser); heads as typed read-clocks; warpBag + jumpGraph as
  the model; `stellatusWith` Lepidoptera round-trip + library registration.
- **S2 — the MAP as a Hylograph graph.** Edit jump weights by drawing chords;
  the walk animates the graph. Music-as-graph, first-class.
- **S3 — pitch to Vetula.** Per-arc pitch pattern quantized to the live chord;
  reef-lockstep the walk for on-rig reproducibility.
- **S4 — sound.** Emit `/dirt/play` (`begin/end/speed/n # …`) via the same C/B
  SuperDirt gates Sufflamen needs; or the future Rust looper.

## Out of scope for S0

No audio, no parser (presets stand in for placement), no real graph editor
(a single jump-probability + seeded target), no Vetula wiring (a pitch-scatter
number, `quantize → Vetula` shown greyed). Prove the *ring is a cycle* first.
