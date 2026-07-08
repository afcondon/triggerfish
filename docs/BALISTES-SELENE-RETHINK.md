# Balistes / Selene rethink — control-space navigation + the trigger-lane move

*Design note, 2026-06-27. Supersedes the "Balistes = Grids + a bolted-on Tidal
drum machine" shape that landed in `2ee8abb`/`4aaddce`.*

## The cramp

Balistes today is two instruments wearing one panel:

1. a **faithful Grids** — X/Y morph pad, three densities (BD/SD/HH), randomness;
   byte-identical to BEAM `balistes_voice` (the differential-conformance core);
2. a **general mini-notation trigger engine** — the Tidal kit lanes + the OH
   bridge + routing patterns — which *happens* to be pointed at drum notes.

The second is what feels half-built, because it's a general thing forced into a
drum-shaped hole. The rig is CV/gate and multi-out at heart; "one drum kit on two
MIDI channels" is a software-era constraint, not a rig constraint. The fixed
notes, the single extra channel, the 1/16-note OH ceiling, the FOCUS toggle that
only matters when routing is heavy — all symptoms of the same cramp.

## The decomposition — one idea per instrument

**Balistes → Grids, deepened.** Shed the Tidal kit and the OH hack. It becomes
*the* Grids instrument: the morphing pad, interpolation-as-visualization,
extended only *in genre*. Its multi-out is the Grids voice set, not an arbitrary
kit.

**Selene → the home of authored + generative signal sources.** The mini-notation
trigger lanes become a new GenKind beside polylfo / polyeuclid / polyclock /
polypresetnote — "authored trigger pattern." A trigger lane is **not** a drum
lane: the same `bd*4` can fire a drum, gate an envelope, clock Lubadh, advance a
sequencer, strike Arbhar. Routing-by-name generalizes to routing-to-target.

This also *advances Selene*, which today has no scheduler and no output path:
moving the working Tidal-lane engine (parser, meter, onsets, MIDI scheduling)
there gives Selene its first real playable generator. Two birds.

### Why this is the right "16 outputs"

Selene's model is already "a stack of **destinations**, each a group of eight
signals **bound to a physical target**," fanning out to ES-9 buses (CV/gate) and
MIDI. The multi-output idea isn't something to bolt onto Balistes — it's Selene's
native shape. A block is **8 internally but routes to a target that consumes what
it consumes**: 4 for vpme QuadDrum / Squarp Rample, 8 for an ES-9 destination.
Fan-out is by target capacity. And because the BEAM Selene polysignals are
*already* group-of-8 generators, the trigger-lane GenKind **retrofits straight
onto the BEAM instrument** — virtual editor and real engine from one shape.

## The open hat, as Emilie would have done it

OG Grids never distinguished closed/open hat; our bolted-on OH lane only offered
1/16 options while the Grids HH ticks faster. The elegant, Grids-native fix is
**not** a second lane — it's a threshold that slides down the HH level landscape.

Grids already computes a `level` (0–255) per hat step; the stressed beats *are*
the high-level ones (accent is just `level > 192`). So:

- One **OPEN** dial sets a second boundary on the same landscape. A hat fires
  closed normally; if its level is *above the open boundary*, it fires **open**.
- The boundary **starts at the accent line and descends** as you turn the dial —
  the loudest, most-stressed hats open *first* (how a real player works the
  pedal), and opening further recruits more modest hats.
- **Open chokes closed** on that step (true hi-hat behaviour); opens get a longer
  gate.

Pure Grids: a continuous threshold on an interpolated field, coupled to the X/Y
morph for free — move the cursor and the open hats follow the accent geography.
It also dissolves the 1/16 ceiling: with OH back *inside* the HH stream it runs at
the engine's full rate (ratchets already give 1/32 bursts where wanted). One knob
replaces the whole OH apparatus and is more expressive.

## The unification: presets, fills, risers, sequencing are one primitive

Four ideas that look separate are one:

| Gesture | What it is | When |
|---|---|---|
| **Preset** | a saved point in parameter space (X/Y + densities + open + push) | recalled manually |
| **Fill** | jump to a preset, **hold one bar, revert** | conditional — every N bars |
| **Riser** | **glide** from here to a target preset over N bars | triggered / scheduled |
| **Sequence** | a pattern of presets over time | song structure |

They are one primitive — **navigation through Grids' control space** — with four
gestures: *a target in parameter space × how you reach it (jump / glide /
hold-then-revert) × when (manual / bar-condition / sequenced).*

This is Grids' thesis. Emilie's instrument let you fly the cursor by hand through
drum-map space; Triggerfish's "past the hardware" move is to **record and
automate flight paths** — points, momentary detours, glides, routes. Build the one
primitive and all four gestures fall out, plus a slow 32-bar morph between two
presets as an arrangement.

The UI gesture is "buttons that fire envelopes, like programming an envelope in
the modular," and it **generalizes beyond Balistes**: one reusable
*ramp-a-scalar-from-A→B-over-N-bars-on-this-trigger* widget, drawn as a tiny
envelope next to the knob, drives Grids density *and* a Euclidean `k` in
Selene/Odonus. Very Rams.

## The deepened Balistes is therefore

1. **OPEN dial** (dissolves OH, the Emilie-grade hat).
2. **Control-space navigation** — preset / fill / riser / sequence as one
   mechanism, with the fire-an-envelope button row.
3. **Accent + fill outputs** on their own buses (multi-out); accent already
   computed, today only bumping velocity.
4. Ratchets stay (in-genre, Elektron-style retrig).

…and the trigger-lane engine leaves for Selene as a blocks-of-8, target-sized
GenKind that also lands on BEAM.

### Ideas deliberately refused

To protect the one big idea (navigable control space), Balistes does **not** grow
full p-locks, deep conditional-trig matrices, or Torso's whole transform stack.
Worth stealing because they *fit*: per-step probability (Patterning, and Grids is
already probabilistic) and conditional fill (Elektron) — both expressible inside
the control-space frame.

## The conformance boundary

The Grids core stays byte-identical to BEAM `balistes_voice`. Accent-out makes it
*more* faithful (accent is in the firmware). OPEN, presets, fills, risers,
sequencing, ratchets, push are all Triggerfish **overlay** — exactly the layer
that already carries ratchets and Dilla push. The engine ignores the overlay; the
component applies it on emit.

## PolyTrig refinements (2026-06-27, after the representation increment)

Three things became clear once the trigger lanes were on screen.

### a. Named jacks + lane-spanning route lines (coexisting)

A per-jack-only model loses the *canonical* Tidal idiom — `"bd sn cp sn"`, one
string whose atoms name **different** voices. So a PolyTrig block decouples
*outputs* from *authoring*:

- the **8 jacks** are the outputs — each a **name** (`bd`, `sn`, …) + a
  note/value + an optional **per-jack** pattern (`bd "bd*2"`);
- **route lines** are lane-spanning patterns (`route "bd sn cp sn"`) whose atoms
  fire jacks **by name**.

Both stack at playback. This is the old Balistes pad-lanes-plus-routes model,
generalized — and it **preserves the blocks-of-8 / BEAM retrofit**, because a
route line is just *authoring sugar that compiles down to the 8 per-jack onset
streams* before anything leaves the box. The only model change: a trig slot
needs its **name** back (so `bd` can address it).

### b. PolyEuclid vs PolyTrig = delegated vs streamed execution

The two kinds aren't "Euclid vs arbitrary" — they're split by *who runs the
pattern*, which is the rack's real organizing principle:

| | PolyEuclid | PolyTrig |
|---|---|---|
| FH-2 **generates** it (from a `{beats,steps,rate}` config) | ✅ delegated | ❌ never |
| FH-2 **receives** streamed gates (as a dumb expander) | ✅ | ✅ |
| BEAM **computes + streams** | ✅ (optional) | ✅ (required) |

PolyEuclid is the **delegatable** form: the FH-2 self-generates it, sample-
accurate, zero BEAM load, *runs even if the computer stops*. PolyTrig is
arbitrary mini-notation — no hardware can parse it — so it's **always computed
and streamed**. "Can't be sent to the FH-2 for execution" is PolyTrig's
*defining* property, not an incidental limit. They must stay distinct. A
destination therefore carries a notion of **delegated vs streamed**, decided by
*whether the target can execute the kind* — the same capability-of-target
thinking that runs through Bosun. The scheduler honors it: PolyEuclid→FH-2 = a
config; everything else = computed gates.

### c. Structure-driven viz — ring the Euclids

Principle: **the viz follows the pattern's structure, not the GenKind.** A Euclid
is a ring wherever it lives. So a PolyTrig jack whose source parses to a *pure*
Euclid (`x(3,8)`) draws as the same ring PolyEuclid uses; a sequence draws the
linear step row. A mixed block shows some rings, some rows — *informative*, not
inconsistent. Bonus: a ringed trig jack visibly says "I'm a Euclid — I could be
*promoted* to a delegatable PolyEuclid," making (b) an affordance.

## The real-pattern library — Grids as one member of a Pattern family

*(AC, 2026-06-29 — resolved the placement, spine landed.)* AC is transcribing
the ~30 starter beats from *The Secrets of Dance Music Production* in Ableton
and exporting MIDI, aiming at the Grids promise — *reliable instantaneous
results without programming, with some tweaking (and maybe tweening)*. The
first export (`lo tempo house 110.mid`) settled the design.

**The unifying move (AC):** a Balistes panel plays a **Pattern**, and Grids is
just *one* member — the special, generative, mutatable one. This dissolves the
old "two instruments in one panel" cramp: there's now a single abstraction.

- **Grids** — the X/Y morph engine. Owns the CONTROL column. Conformance core
  untouched.
- **Fixed rhythm** — a literal `lane × step` velocity grid. A labelled loop you
  recall instantly and edit; it doesn't morph.

**The canonical 16-lane kit** (`Pattern.canonKit`: BD SD CP RS CH PH OH LT MT HT
RD RB CR CW TB SH, GM percussion notes) is the shared coordinate system every
fixed rhythm is laid against, so patterns stack, swap samples, and sequence
against the same rows. Import maps each source MIDI note onto a lane by GM
number (`laneFromNote`). Samples are swapped freely downstream (Ableton /
SuperDirt / modular) — the notes are just the wire.

**The bank** is `◆ GRIDS` plus the library rhythms; clicking a chip switches
what plays. **CONTROL shows iff Grids is active** — a fixed rhythm has no
control space, so that column drops and PATTERN takes the room.

### Spine landed (2026-06-29)

- `Triggerfish.Balistes.Pattern` — `KitLane`/`canonKit`/`FixedPattern`
  (`{name, steps, grid :: Array (Array Int)}`, velocity 0..127), `velAt`,
  `firesAt`, `usedLanes`, `laneFromNote`, `buildGrid`, and `houseLoTempo110`
  decoded byte-faithfully from the MIDI (six voices: BD/CP/OH/HT/RD/CR).
- `Component`: `Active = AGrids | AFixed Int` + `library`; the pattern switcher
  chips; `fixedBody`/`fixedSvg` (folds to used lanes, label gutter, velocity =
  cell intensity, per-lane gate so hats/cymbals ring); the Step handler emits
  the fixed grid verbatim over MIDI (step = `tick.index mod steps`); CONTROL and
  the Grids-specific transport readouts hide for fixed.

### Still to build (the order AC will pick from)

1. **In-app pattern editor** — click cells, name/assign lanes, expand the
   folded view to the full 16 (greyed where unnamed). This *is* the import-
   labelling UI and the extensibility story (starters are meant to be tweaked).
2. **MIDI file import** — drop a `.mid`, map notes→lanes, save to the library.
3. **Sequence across patterns** — broaden the sequencer's unit from "snapshot
   slot" to "bank slot" so it sequences bars of Grids *and* bars of rhythms
   (AC's explicit goal). A scene becomes Grids-control-point | fixed-rhythm.
4. **The morph experiment** — lay rhythms on the 5×5 (or a 2-pattern A/B) and
   let the bilinear engine interpolate between real beats. Grids' interpolation
   is voice-count-agnostic, so an N-voice level field morphs for free. Cheap to
   try once ≥2 patterns share the kit; may smear rather than groove — that's the
   thing to find out.

The snapshot model stays agnostic to where a control point came from (live
gesture vs imported), so it folds into (3) without rework.

## Build order

1. **Selene extraction** *(first — de-crufts the Balistes panel and unblocks
   Selene's first scheduled output).* Lift the trigger-lane engine into a Selene
   trigger-lane GenKind; generalize off drum notes to target-routed triggers;
   blocks of 8, target-sized. **Representation increment landed**; PolyTrig
   refinements (a–c) next, then the scheduler. Remove the Tidal kit + routing +
   FOCUS from Balistes once Selene can play.
2. **OPEN dial** *(the fun, self-contained win)* — replace the OH pad lane with
   the descending-threshold open hat + choke.
3. **Control-space navigation** — the preset/fill/riser/sequence primitive + the
   envelope-on-a-scalar widget; accent + fill multi-out.

## Three-drum-tabs — the drum slice comes back to Balistes (2026-07-08)

*(AC, 2026-07-08 — a **deliberate, considered reversal** of build-order #1
above, for the drum-flavored slice only. Made possible by the destination
model in `PLAN-midi-routing.md`, which the June decomposition didn't have yet.)*

The June extraction was right that "a trigger lane is not a drum lane" — but it
resolved the tension by *location* (move the engine to Selene). The routing
model resolves it better by **destination binding**: one trigger engine, two
authoring surfaces, each seeding a different default destination.

- **Balistes** becomes a three-tab drum page — three ways to *specify* drums,
  **one active at a time** (the user sees and hears only the visible tab; the
  invariant is free because `Active` is already a single value). Each tab is
  **self-contained** — its own editor *and* its own control column:

  | Tab (working name during build) | Final label (post-hoc rename) | What it is |
  |---|---|---|
  | `AGrids` — MI Grids port | **Mutable** | the firmware-faithful X/Y morph engine + CONTROL. Already exists. |
  | `AFixed` — rhythm library | **Grids** | user-programmed `lane × step` patterns (`houseLoTempo110`, the switcher chips). Already exists. |
  | `ASelene` — relocated POLYTRIG | **Tidal** | Tidal mini-notation trigger patterns pointed at drum voices. Reuses the engine already extracted to Selene. New tab. |

  All three default to **MIDI Ch 10** and very likely **also** fan out to
  modular trigger outputs simultaneously (see the fan-out asymmetry in the plan).

- **Selene keeps its general role** from the June doc: authored + generative
  modular signal sources (LFOs, euclid triggers) bound to **FH-2 / ES-9**
  outputs, presented in *those* terms — never MIDI channels. Only the
  **drum-flavored** use of the trigger engine relocates; the general
  target-routed trigger sources stay in Selene.

**Rename discipline:** build with the honest current names (`AGrids`/`AFixed` +
a new `ASelene`); do the cosmetic Grids→Mutable / Patterns→Grids /
Selene-drums→Tidal relabel as a single pass *after* everything works. The word
"Grids" is being recycled onto a different owner, so doing the swap mid-refactor
would make the code lie about itself.

**Effort:** two of three tabs already exist; the work is (a) the tabbed shell
with the one-active invariant, (b) bringing the POLYTRIG drum surface back as
`ASelene`, (c) the shared destination binding (Ch 10 + optional modular). The
channel/routing half lives in `PLAN-midi-routing.md`.
