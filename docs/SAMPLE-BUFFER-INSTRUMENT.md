# A sample-and-buffer virtual instrument — an idea captured 2026-07-02

**Status:** Idea capture during concurrent coding (another Claude was
in flight on the engine repos). Not a plan. To be folded into
`purerl-tidal/docs/` or a `triggerfish` doc once the repos are quiet
enough to coordinate.

**Reader:** Andrew, and the next Claude up. Print, mull, push back.

**Companions:**
- `triggerfish/BRIEF.md` — the "copy the hardware, then transcend it"
  principle, and the SuperDirt-pane parking-lot entry this note
  extends.
- `polysignal-algebra-2026-05-23.md` (→ absorbed into
  `purerl-tidal/docs/slab-c-plan.md`) — the spec-vs-realiser split
  with static capability checking that this note reuses for audio.
- The sequencer-vocabulary insight (memory:
  `project-virtual-modules-on-beam`): hardware modules navigate stored
  state via patched gates; we replace that with a Pattern of typed
  navigation actions. Here the stored state is an *audio buffer*.

**Where it sits in the family (as of 2026-07-02):** Odonus (melody,
lockstep co-sim complete), Balistes (rhythm, lockstep + live knob sync
landed 2026-07-02), Selene (modulation, next in the lockstep queue),
Vetula (harmony, standalone → Triggerfish pane). This note is the
missing quadrant: **audio as material**.

---

## The prompt

Andrew's note: *"Loopy Pro on Ableton / Lubadh / Arbhar / Morphagene"*
— four wildly different visions of things that record and play loops.
Is there an abstraction that brings them into a live-coding /
generative / compositional setting under common controls?

## Decision 1 — two instruments, not one

Split the four references into **two instruments** and design them
separately:

1. **The experimental hardware instrument** — Lubadh, Arbhar,
   Morphagene. Texture-mangling, seconds-time, CV/gate control plane,
   buffers trapped in hardware. This note is mostly about this one.
2. **The loop instrument** — Loopy Pro, Ableton, and a future custom
   Rust recorder/looper. Song-structure, bar-time, MIDI control
   plane, buffers in software we can eventually own. Designed later,
   separately.

The split is principled, not just pragmatic: the groups differ on time
regime (seconds vs bars), control plane (CV vs MIDI), musical role
(texture vs structure), and buffer residence (hardware vs software).
The Rust looper belongs to group 2 — it is the realiser where the
loop instrument's buffers finally live inside the workbench.

## The core abstraction — split buffers from heads

Hardware welds a *buffer* to a particular *read/write mechanism*
inside each box; you can never point Arbhar's grain cloud at Lubadh's
tape. Decomposed, all four references are points in one small space:

| | Write policy | Buffer topology | Read policy | Sound-on-sound |
|---|---|---|---|---|
| **Loopy Pro** | bar-quantized punch | clip (fixed loop) | 1 head/clip, rate 1, launch-quantized | overdub layers |
| **Lubadh** | free, tape-style | circular tape | 1 continuous head, slewed varispeed ± | dub decay coefficient |
| **Morphagene** | append to reel | reel + **splices** | 1 head, jumps between splices, gene-windowed | SOS with morph |
| **Arbhar** | capture gate (+ onset detect) | 6 layer banks | **many ephemeral heads** (grains: position, size, spray, pitch, density) | layering |

Unifying spec vocabulary:

- **Buffer** — shared audio state, owned by no reader
- **Splice** — a *named window* into a buffer
- **ReadHead** — position, signed rate (reverse = negative), window,
  envelope
- **GrainCloud** — a *Pattern of ephemeral ReadHead spawns* (density
  is an event rate — literally Tidal's event model)
- **Capture** — a patterned write event
- **SOS** — a decay coefficient on write (Lubadh dub, Loopy overdub,
  Morphagene SOS are the same parameter)

This rhymes with the family: Odonus is *N playheads over one shared
grid*; this is *N read-heads over one shared buffer* — the same
architectural move lifted from a step grid to an audio timeline.

**The elegant functional move (long game, mostly for the loop
instrument / Rust realiser):** run an always-recording ring buffer per
input; then a splice is a *pure value* — a reference to a time window
in the past — and "capture" is naming, not mutating. Splices become
first-class, pattern-addressable. `splice "verse chorus*2 verse"` —
mini-notation over named audio windows — falls out. (Loopy's
retrospective record and Arbhar's continuous capture gesture at this;
hardware realisers mostly lack the capability, which the checker
catches.)

**Prior-art honesty:** original TidalCycles + SuperDirt already solved
the *playback* half (`begin`/`end`/`speed`, `chop`, `striate`,
`loopAt` are patterned read-heads over named buffers). What's
genuinely new: (a) **capture as a first-class patterned act** — Tidal
never records; (b) **multi-realiser lowering** onto hardware via CV,
per the Selene spec/realiser split, with capability checking (3 grain
clouds won't lower onto Lubadh; will onto Arbhar with constraints;
will onto SuperDirt freely).

## Decision 2 — the simple v1 model for the hardware instrument

Andrew's cut, which survives contact with the rig details intact —
**a universal capture bus + per-module personality cards**:

1. **Arm/cue** — claim jacks for a module card (audio-send lane +
   control lanes), start the parallel shadow-record tap (below).
   Audio-send jacks join the reservation system: a claimed ES-9
   output is now CV, gate, *or* audio-send.
2. **Record** — one tempo-aware capture verb, e.g.
   `capture morphagene (bars 2)`, lowered per module to a gate whose
   high-time is computed in seconds from the Link tempo,
   latency-compensated.
3. **Play** — patterned playback triggers (genes, grains, tape moves)
   as a **Selene assignment group owned by the card**: the card knows
   the module's CV map, ranges, and calibration tables, so "activate
   the Morphagene card" = "claim and configure its lane group" in one
   act. (Decision: cards own their assignments; free-form assignment
   with cards as decoration was considered and rejected.)
4. **Hands** — always live. On these modules CV inputs **sum with
   the physical knob** through attenuverters, so the pattern and the
   hand literally add at the panel — the workbench's collaboration
   model realised in voltage, no arbitration needed. Consequence:
   patterned CV is displayed as *offsets around the knob*, not
   authoritative values (which also matches the fact that knobs are
   unreadable).

### The shadow-record tap (the load-bearing trick)

The ES-9 has inputs. **Mult whatever goes into a module's audio-in
back into an ES-9 input and record it computer-side in parallel.**
Every capture that enters Arbhar/Lubadh/Morphagene also lands as a
real audio file in the workbench, making the system's shadow copy of
the buffer *ground truth by construction*:

- The Morphagene card renders the actual waveform with its splice
  map — which the hardware itself can't report.
- The same material can be re-realised on SuperDirt or the future
  Rust looper — the bridge between the two instruments.
- Captures are archivable artifacts (SamplesProject adjacency: `msm`
  could catalogue them).

### Shadow-state stance

Cards maintain *believed* module state (splice count, layer contents,
loop length), accurate as long as record events go through the
system. Hands-on knob twists are harmless (continuous, additive); a
hands-on record press silently desyncs the discrete index. Don't
fight it in v1: give cards a "forget/resync" affordance and label the
register honestly ("splice map: as performed through the rig"). The
shadow-record tap makes the *audio* trustworthy; the *index* stays
advisory.

### Per-module record lowering (confidence-checked altitude)

| | Record control | A capture produces | Quirk to design around |
|---|---|---|---|
| **Morphagene** | gate high = recording | a new **splice** appended to the reel | splice count grows monotonically; the shadow splice-map keeps organize CV meaningful |
| **Arbhar** | capture gate (+ onset-detect mode) | fills a layer buffer (~10s) | six layers = six addressable buffers; layer-select is part of the arm step |
| **Lubadh** | record gate per deck | sets/overwrites the tape loop | first recording defines loop length; SOS decay makes re-records additive |

### The MIDI door (Andrew, 2026-07-02)

Arbhar and Lubadh are **Raspberry Pis inside**, and **Arbhar has a
semi-public MIDI interface** that might work for us. Andrew wouldn't
be surprised to see an Instruo firmware Rev 3 with more MIDI control.
Consequences:

- Each personality card should be designed with **two possible
  control-plane realisers from the start**: CV/gate (universal,
  additive-with-knobs, costs ES-9/FH-2 lanes) and MIDI (Arbhar today,
  maybe both later — discrete addressing, higher resolution, layer
  select without a CV lane, zero jack cost).
- This is the same realiser-plurality shape as everywhere else in the
  stack; the card's spec verbs stay fixed while the lowering varies.
- Worth a small investigation task when convenient: what Arbhar's
  MIDI surface actually exposes (capture? layer select? grain
  params?), and whether it changes the arm/record/play lowerings.
- Note the trade: MIDI control is authoritative-value, not
  knob-additive — the "collaboration in voltage" property in §Hands
  is a CV-realiser property specifically.

## The transcendence demo (for later, but it names the point)

The BRIEF's juicy-UI test: manipulate the inter-relationship hardware
hides inside a patch cable. Here the relationship hardware *can't
express at all*: **one capture, four simultaneous interpretations** —
record one guitar phrase; a tape head plays it at −0.5×, a grain
cloud shimmers over its tail, a splice-jumping head cuts it under
Balistes' pattern, a clip player holds it as a bed. Hardware can
never do this (each module owns its buffer). Full version needs the
software realiser; a partial version exists as soon as one capture
fans out to two hardware modules plus the shadow copy on SuperDirt.

UI sketch: a waveform timeline with named splice regions, multiple
heads of different *kinds* sweeping it (the Odonus playhead idiom),
grain spawns as sparks. There is even a lockstep story: the control
plane (splice indices, head positions as functions of tick) is
deterministic and co-simulable reef-style, and the browser realiser
is **Web Audio** — the Triggerfish pane could make sound standalone
while the same spec lowers to Arbhar CV or SuperDirt on-rig.

## Naming

If the family stays triggerfish-Latin: **Rhinecanthus** — the
*Picasso* triggerfish, already noted in the naming memory for its
eye-bar motif — for the hardware instrument. Picasso = collage; a
splice-based musique-concrète module is the collage module. (Not
committed; the loop instrument would need its own name later.)

## Open questions

- Does es9-daemon's CoreAudio claim leave input channels free for the
  shadow tap, or does another client (Ableton) need to own the
  recording? (Lookup, not a blocker.)
- How many spare ES-9/ES-5/FH-2 lanes are actually uncommitted for a
  card's worth of CV? (Lookup.)
- Record-gate latency compensation and Morphagene organize-CV
  splice-stepping both need empirical calibration — DeepStar's
  calibration-runner future scope finally has a customer. Same
  measurement culture as ZR Phase 0 (Trig31 rig, but for CV values).
- Where does SOS live — spec (a fold over writes) or
  realiser-specific? Lean spec, lowered per target.
- Two time regimes: the spec probably needs both `Bars` and `Seconds`
  windows with explicit conversion at the clock.
- What exactly does Arbhar's semi-public MIDI interface expose, and
  does it move any lowering from CV to MIDI?

## One-line summary

Split buffers from heads; make capture a tempo-aware verb (and, where
the realiser allows, a naming act over an always-on ring); make every
navigation a Pattern; reuse the Selene spec/realiser compile split
for the audio plane — hardware CV as the humble first realiser, MIDI
as the door Instruo may open wider, the browser as the surprising
second, and mult every capture back into the ES-9 so the workbench
always holds ground truth.
