# DESIGN — shared MIDI clip library + phrase voices (recording axis, #27)

Status: design agreed 2026-08-05. Supersedes the sketch in
`DESIGN-macro-as-text.md` §11 for the *phrase-playback* half. Builds on #26
(Odonus clip capture/name/rename/save, commits 610a6ab + 073675d).

## The idea

The rig is all-synth, no sampler. **Self-sampling the jam** — capturing markers
and loops from a machine's own output — is a first-class way to find and make
music. #26 gave Odonus persistent, named clips. #27 turns those captures into a
**shared MIDI clip library** that any machine (Odonus, Vetula, later others —
even other apps) writes to and reads from, and lets a **Vetula Perform voice**
play a captured phrase as an alternative to a harmonic progression.

Two principles carry the whole design:

- **things → chips, structure → text.** A recording is a *thing* (frozen note
  data), not *structure over time*. So a clip lives as **data** (a browsable
  chip in a library), never as eDSL text. Phrase voices are therefore excluded
  from the Lepidoptera text round-trip.
- **store everything, flatten late.** A clip keeps the full captured events —
  pitch, source channel (`headIdx`), velocity, gate, absolute onset micros —
  and never bakes in a tempo, key, or quantization. Every lossy projection
  (flatten-to-chord, drop-velocity, re-tempo) happens at *playback*, so future
  fidelity (honor velocity, per-phrase rate) needs no re-capture.

## The shared library

A new machine-agnostic store, read/written by every capturing machine.

- Module `Triggerfish.Clips` — the `MidiClip` type.
- Module `Triggerfish.Clips.Store` — localStorage at key `triggerfish.clips.v1`
  (mirrors the Odonus/Selene store FFI pattern: `_save`/`_load`/`_stringify`,
  best-effort, JSON envelope).
- Migration: on first load, if the shared store is empty, import any clips from
  the Odonus `patch.v4` envelope (#26) and map them up to `MidiClip`. Odonus
  then reads/writes the shared store; the `patch.v4` `clips` field is left in
  place but unused (harmless; a later cleanup can drop it).

### `MidiClip` schema — core + optional metadata

Naming clips is *a mug's game* (you won't remember what "clip 7" was) — so names
are allowed but the browser must work by **metadata and computed features**, not
names. All metadata is optional/defaulted so the schema is forward-compatible;
we populate the cheap fields at capture and defer computed features + the browser
UI to later without a store migration.

```
type MidiClip =
  { -- core (always present) --
    id            :: String        -- stable id (capture instant + machine); survives rename
  , events        :: Array NoteEvent  -- FULL fidelity: pitch, headIdx, vel, gateMs, onset micros
  , lenMicros     :: Number         -- loop length; onsets are rebased to [0, lenMicros)
  , heads         :: Int            -- number of distinct source channels present
  , capturedMicros :: Number        -- wall-clock capture instant (default sort key)
  , source        :: String         -- capturing machine: "odonus" | "vetula" | …

    -- human metadata (optional, default "" / [] / Nothing) --
  , name          :: String         -- may be ""; editable; never relied upon
  , tags          :: Array String   -- populated at capture (machine, key, scale); user-editable
  , notes         :: String         -- free annotation
  , bpm           :: Maybe Number    -- capture tempo (informational — playback RE-tempos)
  , key           :: Maybe String    -- capture key/scale if known (from the patch)
  , context       :: Maybe String    -- the capturing patch (Lepidoptera text), for harmonic recall
  }
```

`NoteEvent` is Odonus's existing `{ pitch, headIdx, fireUnixMicros, vel, gateMs }`
— moved to (or shared from) `Triggerfish.Clips` so both machines and the store
depend on one definition, no cycle.

Computed features for the future browser (density = notes/sec, pitch range, voice
count, rhythmic profile, …) are **derivable** from `events`, so they get a
version bump when the browser is built — not stored now.

### Capture-time population (cheap, automatic)

Odonus `SaveMarkClip` (and later Vetula) builds a `MidiClip` with:
`id`/`capturedMicros` from the wall clock, `source = "odonus"`, `heads` from the
distinct `headIdx` in the span, `bpm` from `clockTempo`, `key`/`context` from the
mark's `patch` via `harmonicSummary`, `tags = ["odonus", <root>, <scale>]`. Name
stays auto (`clip · <key>`) and rename-in-place as in #26.

## Phrase voices in Vetula

A Perform box is a `Pattern (Array Int)` source → transform stack → terminal.
The source is currently `seq :: Maybe SavedSeq` (a chord progression). We add an
**alternative source**: a captured phrase.

### On the box

An additive optional field (the surgical choice — `boxPattern`'s `base` gets one
new case; the attach handlers enforce the invariant that a box sources *either*
chords *or* a phrase, so no ambiguous both-set state). A full `BoxSource` sum
type is the eventual honest model; deferred to when #28 lands and the shape has
settled.

```
box.phrase :: Maybe PhraseAttach

type PhraseAttach =
  { clip        :: MidiClip     -- a self-contained COPY (small data; survives the
                                --   library clip being renamed/deleted). The library
                                --   is just the picker.
  , mutedHeads  :: Set Int      -- source channels silenced from the recording (perf control)
  , channelMode :: ChannelMode  -- how the multi-channel recording maps to output
  }

data ChannelMode
  = Flatten     -- collapse all sounding heads onto THIS box's channel
  | Original    -- keep each note on its source channel (headIdx → channel); no re-map
```

### Two playback projections (the toggle)

Multi-channel information is preserved and **not** thrown away by default. The
box offers a toggle (no channel *re-mapping*, just the two modes):

- **Flatten — "as a Vetula voice."** Filter events by `mutedHeads`, group
  simultaneous onsets into chords → `Pattern (Array Int)` → **the full existing
  stack applies** (transpose · voice · select · arp · slow/fast) → `scheduleBox`
  on the box's single channel. This is the headline: re-voice and arpeggiate your
  captured Odonus loop through a Vetula voice. Velocity/gate drop at the pattern
  seam (as they already do for chord voices).

- **Original — "as a MIDI clip."** Filter events by `mutedHeads`, then schedule
  each faithfully on its **source channel** at its onset, honoring the box's
  terminal and mute mask. The chord/pitch stack does **not** apply here (voicing
  an arp across four independent channels is meaningless) — in this mode the box
  presents as a faithful-playback clip, not a voice. (A global time scale / the
  future rate multiplier can still apply, since that's a per-onset scaling, not a
  chord op.)

### Re-tempo + future rate multiplier

Both modes **re-tempo to the current clock**: an event's cycle position is
`onsetMicros / lenMicros`, mapped into the bar grid at playback — so a phrase
locks to the rig's tempo, never its capture tempo. A phrase box counts as
"uses the bar grid" (like a seq box). A multi-bar phrase is `slow N` today; a
dedicated per-phrase **rate multiplier** is a scalar on that mapping — designed
in (absolute onsets are stored, quantization is at playback), UI deferred.

### Terminal gating

A phrase → **midi / rig only**. `→ odo` is hidden/disabled for a phrase: routing
to Odonus *harmonically conditions* it, which is meaningless for a frozen
foreground gesture. Testable on **→ midi with no rig** (WebMIDI → Continuo/IAC):
you hear the notes (pitch + timing faithful), just not Odonus's own timbre.

### Persistence + round-trip

The `PhraseAttach` copy rides inside the box state, so it saves with the Vetula
scene through the existing `Vetula.Store` record/JSON path — a phrase voice
survives reload. The **eDSL text** round-trip skips phrase boxes (a recording is
a thing, not structure), so the harness round-trip tests stay green; the record
form is the durable carrier. A phrase box prints in text as an opaque marker
(e.g. `-- phrase "<name>"`) for human readability, not reconstruction.

## Scope + sequencing

- **#27a — shared clip library.** `Triggerfish.Clips` (+ `NoteEvent`),
  `Clips.Store` (v1, shared), Odonus migration + write/read the shared store,
  capture-time metadata population. No Vetula yet. Low risk, foundational.
- **#27b — Vetula phrase voice.** `box.phrase` / `PhraseAttach`, the two
  playback projections in `boxPattern`/`scheduleBox`, the picker (browse the
  shared library), the mute-mask + flatten/original toggles, terminal gating.
  The by-ear test lands here (→ midi, no rig).

Deferred beyond #27: computed features + the MIDI clip **browser** UI; per-phrase
rate-multiplier UI; the `BoxSource` sum-type cleanup; Vetula's own capture (#28,
which writes into this same shared library).
