# DESIGN — reusable machine-agnostic capture/replay surface (#28)

Status: design. Companion to `DESIGN-midi-clip-library.md` (#27) and the shared
`Triggerfish.Clips` / `Triggerfish.Clips.Store` (#33, the library modal).

## What this is

The Odonus REPLAY tab (#151) is a self-sampler: an always-on logbook of every
emitted note, performer-tapped **marks** on the good bits, draggable bar-aligned
**loop regions**, and **⧉ clip** to lift a region into the shared library. It
works perfectly and AC has no notes on the Odonus *behaviour*.

#28 does two things:

1. **Generalise** that surface into a machine-agnostic component so Vetula and
   Balistes get the same self-sampling, with **orientation as a parameter** —
   Odonus horizontal (time → X, as now), Vetula vertical (time → Y, tracker /
   piano-roll down the screen). Same data, same gestures, different axis. It's
   N-machine from the start: Balistes live-jam capture is the third consumer.

   **Why Vetula especially wants this** (AC, 2026-08-05): playing across several
   Vetula voices throws up odd little moments you could never quite trigger
   again — the point of always-on capture is to *harvest* those after the fact,
   not to reproduce them. That sets the bar: capture must be genuinely always-on
   and lossless enough that the un-repeatable accident survives in the logbook,
   ready to mark and lift once you realise it happened.
2. **Simplify the flow** (AC's one note, 2026-08-05): now that the clip-library
   modal (#33) exists, **capture saves straight to the library** — drop the
   on-surface CLIPS strip and its per-clip players. The capture surface's job
   ends at "lift this region → library"; finding, auditioning, renaming and
   deleting clips all happen in the library modal.

## Principle

The capture surface is **pure over a `CaptureState` sub-record and polymorphic in
the host's Action type**. The shared view never imports a machine's `Action`
(that would be a cycle and machine-specific); instead each machine passes the
constructors it wants the view to emit. Halogen HTML is already
`ComponentHTML action slots m` — polymorphic in `action` — so "pass the actions
in" is the idiomatic, zero-magic seam. State transitions live in a pure engine
each machine's handler delegates to.

## Module plan — `Triggerfish.Capture.*`

`Odonus/Logbook.purs` is already machine-agnostic (append / freeze / retention /
`regionBounds` / `snapMicrosToBeat` / `noteCount`) and `NoteEvent` already lives
in the shared `Triggerfish.Clips`. So most of this is a **move + re-export**, not
a rewrite — Odonus keeps compiling by re-exporting from the new home.

- **`Triggerfish.Capture.Types`** — `Chunk`, `Mark`, `Logbook`, `PlayState`,
  `RegionEdge`, `RegionDrag`, and new `Orientation`. Lifted verbatim from
  `Odonus/Grid/Types`. `Mark.patch :: String` stays — it's already generic ("the
  capturing machine's state as text", i.e. its Lepidoptera). `PlaySource`
  collapses to just region playback on the surface (see "Clips off the surface").
- **`Triggerfish.Capture.Logbook`** — the current `Odonus/Logbook.purs` verbatim.
- **`Triggerfish.Capture.Engine`** — the pure region-drag math + `materializeRegion`
  + the clip-lift builder, extracted from `Odonus/Grid.purs` (currently inline in
  the `RegionUp` / `SaveMarkClip` handlers). One `liftRegionToClip` that takes a
  mark, the logbook, and a `{ source, bpm, key, tags, context }` metadata bundle
  and returns a `MidiClip` — so every machine builds a library clip identically.
- **`Triggerfish.Capture.View`** — the timeline/tracker renderer, orientation-
  parameterised, polymorphic in `action`. Replaces `Odonus/View/Replay.purs`'s
  `replayPanel` (minus the clip strip).

```purescript
type CaptureState =
  { logbook    :: Logbook
  , playing    :: Maybe PlayState   -- a region loop in flight (Nothing = idle)
  , regionDrag :: Maybe RegionDrag
  , contextOpen :: Boolean
  }

data Orientation = Horizontal | Vertical

-- What the view needs the host to give it: the state to render, the orientation,
-- the live tempo (for the bar grid), and the constructors for every gesture.
type CaptureWiring action =
  { regionDown :: Int -> RegionEdge -> Int -> Int -> action
  , regionMove :: Int -> Int -> action
  , regionUp   :: action
  , playRegion :: Int -> action
  , stopPlay   :: action
  , saveClip   :: Int -> action      -- lift region i → shared library
  , saveScene  :: Int -> action      -- optional; Nothing-guarded per machine
  , toggleContext :: action
  , timelineId :: String             -- machine-unique DOM id for pointer math
  , orientation :: Orientation
  }

capturePanel
  :: forall action slots m
   . CaptureWiring action -> CaptureState -> Number
  -> H.ComponentHTML action slots m
```

Odonus's existing `State` keeps its flat fields; a thin adapter builds
`CaptureState` + `CaptureWiring` from Odonus's `State`/`Action` at the one call
site. (No need to nest Odonus's fields — the adapter is five lines and keeps the
working component untouched.) Vetula/Balistes add a `capture :: CaptureState`
sub-record and their own wiring.

## Orientation

The only axis-aware code is the coordinate mapping and the region bands. Factor
two projections behind `Orientation`:

| | Horizontal (Odonus, as now) | Vertical (Vetula tracker) |
|---|---|---|
| time axis | X (left→right, old→new) | Y (top→bottom, old→new) |
| pitch axis | Y (high = top) | X (low→high, left→right) |
| region band | full-height vertical strip `[from,to]` on X | full-width horizontal strip `[from,to]` on Y |
| edge handles | `ew-resize` at strip L/R | `ns-resize` at strip T/B |
| playhead | vertical line moving in X | horizontal line moving in Y |

MVP for vertical = the **same decimated note-dot scatter with axes swapped** —
cheapest path to "it works, both ways". The richer *true tracker* rendering
(fixed per-channel columns, note-name text per row, headIdx → column) is a
follow-up refinement layered on the same `CaptureState`; it doesn't change the
data or the gestures, only the vertical draw. Note this explicitly so the MVP
isn't mistaken for the finished tracker.

## Clips off the surface (AC's note)

With the library modal (#33) as the home for clips:

- **Remove** `clipStrip` / `clipChip` from the capture view, and with them the
  on-surface clip player. `PlaySource` loses `FromClip`; the surface only ever
  loops a **region** (`FromRegion` — auditioning the *live timeline*, which stays,
  it's not a saved clip).
- **`saveClip`** (was `SaveMarkClip`) builds the `MidiClip` via
  `Capture.Engine.liftRegionToClip` and **appends straight to the shared store**
  (`ClipStore.loadClips` → cons → `ClipStore.saveClips`), with no machine-local
  `clips` array for display. Odonus's `State.clips`, `PlayClip`, `RenameClip`,
  `DeleteClip`, `startClip`, and the `FromClip` branch of `hushReplayVoices` all
  **retire from the surface** (clip audition/rename/delete now live in the modal:
  `ClipAudition`/`ClipRename`/`ClipDelete`, already built in #33).
- **Consequence — the library modal must be reachable from any machine.** Today
  it's Vetula-local and opened per-box for *attach*. Promote it to a **shell-level
  clip-library modal** with two entry modes: **browse/manage** (audition + rename
  + delete, no attach column) reachable from each machine's REPLAY surface / nav,
  and **attach-to-box i** (the current mode, adds the ＋ column) reached from a
  Vetula box. This promotion is the one genuinely new surface #28 needs beyond the
  capture component; scope it as a companion step (#28-lib) so the capture
  generalisation and the modal move can land independently.

## The tap point — feeding the logbook

Each machine appends the notes it just emitted to its own logbook every tick via
`Capture.Logbook.logAppend now fresh`.

- **Odonus** already does this in `Step`.
- **Vetula**: the tap is `PerfTick` (App.purs ~2598). Today `scheduleBox` emits
  to MIDI as a side effect and returns `Unit` — it doesn't report *what* it
  played. The new wiring: `scheduleBox` (and `schedulePat`/`stepVoice`) **return
  the `Array NoteEvent` they scheduled** (pitch, `headIdx` = box channel or voice
  index, `fireUnixMicros` = tick wall-clock + per-note offset, `vel`, `gateMs`);
  `PerfTick` concats across boxes/voices and `logAppend`s once. This is the main
  new *plumbing* in Vetula — the surface itself is the shared component. Guard it
  so it costs nothing when the capture tab is closed (still append — capture is
  always-on by design — but skip the redraw).
- **Balistes**: same shape, `source: "balistes"`, its own tick.

Vetula clips get `source: "vetula"`, Balistes `source: "balistes"` — the modal's
source badge (#33) already colours all three.

## Phasing

- **#28a — extract + generalise Odonus, no behaviour change.** Move Logbook/Types
  to `Capture.*`, extract `Capture.Engine`, build `Capture.View` with `Orientation`
  (Odonus wires `Horizontal`). **Drop the clip strip**; `saveClip` writes straight
  to the store; retire the on-surface clip players. Verify Odonus REPLAY is
  visually + behaviourally identical **except** the strip is gone and clips appear
  in the library modal instead. This is the load-bearing refactor; the working
  Odonus is the oracle.
- **#28-lib — promote the library modal to shell-level**, browse/manage vs attach
  modes; add a "library" entry point to Odonus REPLAY (so a just-captured clip is
  reachable). Can land in parallel with #28a.
- **#28b — Vetula vertical capture.** Add `capture :: CaptureState`, the `PerfTick`
  note-harvest tap, a REPLAY surface wired `Vertical`, mark/region/lift. By-ear +
  by-eye: capture a Vetula phrase, see it as a vertical tracker, lift → library →
  attach it back into a box (closes the loop through #27).
- **#28c — Balistes.** Third consumer, live-jam capture. Mostly wiring once #28a/b
  prove the seam.
- **refinement — true tracker rendering** for the vertical orientation (per-channel
  columns + note-name rows) once the axis-swap MVP is proven.

## Open questions (decide by build/ear, not up front)

- Vertical time direction: newest at **top** (matches Odonus newest-on-right) or
  **bottom** (tracker convention scrolls downward)? Try both in #28b.
- ~~Whether Vetula capture is a full tab or a modal~~ — **DECIDED (AC): full
  surface, exactly like Odonus's LIVE/REPLAY switch.** A tab, not a modal over the
  Perform surface. Reinforces the always-on-harvest framing above: it's a
  first-class review surface, not a transient popover.
- Does `Mark.patch` (the machine's Lepidoptera at capture) generalise cleanly to
  Vetula/Balistes state text? It should (all three serialise to eDSL), but confirm
  the `harmonicSummary`-style context read has a Vetula/Balistes analogue or
  degrades gracefully to "no context".
