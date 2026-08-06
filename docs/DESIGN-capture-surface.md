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

## Two renderers, not one (2026-08-05)

A capture surface is really **two** views, and conflating them was a bug:

| | `Capture.River` | `Capture.View.capturePanel` |
|---|---|---|
| time scale | CONSTANT (`pxPerMs = 0.05`, ~7.6 s across) | fit-to-session (`tMin`..`tMax` squeezed into the box) |
| motion | notes flow at a fixed speed, fading over 7 s | rescales on every new note — the picture lurches |
| note mark | 9×5 rounded rect in a fixed 380×520 viewBox | 2×3 rect in a viewBox stretched to an ever-growing span → slivers |
| redraw | a 33 ms frame timer | whenever host state changes |
| right for | **LIVE** | **REPLAY** (whole take at once, to pick a phrase out of it) |

Odonus always had both — the scope (river) and the replay surface. Vetula's #28b
band was built on `capturePanel` alone and used it in both modes, which is why
AC saw it as "jerky and slow… notes very much smaller" next to Odonus's scope:
it was the wrong renderer for the live case, not a styling difference.

`Capture.River` was generalised out of `Odonus.View.Scope` (now a thin adapter
over it) with one parameter, `Flow`: `FlowLeft` emits at the right edge and ages
leftward (Odonus); `FlowRight` emits at the left edge and ages rightward (Vetula,
whose river sits right of the voices, so notes flow *out of* the voice that
played them). The background gradient brightens toward the emit edge either way.
Hosts need a frame clock (`nowMicros`) and a pruned recent-note array; Vetula's
tick is guarded to the Perform surface in LIVE so a 30 fps redraw never runs
while you're on the tonnetz.

## Orientation

The only axis-aware code is the coordinate mapping and the region bands. Factor
two projections behind `Orientation`:

> **SUPERSEDED for Vetula, 2026-08-05.** After playing the vertical tracker in
> anger AC concluded it "compromises Vetula too much" — the vertical axis reads
> well on its own but eats the width the voices need. Vetula now uses a THIRD
> orientation, `HorizontalOutward`: time on X but REVERSED, newest note at the
> left edge, ageing rightward, on a surface that sits to the RIGHT of the voices
> — so notes appear to flow *out of* the voice that played them. Differentiation
> from Odonus is now POSITION (side strip vs full-width) and time DIRECTION, not
> axis. The `Vertical` projection stays in `Capture.Types` unused, for the
> true-tracker refinement parked below. The table below documents the original
> two-way split.

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
- **#28-lib — promote the library modal to shell-level** — **DONE 2026-08-05.**
  `Triggerfish.Clips.View.libraryPanel` is one action-polymorphic renderer (the
  `CaptureWiring` idiom) with a `LibraryWiring` whose `attach` field selects the
  mode: `Nothing` = browse/manage, `Just` = the ＋ attach column. Two hosts —
  the shell's **⌥6 modal** (`MClips`, browse/manage, opens with a fresh
  `loadClips`) and Vetula's phrase picker (attach). The shell requests its OWN
  Web MIDI out at Init so ▶ audition works from anywhere; a clip plays on the
  channels it was CAPTURED on, deliberately not through a machine's audition
  dest. Rename/delete touch the library only — an attached voice keeps its
  snapshot copy. **Still open:** the in-surface "library" button on a REPLAY
  surface. `OpenClipLibrary` exists on the shell and does the right thing, but
  nothing raises it yet — it needs a `CaptureWiring.openLibrary` field plus an
  Output message per machine. ⌥6 works from every machine meanwhile.
- **#28b — Vetula capture.** (Landed vertical; re-cut to `HorizontalOutward` +
  a LIVE/REPLAY switch on 2026-08-05 — see the superseded note above. LIVE puts
  the roll in a third-width strip right of the voices; REPLAY gives it the whole
  surface. Region PREVIEW landed the
  same day: `driveCaptureReplay` is Odonus's windowed `driveReplay` ported onto
  Vetula's new frame clock — click a gold band and it loops, on the channels it
  was captured on, through `st.midiOut` so ⌥1 AuditionOff silences it. Stop /
  leave REPLAY / clear / lift all `hushCapture`, which sends all-notes-off on
  just the region's own channels rather than all 16, so a preview can't cut
  voices that are still performing. ~~Known wrinkle: switching Vetula to a Browse
  surface while a region loops leaves it looping.~~ **FIXED 2026-08-06** by the
  Stage collapse — `SetStage` absorbed `SetCaptureView`, so leaving REVIEW by any
  route hushes the preview. See docs/DESIGN-stages.md.)  Original scope: Add `capture :: CaptureState`, the `PerfTick`
  note-harvest tap, a REPLAY surface wired `Vertical`, mark/region/lift. By-ear +
  by-eye: capture a Vetula phrase, see it as a vertical tracker, lift → library →
  attach it back into a box (closes the loop through #27).
- **#28c — Balistes.** Third consumer, live-jam capture. Mostly wiring once #28a/b
  prove the seam.
- **refinement — true tracker rendering** for the vertical orientation (per-channel
  columns + note-name rows) once the axis-swap MVP is proven.

## Open questions (decide by build/ear, not up front)

- ~~Vertical time direction: newest at **top** or **bottom**?~~ — **MOOT**: Vetula
  left the vertical projection entirely (see the superseded note). The question
  returns only if the true-tracker refinement is ever built.
- ~~Whether Vetula capture is a full tab or a modal~~ — **DECIDED (AC): full
  surface.** A tab, not a modal over the Perform surface. Since 2026-08-06 it is
  a first-class STAGE on both machines — `Review`, peer to `Perform` rather than
  a flag inside it (docs/DESIGN-stages.md), and the LIVE/REPLAY switch it used to
  hang off is gone in favour of the stage tabs. Reinforces the always-on-harvest framing above: it's a
  first-class review surface, not a transient popover.
- Does `Mark.patch` (the machine's Lepidoptera at capture) generalise cleanly to
  Vetula/Balistes state text? It should (all three serialise to eDSL), but confirm
  the `harmonicSummary`-style context read has a Vetula/Balistes analogue or
  degrades gracefully to "no context".
