-- | `Triggerfish.Capture.Types` — the machine-agnostic value types of the
-- | self-sampling capture/replay surface (#28, docs/DESIGN-capture-surface.md).
-- | Every capturing machine (Odonus now, Vetula + Balistes next) shares ONE
-- | definition of the always-on logbook, its marks, loop regions, and the replay
-- | play-state. A clean leaf (Prelude + `Data.Maybe` + the shared `NoteEvent`), so
-- | any machine can depend on it without a cycle.
-- |
-- | Lifted verbatim from `Odonus.Grid.Types` (which now re-exports these), so the
-- | working Odonus surface is unchanged. `NoteEvent` itself lives one level lower,
-- | in `Triggerfish.Clips`.
module Triggerfish.Capture.Types
  ( Chunk
  , Mark
  , RegionEdge(..)
  , RegionDrag
  , PlaySource(..)
  , PlayState
  , Logbook
  , Orientation(..)
  , Zoom(..)
  ) where

import Prelude

import Triggerfish.Clips (NoteEvent)

-- | One frozen span of the always-on logbook: a chunk of captured notes with its
-- | time bounds. Chunking keeps the live append O(current chunk) instead of
-- | O(whole session), and makes retention a matter of dropping whole chunks.
type Chunk = { fromMicros :: Number, toMicros :: Number, events :: Array NoteEvent }

-- | A flagged good bit: WHEN it happened (wall clock + the absolute Link `beat`),
-- | the loop window `from`/`to` (recording micros — bar-aligned at capture, then
-- | freely draggable/resizable), and the capturing machine's state as text
-- | (`patch` = its Lepidoptera at that instant). So a mark carries the notes that
-- | came out (via its span in the note stream), an editable loop region, and the
-- | machine state that made it.
type Mark = { atMicros :: Number, beat :: Number, from :: Number, to :: Number, patch :: String }

-- | Which part of a loop region a drag grabbed: its start edge, end edge, or body
-- | (slide the whole window). Edge naming is time-relative, not screen-relative, so
-- | it reads the same in either `Orientation`.
data RegionEdge = EdgeFrom | EdgeTo | EdgeBody

derive instance eqRegionEdge :: Eq RegionEdge

-- | A region drag in progress. `grabMicros` is the pointer position (in recording
-- | micros) where the grab began; `moved` distinguishes a resize/slide from a bare
-- | click (a click on the body starts playback instead).
type RegionDrag =
  { markIdx :: Int, edge :: RegionEdge, grabMicros :: Number
  , startFrom :: Number, startTo :: Number, moved :: Boolean
  }

-- | What a replay loop is currently playing. On the generalised surface this is
-- | always a region on the timeline; saved clips play from the library modal, not
-- | here (#28 dropped the on-surface clip player), so `FromClip` is legacy — kept
-- | until the Odonus surface is rewired off it.
data PlaySource = FromRegion Int | FromClip Int

derive instance eqPlaySource :: Eq PlaySource

-- | A replay loop in flight: the region bounds in recording time, which mark it
-- | came from, the perf-clock instant the next loop iteration aligns to, and the
-- | 0..1 playhead position for the view. The scheduler is source-agnostic (it reads
-- | the rebased `events`); the source only decides what the view highlights.
type PlayState =
  { source :: PlaySource
  , events :: Array NoteEvent  -- the loop's notes, rebased to [0, lenMicros)
  , lenMicros :: Number        -- loop length; the notes repeat every lenMicros
  , fromMicros :: Number       -- region bounds on the timeline (FromRegion playhead only)
  , toMicros :: Number
  , loopStartMs :: Number      -- perf-now ms that the loop's phase-0 aligns to
  , scheduledUntilMs :: Number  -- watermark: notes are queued up to this perf-now ms
  , playheadFrac :: Number
  }

-- | The always-on performance logbook: the note stream WITHOUT the scope's short
-- | prune, so what actually happened survives to be harvested later. The machine is
-- | always capturing — no arm. `live` is the growing current chunk (newest-first);
-- | once it fills it freezes into `chunks` (newest-first) and a new live chunk
-- | starts. `marks` are instants the performer tapped to flag a good bit — the seam
-- | to lift a span into a clip. Retention: on each freeze, chunks older than the
-- | window are dropped UNLESS a mark falls within them. Frontend-only.
type Logbook =
  { live :: Array NoteEvent    -- current growing chunk, newest-first
  , liveFrom :: Number         -- wall-clock start of the live chunk
  , chunks :: Array Chunk      -- frozen chunks, newest-first
  , marks :: Array Mark        -- flagged good bits, newest-first
  }

-- | How a capture surface lays time out. The one axis-aware parameter of the shared
-- | view — same data, same gestures, different projection.
-- |
-- |   * `Horizontal` — time → X, OLDEST at left (Odonus, as it always has).
-- |   * `HorizontalOutward` — time → X REVERSED: the newest note enters at the LEFT
-- |     edge and ages rightward. Vetula's surface sits to the RIGHT of its voices,
-- |     so notes appear to flow OUT of the voice that played them (AC, 2026-08-05).
-- |   * `Vertical` — time → Y, newest at top. Built for the Vetula tracker; unused
-- |     since Vetula went horizontal, kept for the true-tracker refinement the
-- |     design note still parks (docs/DESIGN-capture-surface.md).
-- |
-- | The two horizontal variants share ALL their geometry (strip shape, resize
-- | cursors, card anchoring) — they differ only in `timeCoord`/`axisPos`, which is
-- | why the view matches `Vertical` explicitly and lets `_` carry the rest.
data Orientation = Horizontal | HorizontalOutward | Vertical

derive instance eqOrientation :: Eq Orientation

-- | How much of the take the surface shows. `Whole` fits the session, its span
-- | growing in steps rather than with every note; `Last d` follows the newest
-- | `d` microseconds; `Window` holds a fixed stretch (a crop to a mark).
data Zoom = Whole | Last Number | Window { from :: Number, to :: Number }

derive instance eqZoom :: Eq Zoom
