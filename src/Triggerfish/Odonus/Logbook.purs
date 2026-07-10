-- | The always-on performance logbook (#151) — pure operations over `Logbook`.
-- |
-- | The rig is always capturing: every step's fresh notes append to a small
-- | LIVE chunk; when it fills (`chunkSize`), it freezes into `chunks` and a new
-- | live chunk begins. Chunking keeps the per-step append O(live) rather than
-- | O(whole session) — the real cost of a naive growing array — and makes
-- | retention a matter of dropping whole chunks. On each freeze, chunks whose
-- | span has aged past `retentionMicros` are dropped, UNLESS a mark falls inside
-- | them: "the recent past, plus anything I flagged."
-- |
-- | All arrays are newest-first, matching the scope's `notes` river. Frontend-
-- | only — nothing here rides to the rig or touches a reef golden.
module Triggerfish.Odonus.Logbook
  ( emptyLog
  , logAppend
  , mark
  , deleteMark
  , noteCount
  , retentionMicros
  , chunkSize
  ) where

import Prelude

import Data.Array (any, deleteAt, filter, length, null, (:))
import Data.Foldable (sum)
import Data.Maybe (fromMaybe)
import Triggerfish.Odonus.Grid.Types (Chunk, Logbook, NoteEvent)

-- | Freeze the live chunk once it reaches this many notes (~20s at typical
-- | density). Small enough that the per-step `fresh <> live` copy stays cheap.
chunkSize :: Int
chunkSize = 400

-- | Retention window: keep the last ~90 minutes of the session. Older chunks
-- | are dropped on freeze unless a mark falls within them.
retentionMicros :: Number
retentionMicros = 90.0 * 60.0 * 1.0e6

emptyLog :: Logbook
emptyLog = { live: [], liveFrom: 0.0, chunks: [], marks: [] }

-- | Append this step's fresh notes (newest-first) at wall-clock `now`. Seeds the
-- | live chunk's start when it was empty; freezes + purges when it fills.
logAppend :: Number -> Array NoteEvent -> Logbook -> Logbook
logAppend now fresh lb =
  let
    from = if null lb.live then now else lb.liveFrom
    live' = fresh <> lb.live
    lb' = lb { live = live', liveFrom = from }
  in
    if length live' >= chunkSize then freeze now lb' else lb'

-- | Freeze the live chunk into `chunks`, purge aged chunks, and start fresh.
freeze :: Number -> Logbook -> Logbook
freeze now lb =
  let
    frozen :: Chunk
    frozen = { fromMicros: lb.liveFrom, toMicros: now, events: lb.live }
    cutoff = now - retentionMicros
    keep c = c.toMicros >= cutoff || any (\m -> m.atMicros >= c.fromMicros && m.atMicros <= c.toMicros) lb.marks
    chunks' = filter keep (frozen : lb.chunks)
  in
    lb { live = [], liveFrom = now, chunks = chunks' }

-- | Flag a good bit at instant `now`, snapshotting the live patch alongside it.
mark :: Number -> String -> Logbook -> Logbook
mark now patch lb = lb { marks = { atMicros: now, patch } : lb.marks }

deleteMark :: Int -> Logbook -> Logbook
deleteMark i lb = lb { marks = fromMaybe lb.marks (deleteAt i lb.marks) }

-- | Total captured notes across the live chunk and all frozen chunks.
noteCount :: Logbook -> Int
noteCount lb = length lb.live + sum (map (length <<< _.events) lb.chunks)
