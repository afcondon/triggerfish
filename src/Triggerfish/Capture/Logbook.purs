-- | The always-on performance logbook (#28, machine-agnostic) — pure operations
-- | over `Logbook`. Lifted from `Odonus.Logbook` (which now re-exports this), so
-- | every capturing machine (Odonus now, Vetula + Balistes next) shares one engine.
-- |
-- | The machine is always capturing: every step's fresh notes append to a small
-- | LIVE chunk; when it fills (`chunkSize`), it freezes into `chunks` and a new
-- | live chunk begins. Chunking keeps the per-step append O(live) rather than
-- | O(whole session) — the real cost of a naive growing array — and makes
-- | retention a matter of dropping whole chunks. On each freeze, chunks whose
-- | span has aged past `retentionMicros` are dropped, UNLESS a mark falls inside
-- | them: "the recent past, plus anything I flagged."
-- |
-- | All arrays are newest-first. Frontend-only — nothing here rides to a rig.
module Triggerfish.Capture.Logbook
  ( emptyLog
  , logAppend
  , pushMark
  , addSnapshot
  , deleteMark
  , noteCount
  , regionBounds
  , snapMicrosToBeat
  , retentionMicros
  , chunkSize
  , materializeRegion
  , applyBounds
  ) where

import Prelude

import Data.Array (any, concatMap, deleteAt, filter, length, modifyAt, null, (:))
import Data.Foldable (sum)
import Data.Int (floor, round, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Triggerfish.Capture.Types (Chunk, Logbook, Mark, PlaySource(..), PlayState)
import Triggerfish.Clips (NoteEvent)

-- | Freeze the live chunk once it reaches this many notes (~20s at typical
-- | density). Small enough that the per-step `fresh <> live` copy stays cheap.
chunkSize :: Int
chunkSize = 400

-- | Retention window: keep the last ~90 minutes of the session. Older chunks
-- | are dropped on freeze unless a mark falls within them.
retentionMicros :: Number
retentionMicros = 90.0 * 60.0 * 1.0e6

emptyLog :: Logbook
emptyLog = { live: [], liveFrom: 0.0, chunks: [], marks: [], runs: [], cuts: [] }

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

-- | Push a fully-built mark (instant + beat + region + patch) onto the log.
pushMark :: Mark -> Logbook -> Logbook
pushMark m lb = lb { marks = m : lb.marks }

-- | Another machine's text at the mark made at `at`, replacing any it sent
-- | before.
addSnapshot :: Number -> { machine :: String, text :: String } -> Logbook -> Logbook
addSnapshot at snap lb = lb { marks = map add lb.marks }
  where
  add m
    | m.atMicros == at = m { rig = filter (\r -> r.machine /= snap.machine) m.rig <> [ snap ] }
    | otherwise = m

-- | Beats per bar the rig runs (4/4). The loop window is a whole number of these.
quantum :: Number
quantum = 4.0

-- | How many bars a default loop window spans.
loopBars :: Int
loopBars = 2

-- | The bar-aligned default loop window for a mark at `atMicros` / Link `beat`:
-- | the `loopBars` bars that end with the bar the mark falls in (the bar
-- | before it and that bar), as the rig makes it (rig_loops): a mark says
-- | "that was nice", so its window looks back. Computed in beat space and
-- | converted to recording micros via the tempo — bar alignment is what keeps
-- | the loop seam clean rather than clicking. The stored `from`/`to` are then
-- | freely draggable.
regionBounds :: Number -> Number -> Number -> { from :: Number, to :: Number }
regionBounds tempo atMicros beat =
  let beatMicros = 60.0e6 / (if tempo > 1.0 then tempo else 120.0)
      toBeat = toNumber (floor (beat / quantum)) * quantum + quantum
      fromBeat = toBeat - toNumber loopBars * quantum
  in { from: atMicros + (fromBeat - beat) * beatMicros
     , to: atMicros + (toBeat - beat) * beatMicros }

-- | Snap a recording time to the nearest beat line, using a mark as the grid
-- | anchor (its atMicros ↔ Link beat). Applied to a dragged region edge on
-- | release, so a freehand resize still lands musically.
snapMicrosToBeat :: Number -> Mark -> Number -> Number
snapMicrosToBeat tempo m t =
  let beatMicros = 60.0e6 / (if tempo > 1.0 then tempo else 120.0)
      beatAt = m.beat + (t - m.atMicros) / beatMicros
  in m.atMicros + (toNumber (round beatAt) - m.beat) * beatMicros

deleteMark :: Int -> Logbook -> Logbook
deleteMark i lb = lb { marks = fromMaybe lb.marks (deleteAt i lb.marks) }

-- | Total captured notes across the live chunk and all frozen chunks.
noteCount :: Logbook -> Int
noteCount lb = length lb.live + sum (map (length <<< _.events) lb.chunks)

-- | The notes inside a loop region `[from, to]`, rebased so the region starts at
-- | zero (`fireUnixMicros - from`) — a self-contained clip buffer ready for the
-- | shared library. Reads the FULL logbook (live + all chunks), not the decimated
-- | view.
materializeRegion :: Number -> Number -> Logbook -> Array NoteEvent
materializeRegion from to lb =
  map (\e -> e { fireUnixMicros = e.fireUnixMicros - from })
    (filter (\e -> e.fireUnixMicros >= from && e.fireUnixMicros <= to)
      (lb.live <> concatMap _.events lb.chunks))

-- | Write new bounds onto mark `i`, and if it is the one looping, onto the
-- | loop too (its notes taken again), so the change is heard next time round.
applyBounds
  :: forall r
   . Int
  -> { from :: Number, to :: Number }
  -> { logbook :: Logbook, playing :: Maybe PlayState | r }
  -> { logbook :: Logbook, playing :: Maybe PlayState | r }
applyBounds i b s =
  let
    lb = s.logbook { marks = fromMaybe s.logbook.marks (modifyAt i (\m -> m { from = b.from, to = b.to }) s.logbook.marks) }
    playing = case s.playing of
      Just p | p.source == FromRegion i ->
        Just p { fromMicros = b.from, toMicros = b.to, events = materializeRegion b.from b.to lb, lenMicros = b.to - b.from }
      other -> other
  in
    s { logbook = lb, playing = playing }
