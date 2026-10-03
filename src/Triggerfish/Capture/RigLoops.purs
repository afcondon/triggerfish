-- | **The record buffer and its loops, kept on the rig** (docs/kb/plans/the-deck.md,
-- | step 3b; words as in docs/kb/reference/atlantis-vocabulary.md).
-- |
-- | When the rig is there it keeps each machine's record buffer, numbers the
-- | marks (from 1, in the order made, so `loop 2` always means one mark), and
-- | plays the loops on Link time; the Review surface draws what it is told
-- | (`loops {"machine", "marks": [...]}`) and sends what the hand does, as
-- | the lines Limulus would send (`odonus $ loop 2`). It holds no loop of its
-- | own. Without a rig, the page keeps marks and plays loops itself, as before.
-- |
-- | The rig counts in Link beats; a surface draws in its own microseconds
-- | (Odonus's are Unix, Vetula's performance time), so each host hands over a
-- | `Clock`: the same instant in both.
module Triggerfish.Capture.RigLoops
  ( RigMark
  , Clock
  , microsOf
  , beatOf
  , readLoops
  , reconcile
  , insertMark
  , nextId
  , renumber
  , readClear
  , looping
  , focus
  , playheadFrac
  , syncLine
  , runLine
  , notesLine
  , readNotes
  , seedNotes
  , cueLine
  , windowLine
  , deleteLine
  , recordLine
  ) where

import Prelude

import Data.Array (filter, find, foldl, insertBy, length, mapMaybe, mapWithIndex, null, reverse, (!!))
import Data.Either (hush)
import Data.Foldable (maximum)
import Data.Int (floor, round)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (Pattern(..), stripPrefix)
import Data.Nullable (Nullable, toMaybe)
import Simple.JSON (readJSON, writeJSON)
import Triggerfish.Capture.Runs (Run)
import Triggerfish.Capture.Types (Logbook, Mark)
import Triggerfish.Clips (NoteEvent)

-- | A mark as the rig keeps it: its number (position) and id, when it was made (Link beat and
-- | Unix µs), its window in beats, and whether a loop plays it (from `start`,
-- | the downbeat it began on).
type RigMark =
  { n :: Int, id :: Int, beat :: Number, us :: Number, from :: Number, to :: Number
  , originFrom :: Number, originTo :: Number, playing :: Boolean, start :: Maybe Number }

-- | One instant in the surface's microseconds and in Link beats, and the tempo.
type Clock = { micros :: Number, beat :: Number, tempo :: Number }

beatMicros :: Clock -> Number
beatMicros c = 60.0e6 / (if c.tempo > 1.0 then c.tempo else 120.0)

microsOf :: Clock -> Number -> Number
microsOf c b = c.micros + (b - c.beat) * beatMicros c

beatOf :: Clock -> Number -> Number
beatOf c t = c.beat + (t - c.micros) / beatMicros c

-- | The marks of `machine`, newest first, from a `loops` frame.
readLoops :: String -> String -> Maybe (Array RigMark)
readLoops machine msg = do
  json <- stripPrefix (Pattern "loops ") msg
  r :: { machine :: String, marks :: Array RigMark } <- hush (readJSON json)
  if r.machine == machine then Just r.marks else Nothing

-- | The surface's marks brought to the rig's: windows and loops as the rig
-- | has them, marks it no longer has dropped. The rig's marks this surface has
-- | not met yet (one made from Limulus, or by this page a moment ago) come
-- | back as `fresh`, for the host to fill with what it knows at the mark.
reconcile :: Clock -> Array RigMark -> Array Mark -> { marks :: Array Mark, fresh :: Array RigMark }
reconcile c rig marks =
  { marks: mapMaybe update marks
  , fresh: filter (\r -> not (any' (\m -> m.id == r.id) marks)) rig
  }
  where
  update m = find (\r -> r.id == m.id) rig <#> \r ->
    m { n = r.n, from = microsOf c r.from, to = microsOf c r.to
      , loop = if r.playing then r.start else Nothing }
  any' p xs = isJust (find p xs)

-- | A mark into the surface's list, which runs newest first.
insertMark :: Mark -> Array Mark -> Array Mark
insertMark m = insertBy (\a b -> compare b.id a.id) m <<< filter (\o -> o.id /= m.id)

-- | The id a mark made here, with no rig, takes.
nextId :: Array Mark -> Int
nextId marks = 1 + fromMaybe 0 (maximum (map _.id marks))

-- | Number marks by position, oldest first, as the rig does: for a page with
-- | no rig, after a mark is made or deleted. The list runs newest first.
renumber :: Array Mark -> Array Mark
renumber marks = mapWithIndex (\i m -> m { n = length marks - i }) marks

-- | Whether a frame says the rig cleared `machine` (`loops-clear`).
readClear :: String -> String -> Boolean
readClear machine msg = case stripPrefix (Pattern "loops-clear ") msg >>= (hush <<< readJSON) of
  Just (r :: { machine :: String }) -> r.machine == machine
  Nothing -> false

-- | Whether the rig is playing mark `m` as a loop.
looping :: Mark -> Boolean
looping m = isJust m.loop

-- | The loop the surface's card is about: the one started last.
focus :: Array Mark -> Maybe Mark
focus marks = foldl later Nothing (filter looping marks)
  where
  later acc m = case acc of
    Just a | a.loop >= m.loop -> Just a
    _ -> Just m

-- | Where in its window a rig loop is, 0..1, at `beat`.
playheadFrac :: Clock -> Mark -> Maybe Number
playheadFrac c m = m.loop <#> \start ->
  let
    len = max 1.0 (beatOf c m.to - beatOf c m.from)
    into = c.beat - start
    k = Int.toNumber (floor (into / len))
  in if into < 0.0 then 0.0 else (into - k * len) / len

-- | Ask the rig to tell every page its marks and loops.
syncLine :: String
syncLine = "loops-sync"

-- | Ask the rig for a machine's whole record buffer (to this page alone).
notesLine :: String -> String
notesLine machine = "loops-notes " <> machine

-- | The machine's transport started or stopped, for the rig's runs.
runLine :: String -> Boolean -> String
runLine machine playing = "loops-run " <> writeJSON { machine, playing }

-- | The record buffer from a `loops-notes` frame, as the surface's notes and
-- | runs, newest first, timed by `Clock`.
readNotes :: String -> Clock -> String -> Maybe { notes :: Array NoteEvent, runs :: Array Run }
readNotes machine c msg = do
  json <- stripPrefix (Pattern "loops-notes ") msg
  r :: { machine :: String, notes :: Array (Array Number), runs :: Maybe (Array (Array (Nullable Number))) } <- hush (readJSON json)
  if r.machine /= machine then Nothing
  else Just { notes: reverse (mapMaybe note r.notes), runs: reverse (mapMaybe run (fromMaybe [] r.runs)) }
  where
  run a = do
    from <- a !! 0 >>= toMaybe
    pure { from: microsOf c from, to: microsOf c <$> (a !! 1 >>= toMaybe) }
  note a = do
    beat <- a !! 0
    pitch <- a !! 1
    vel <- a !! 2
    dur <- a !! 3
    voice <- a !! 4
    pure { pitch: round pitch, headIdx: round voice, fireUnixMicros: microsOf c beat
         , vel: round vel, gateMs: dur * beatMicros c / 1000.0 }

-- | A page that opens after the notes were played takes the rig's record
-- | buffer, and its runs, as its own; one that has notes keeps them.
seedNotes :: { notes :: Array NoteEvent, runs :: Array Run } -> Logbook -> Logbook
seedNotes r lb
  | null lb.live && null lb.chunks && not (null r.notes) =
      lb { live = r.notes, liveFrom = fromMaybe 0.0 (map _.fireUnixMicros (r.notes !! 0))
         , runs = if null r.runs then lb.runs else r.runs }
  | otherwise = lb

-- | A cue, as Limulus would send it: `tidal odonus $ loop 2`.
cueLine :: String -> String -> String
cueLine machine cue = "tidal " <> machine <> " $ " <> cue

-- | Mark `n`'s window as drawn, in the surface's µs, to the rig in beats
-- | (whole ones: the drag snaps to beats already).
windowLine :: String -> Clock -> Mark -> String
windowLine machine c m =
  "loops-window " <> writeJSON
    { machine, n: m.n
    , from: Int.toNumber (round (beatOf c m.from)), to: Int.toNumber (round (beatOf c m.to)) }

deleteLine :: String -> Int -> String
deleteLine machine n = "loops-delete " <> writeJSON { machine, n }

-- | Notes this page played itself (Solo), for the rig's record buffer.
-- | `toUnix` turns the surface's µs into Unix µs; a note's voice is its
-- | `headIdx` (Odonus's head, Vetula's channel).
recordLine :: String -> (Number -> Number) -> Array NoteEvent -> Maybe String
recordLine machine toUnix notes
  | null notes = Nothing
  | otherwise = Just $ "loops-record " <> writeJSON
      { machine
      , notes: map (\e -> { us: toUnix e.fireUnixMicros, pitch: e.pitch, vel: e.vel, gateMs: e.gateMs, voice: e.headIdx }) notes }
