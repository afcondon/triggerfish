-- | Triggerfish's Odonus model. A 4×4 grid of 16 cells (note + skip/gate/glide);
-- | each of four playheads walks the grid along its OWN access **pattern** (a
-- | René-style ordering of the 16 cells), at its own speed/direction, with a
-- | per-head transposition. Per-head pattern × speed × direction × interval is a
-- | richer Fugue Machine than either René (global pattern) or Fugue Machine
-- | (linear only). Patterns are drawn as small-multiple thumbnails by the grid.
module Triggerfish.Odonus.Model
  ( Cell
  , Head
  , Odonus
  , Pattern
  , patternLibrary
  , orderOf
  , defaultOdonus
  , replicate16
  , step
  , Fired
  , stepEmit
  , cursorsOf
  , toggleSkip
  , toggleGate
  , toggleGlide
  , setNote
  , speedTable
  , speedOf
  , setHeadSpeedIx
  , setHeadDir
  , setHeadTransp
  , toggleHeadMute
  , cyclePattern
  ) where

import Prelude

import Data.Array (catMaybes, mapWithIndex, replicate, modifyAt, length, (!!))
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)

type Cell =
  { note :: Int
  , skip :: Boolean
  , gate :: Boolean
  , glide :: Boolean
  }

-- | A René-style access pattern: a name + an ordering (permutation of 0..15,
-- | row-major y*4+x) giving the sequence in which cells are visited.
type Pattern = { name :: String, order :: Array Int }

patternLibrary :: Array Pattern
patternLibrary =
  [ { name: "Rows",       order: [ 0,1,2,3, 4,5,6,7, 8,9,10,11, 12,13,14,15 ] }
  , { name: "Serpentine", order: [ 0,1,2,3, 7,6,5,4, 8,9,10,11, 15,14,13,12 ] }
  , { name: "Columns",    order: [ 0,4,8,12, 1,5,9,13, 2,6,10,14, 3,7,11,15 ] }
  , { name: "Spiral",     order: [ 0,1,2,3, 7,11,15,14, 13,12,8,4, 5,6,10,9 ] }
  , { name: "Diagonal",   order: [ 0,1,4, 2,5,8, 3,6,9,12, 7,10,13, 11,14, 15 ] }
  ]

defaultOrder :: Array Int
defaultOrder = [ 0,1,2,3, 4,5,6,7, 8,9,10,11, 12,13,14,15 ]

orderOf :: Int -> Array Int
orderOf ix = maybe defaultOrder _.order (patternLibrary !! ix)

-- | A playhead. `seqPos` is the index into its pattern's order; `cursor` is the
-- | derived grid cell (order !! seqPos). `direction` 0=fwd 1=back 2=pendulum.
type Head =
  { cursor :: Int
  , seqPos :: Int
  , accumulator :: Number
  , pendStep :: Int
  , speedIx :: Int
  , direction :: Int
  , transp :: Int
  , mute :: Boolean
  , patternIx :: Int
  }

type Odonus =
  { cells :: Array Cell   -- length 16
  , heads :: Array Head
  }

speedTable :: Array Number
speedTable = [ 0.25, 0.5, 0.75, 1.0, 1.5, 2.0, 3.0, 4.0 ]

speedOf :: Head -> Number
speedOf h = fromMaybe 1.0 (speedTable !! h.speedIx)

replicate16 :: forall a. a -> Array a
replicate16 = replicate 16

mkHead :: Int -> Int -> Int -> Boolean -> Int -> Head
mkHead speedIx direction transp mute patternIx =
  { cursor: 0, seqPos: 0, accumulator: 0.0, pendStep: 1
  , speedIx, direction, transp, mute, patternIx }

-- | Head I runs (Rows, 1.0×); II–IV start muted with distinct patterns + fugue
-- | offsets — unmute to build the canon.
defaultHeads :: Array Head
defaultHeads =
  [ mkHead 3 0 0 false 0       -- I:   Rows, 1.0× fwd
  , mkHead 1 0 7 true 1        -- II:  Serpentine, 0.5× +7
  , mkHead 5 1 (-12) true 3    -- III: Spiral, 2.0× rev −12
  , mkHead 2 2 3 true 2        -- IV:  Columns, 0.75× pend +3
  ]

defaultCells :: Array Cell
defaultCells =
  mapWithIndex (\i _ -> { note: 60 + i, skip: false, gate: true, glide: false })
    (replicate 16 unit)

defaultOdonus :: Odonus
defaultOdonus = { cells: defaultCells, heads: defaultHeads }

-- ---------------------------------------------------------------------------
-- traversal — walk the head's pattern ordering, skip-aware
-- ---------------------------------------------------------------------------

modPos :: Int -> Int -> Int
modPos a b = ((a `mod` b) + b) `mod` b

skipAt :: Array Cell -> Int -> Boolean
skipAt cells i = maybe false _.skip (cells !! i)

gridAt :: Array Int -> Int -> Int
gridAt order pos = fromMaybe 0 (order !! pos)

data Dir = Fwd | Back | Pend

decodeDir :: Int -> Dir
decodeDir n
  | n <= 0 = Fwd
  | n == 1 = Back
  | otherwise = Pend

-- | Next seq position in `dir`, hopping positions whose grid cell is skipped.
nextSeq :: Array Int -> Array Cell -> Int -> Int -> Int
nextSeq order cells start dir = go (modPos (start + dir) 16) 0
  where
  go pos n
    | n >= 16 = start
    | not (skipAt cells (gridAt order pos)) = pos
    | otherwise = go (modPos (pos + dir) 16) (n + 1)

stepSeq :: Array Int -> Array Cell -> Dir -> { pos :: Int, pend :: Int } -> { pos :: Int, pend :: Int }
stepSeq order cells dir st = case dir of
  Fwd -> { pos: nextSeq order cells st.pos 1, pend: st.pend }
  Back -> { pos: nextSeq order cells st.pos (-1), pend: st.pend }
  Pend ->
    let
      ns
        | st.pos == 0 && st.pend == (-1) = 1
        | st.pos == 15 && st.pend == 1 = -1
        | otherwise = st.pend
    in
      { pos: nextSeq order cells st.pos ns, pend: ns }

advanceSeqN :: Array Int -> Array Cell -> Dir -> Int -> { pos :: Int, pend :: Int } -> { pos :: Int, pend :: Int }
advanceSeqN order cells dir n st
  | n <= 0 = st
  | otherwise = advanceSeqN order cells dir (n - 1) (stepSeq order cells dir st)

advanceHead :: Array Cell -> Head -> Head
advanceHead cells h =
  let
    order = orderOf h.patternIx
    newAcc = h.accumulator + speedOf h
    steps = floor newAcc
    remain = newAcc - toNumber steps
    r = advanceSeqN order cells (decodeDir h.direction) steps { pos: h.seqPos, pend: h.pendStep }
  in
    h { seqPos = r.pos, cursor = gridAt order r.pos, accumulator = remain, pendStep = r.pend }

step :: Odonus -> Odonus
step o = o { heads = map (advanceHead o.cells) o.heads }

cursorsOf :: Odonus -> Array Int
cursorsOf o = map _.cursor o.heads

-- | A note a head emits this tick: which head, and the resulting pitch
-- | (cell note + the head's transposition).
type Fired = { headIdx :: Int, pitch :: Int }

-- | Advance one tick and report what fired: an unmuted head that MOVED onto a
-- | gated, non-skipped cell emits its note. (A head that didn't advance this
-- | tick — speed < 1 — holds, it doesn't retrigger.)
stepEmit :: Odonus -> { odo :: Odonus, fired :: Array Fired }
stepEmit o =
  let
    oldCursors = map _.cursor o.heads
    o2 = step o
    firedFor idx hd =
      let moved = hd.cursor /= fromMaybe (-1) (oldCursors !! idx)
      in case o2.cells !! hd.cursor of
        Just c | moved && not hd.mute && c.gate && not c.skip ->
          Just { headIdx: idx, pitch: c.note + hd.transp }
        _ -> Nothing
  in
    { odo: o2, fired: catMaybes (mapWithIndex firedFor o2.heads) }

-- ---------------------------------------------------------------------------
-- editing
-- ---------------------------------------------------------------------------

editCell :: Int -> (Cell -> Cell) -> Odonus -> Odonus
editCell i f o = o { cells = fromMaybe o.cells (modifyAt i f o.cells) }

editHead :: Int -> (Head -> Head) -> Odonus -> Odonus
editHead h f o = o { heads = fromMaybe o.heads (modifyAt h f o.heads) }

clampI :: Int -> Int -> Int -> Int
clampI lo hi v = if v < lo then lo else if v > hi then hi else v

toggleSkip :: Int -> Odonus -> Odonus
toggleSkip i = editCell i \c -> c { skip = not c.skip }

toggleGate :: Int -> Odonus -> Odonus
toggleGate i = editCell i \c -> c { gate = not c.gate }

toggleGlide :: Int -> Odonus -> Odonus
toggleGlide i = editCell i \c -> c { glide = not c.glide }

setNote :: Int -> Int -> Odonus -> Odonus
setNote i v = editCell i \c -> c { note = v }

setHeadSpeedIx :: Int -> Int -> Odonus -> Odonus
setHeadSpeedIx h v = editHead h \hd -> hd { speedIx = clampI 0 (length speedTable - 1) v }

setHeadDir :: Int -> Int -> Odonus -> Odonus
setHeadDir h v = editHead h \hd -> hd { direction = clampI 0 2 v }

setHeadTransp :: Int -> Int -> Odonus -> Odonus
setHeadTransp h v = editHead h \hd -> hd { transp = clampI (-24) 24 v }

toggleHeadMute :: Int -> Odonus -> Odonus
toggleHeadMute h = editHead h \hd -> hd { mute = not hd.mute }

-- | Advance a head to the next pattern in the library (resets its position).
cyclePattern :: Int -> Odonus -> Odonus
cyclePattern h = editHead h \hd ->
  let ni = (hd.patternIx + 1) `mod` length patternLibrary
  in hd { patternIx = ni, seqPos = 0, cursor = gridAt (orderOf ni) 0 }
