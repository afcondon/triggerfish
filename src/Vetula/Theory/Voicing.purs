-- | `Vetula.Theory.Voicing` — voicings + the voice-leading engine.
-- |
-- | VENDORED, verbatim, from `purerl-tidal/src/Tidal/Vetula/Voicing.purs`
-- | (module name + the `Vetula.Theory` import differ from the original's
-- | `Tidal.Vetula`). This is the algorithm the whole app exists to make
-- | visible/audible: `voiceLead :: Voicing -> Chord -> Voicing` and
-- | `enumerateVoicings :: Voicing -> Chord -> Array (Tuple Voicing Int)`
-- | (all voicings of a chord RANKED by total semitone motion).  The motion
-- | score becomes link distance on the Hylograph force surface.
module Vetula.Theory.Voicing
  ( Voicing(..)
  , voicingMidi
  , closeVoicing
  -- Primitives
  , openTriad
  , rootless
  , drop2
  , drop2and4
  , quartal
  , cluster
  , spread
  -- Composition alias
  , VoicingStrategy
  -- Selectors
  , Selector(..)
  , takeChord
  , takeVoicing
  -- Voice leading (V-C)
  , Progression
  , voiceLead
  , enumerateVoicings
  , play
  , playFrom
  , nearestNote
  ) where

import Prelude

import Data.Array as Array
import Data.Array (cons, deleteAt, filter, nub, range, sort, (!!), zipWith)
import Data.Foldable (elem, foldl, sum)
import Data.Function (on)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst)

import Vetula.Theory (Chord(..), DegreeChord, Key, realize)

-- ---------------------------------------------------------------------------
-- Voicing — sorted-ascending array of MIDI note numbers
-- ---------------------------------------------------------------------------

newtype Voicing = Voicing (Array Int)

derive instance eqVoicing :: Eq Voicing

instance showVoicing :: Show Voicing where
  show (Voicing xs) = "Voicing " <> show xs

-- | Extract the MIDI note numbers from a voicing.
voicingMidi :: Voicing -> Array Int
voicingMidi (Voicing xs) = xs

-- ---------------------------------------------------------------------------
-- closeVoicing — the one Chord → Voicing lift
-- ---------------------------------------------------------------------------

closeVoicing :: { centre :: Int } -> Chord -> Voicing
closeVoicing { centre } (Chord pcs) =
  Voicing (map (\pc -> pc + 12 * (centre + 1)) (sort pcs))

-- ---------------------------------------------------------------------------
-- Voicing transformations — composable through (<<<)
-- ---------------------------------------------------------------------------

type VoicingStrategy = Voicing -> Voicing

openTriad :: Voicing -> Voicing
openTriad (Voicing xs) =
  if Array.length xs < 2 then Voicing xs
  else case xs !! 1, deleteAt 1 xs of
    Just second, Just rest -> Voicing (sort (cons (second + 12) rest))
    _, _ -> Voicing xs

rootless :: Voicing -> Voicing
rootless (Voicing xs) = case Array.uncons xs of
  Just { tail } -> Voicing tail
  Nothing -> Voicing xs

drop2 :: Voicing -> Voicing
drop2 (Voicing xs) =
  let n = Array.length xs
  in if n < 2 then Voicing xs
     else case xs !! (n - 2), deleteAt (n - 2) xs of
       Just second, Just rest -> Voicing (sort (cons (second - 12) rest))
       _, _ -> Voicing xs

drop2and4 :: Voicing -> Voicing
drop2and4 v@(Voicing xs) =
  let n = Array.length xs
  in if n < 4 then drop2 v
     else
       case xs !! (n - 2), xs !! (n - 4) of
         Just two, Just four ->
           case deleteAt (n - 2) xs >>= deleteAt (n - 4) of
             Just rest -> Voicing (sort (cons (two - 12) (cons (four - 12) rest)))
             Nothing -> Voicing xs
         _, _ -> Voicing xs

quartal :: Voicing -> Voicing
quartal (Voicing xs) = case Array.uncons xs of
  Nothing -> Voicing []
  Just { head: bottom } ->
    let
      pcsPresent = nub (map (\n -> n `mod` 12) xs)
      bottomPc = bottom `mod` 12
      cycleOrder = filter (\pc -> elem pc pcsPresent)
                          (map (\i -> (bottomPc + 5 * i) `mod` 12) (range 0 11))
      restPcs = fromMaybe [] (Array.tail cycleOrder)
      stacked = foldl placeNext [bottom] restPcs
    in
      Voicing stacked
  where
    placeNext acc pc = case Array.last acc of
      Nothing -> acc
      Just prev ->
        let
          target = prev + 5
          base = (target `div` 12) * 12
          candidate = base + pc
          n = if candidate >= target then candidate else candidate + 12
        in acc <> [n]

cluster :: Voicing -> Voicing
cluster (Voicing xs) = case Array.uncons xs of
  Nothing -> Voicing []
  Just { head: bottom } ->
    let
      bottomOct = bottom `div` 12
      pcs = sort (nub (map (\n -> n `mod` 12) xs))
    in
      Voicing (map (\pc -> pc + 12 * bottomOct) pcs)

spread :: { low :: Int, high :: Int } -> Voicing -> Voicing
spread { low, high } (Voicing xs) =
  let n = Array.length xs
  in if n == 0 then Voicing []
     else if n == 1
       then Voicing (map (\note -> shiftToOctave low note) xs)
     else
       let
         lowMidi = 12 * (low + 1)
         highMidi = 12 * (high + 1) + 11
         span = highMidi - lowMidi
         step = if n <= 1 then 0 else span / (n - 1)
         placed = Array.mapWithIndex
           (\i note ->
              let
                pc = note `mod` 12
                targetMidi = lowMidi + step * i
                targetOctave = targetMidi `div` 12
                candidate = pc + 12 * targetOctave
              in
                nearest candidate targetMidi)
           xs
       in
         Voicing (sort placed)
  where
    shiftToOctave o n = (n `mod` 12) + 12 * (o + 1)
    nearest candidate target =
      let
        below = candidate - 12
        above = candidate + 12
        d0 = absVal (candidate - target)
        dBelow = absVal (below - target)
        dAbove = absVal (above - target)
      in
        if dBelow < d0 && dBelow <= dAbove then below
        else if dAbove < d0 then above
        else candidate
    absVal n = if n < 0 then -n else n

-- ---------------------------------------------------------------------------
-- Selectors — sub-chord plumbing
-- ---------------------------------------------------------------------------

data Selector
  = TakeLow Int             -- ^ The N lowest voices.
  | TakeHigh Int            -- ^ The N highest voices.
  | TakeRange Int Int       -- ^ Voices [i..j) — half-open.
  | TakeIndices (Array Int) -- ^ Explicit voice indices (0-based, low-to-high).
  | TakeEvery Int Int       -- ^ (offset, stride) — modulo selector.
  | DropS Selector          -- ^ Complement of a selector.

derive instance eqSelector :: Eq Selector

instance showSelector :: Show Selector where
  show = case _ of
    TakeLow n        -> "TakeLow " <> show n
    TakeHigh n       -> "TakeHigh " <> show n
    TakeRange i j    -> "TakeRange " <> show i <> " " <> show j
    TakeIndices xs   -> "TakeIndices " <> show xs
    TakeEvery o s    -> "TakeEvery " <> show o <> " " <> show s
    DropS s          -> "DropS (" <> show s <> ")"

takeChord :: Selector -> Chord -> Chord
takeChord sel (Chord pcs) =
  Chord (selectFrom sel (sort (nub pcs)))

takeVoicing :: Selector -> Voicing -> Voicing
takeVoicing sel (Voicing notes) =
  Voicing (selectFrom sel notes)

selectFrom :: forall a. Selector -> Array a -> Array a
selectFrom sel xs =
  let ixs = selectIndices sel (Array.length xs)
  in  Array.mapMaybe (\i -> xs !! i) ixs

selectIndices :: Selector -> Int -> Array Int
selectIndices sel len = case sel of
  TakeLow n
    | n <= 0 || len <= 0 -> []
    | otherwise          -> allIndices (min n len)
  TakeHigh n
    | n <= 0 || len <= 0 -> []
    | otherwise          ->
        let start = max 0 (len - n)
        in if start >= len then [] else rangeIncl start (len - 1)
  TakeRange i j ->
    let lo = max 0 i
        hi = min len (max 0 j) - 1
    in if hi < lo then [] else rangeIncl lo hi
  TakeIndices ixs ->
    filter (\i -> i >= 0 && i < len) ixs
  TakeEvery offset stride ->
    let validStride = if stride < 1 then 1 else stride
        countMax = if validStride == 0 then 0 else (len + validStride) / validStride
        candidates = map (\k -> offset + k * validStride) (allIndices countMax)
    in  filter (\i -> i >= 0 && i < len) candidates
  DropS inner ->
    let kept = selectIndices inner len
    in  filter (\i -> not (elem i kept)) (allIndices len)

allIndices :: Int -> Array Int
allIndices len
  | len <= 0  = []
  | otherwise = rangeIncl 0 (len - 1)

rangeIncl :: Int -> Int -> Array Int
rangeIncl lo hi
  | hi < lo   = []
  | otherwise = range lo hi

-- ---------------------------------------------------------------------------
-- Voice leading — V-C
-- ---------------------------------------------------------------------------

type Progression = Array DegreeChord

voiceLead :: Voicing -> Chord -> Voicing
voiceLead (Voicing []) chord =
  closeVoicing { centre: 4 } chord
voiceLead voicing@(Voicing current) (Chord pcs) =
  let
    nextPcs = nub pcs
    n = Array.length current
    m = Array.length nextPcs
  in
    if n /= m
      then closeVoicing { centre: bottomOctave voicing } (Chord nextPcs)
      else case fst <$> bestPerm current nextPcs of
        Just v  -> v
        Nothing -> closeVoicing { centre: bottomOctave voicing } (Chord nextPcs)

enumerateVoicings :: Voicing -> Chord -> Array (Tuple Voicing Int)
enumerateVoicings (Voicing []) chord =
  [ Tuple (closeVoicing { centre: 4 } chord) 0 ]
enumerateVoicings voicing@(Voicing current) (Chord pcs) =
  let
    nextPcs = nub pcs
    n = Array.length current
    m = Array.length nextPcs
  in
    if n /= m
      then [ Tuple (closeVoicing { centre: bottomOctave voicing } (Chord nextPcs)) 0 ]
      else
        Array.sortBy (compare `on` snd)
          (Array.nubByEq (\a b -> fst a == fst b)
             (map (scorePerm current) (permutations nextPcs)))
  where
    snd (Tuple _ s) = s

playFrom :: Int -> Key -> VoicingStrategy -> Progression -> Array Voicing
playFrom centre key strategy chords =
  case Array.uncons chords of
    Nothing -> []
    Just { head: first, tail: rest } ->
      let
        firstV = strategy (closeVoicing { centre } (realize key first))
      in
        cons firstV
          (Array.scanl (\prev dc -> voiceLead prev (realize key dc)) firstV rest)

play :: Key -> VoicingStrategy -> Progression -> Array Voicing
play = playFrom 4

-- ---------------------------------------------------------------------------
-- Voice-leading internals
-- ---------------------------------------------------------------------------

bestPerm :: Array Int -> Array Int -> Maybe (Tuple Voicing Int)
bestPerm current nextPcs = case permutations nextPcs of
  [] -> Nothing
  perms ->
    let scored = map (scorePerm current) perms
        sorted = Array.sortBy (compare `on` (\(Tuple _ s) -> s)) scored
    in  Array.head sorted

scorePerm :: Array Int -> Array Int -> Tuple Voicing Int
scorePerm current perm =
  let
    placed = zipWith nearestNote current perm
    motion = sum (zipWith (\c p -> absInt (c - p)) current placed)
  in
    Tuple (Voicing (sort placed)) motion

nearestNote :: Int -> Int -> Int
nearestNote target pc =
  let
    base = (target - pc) `div` 12
    c0 = pc + 12 * base
    cAbove = c0 + 12
    cBelow = c0 - 12
    d0 = absInt (c0 - target)
    dA = absInt (cAbove - target)
    dB = absInt (cBelow - target)
  in
    if dA < d0 && dA <= dB then cAbove
    else if dB < d0 && dB < dA then cBelow
    else c0

bottomOctave :: Voicing -> Int
bottomOctave (Voicing xs) = case Array.head xs of
  Just n  -> (n `div` 12) - 1
  Nothing -> 4

permutations :: forall a. Array a -> Array (Array a)
permutations xs = case Array.length xs of
  0 -> [[]]
  _ ->
    Array.concatMap
      (\i -> case xs !! i, deleteAt i xs of
         Just x, Just rest -> map (cons x) (permutations rest)
         _, _              -> [])
      (allIndices (Array.length xs))

absInt :: Int -> Int
absInt n = if n < 0 then -n else n
