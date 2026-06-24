-- | Triggerfish.Balistes.Tidal — the bridge between the vendored purerl-tidal
-- | mini-notation engine (`Tidal.*`) and a Balistes pad lane.
-- |
-- | A pad lane is a *pattern*, not a fixed grid. Two layers merge (`stack`):
-- | the typed mini-notation `source` and the hand-clicked overlay `clicks`.
-- | Everything the rest of the app needs about a lane is derived here:
-- |
-- |   • its **meter** — the visible cell count, computed structurally from the
-- |     parsed pattern (a top-level seq of 4 → 4 cells, `bd*3` → 3, `bd(3,8)`
-- |     → 8). Lanes disagree on meter freely; that is the polymeter.
-- |   • its **cell mask** — which of those cells light up (source onset in the
-- |     cell, or a click), for rendering the row.
-- |   • its **cycle onsets** — the true fractional onset times in [0,1) of the
-- |     merged pattern, for scheduling. Nothing snaps to the Grids 32-grid.
-- |
-- | Onsets fire on the event's *whole start* (the note's beginning), so a held
-- | or clipped event is never retriggered.
module Triggerfish.Balistes.Tidal
  ( laneMeterAt
  , laneCellMaskAt
  , laneSourceOnsetsAt
  , laneCycleOnsetsAt
  , setLaneSource
  , toggleLaneClick
  , lookupLane
  , routeEvents
  , routedOnsetsForLane
  ) where

import Prelude

import Data.Array (catMaybes, concatMap, find, head, length, mapMaybe, mapWithIndex, nub, range, sort, updateAt, zipWith, (!!))
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Rational as R
import Data.String as Str
import Tidal.AST.Types (Located(..), TPat(..))
import Tidal.Pattern.Core (queryArc)
import Tidal.Pattern.Mini (parseMiniPattern)
import Tidal.Pattern.Types (Arc(..), Event, eventValue, eventWhole)
import Tidal.Parse.Parser (parse)
import Triggerfish.Balistes.Model (Balistes)
import Triggerfish.Balistes.Model as M

-- ---------------------------------------------------------------------------
-- Meter — the cell count, derived from the pattern's structure
-- ---------------------------------------------------------------------------

-- | The visible cell count for a lane: structural meter of the parsed source,
-- | or (when blank/unparseable) the current clicks grid so there's always
-- | something to click.
laneMeterAt :: Balistes -> Int -> Int
laneMeterAt b i = meterFor (M.padSource b i) (M.padClicks b i)

meterFor :: String -> Array Boolean -> Int
meterFor src clicks
  | Str.null (Str.trim src) = max 1 (length clicks)
  | otherwise = case parse src of
      Right t -> max 1 (patternMeter t)
      Left _ -> max 1 (length clicks)

-- | The top-level subdivision a pattern expresses: how many "cells" it draws.
-- | A flat sequence is its length; `bd*n` is n; a euclid `bd(k,steps)` is its
-- | steps; nesting counts only at the top level (`[bd sn] cp` → 2).
patternMeter :: forall a. TPat a -> Int
patternMeter = case _ of
  -- a single-element sequence is transparent (the parser wraps `bd*3` as
  -- `Seq [Fast 3 bd]`); look inside so the meter is the child's, not 1.
  TPat_Seq _ [ single ] -> patternMeter single
  TPat_Seq _ xs -> max 1 (length xs)
  TPat_Euclid _ _ stepsT _ _ -> max 1 (fromMaybe 1 (litInt stepsT))
  TPat_Fast _ rT x -> max 1 (ratToInt rT * patternMeter x)
  TPat_Slow _ _ x -> patternMeter x
  TPat_Stack _ xs -> fromMaybe 1 (patternMeter <$> head xs)
  TPat_Polyrhythm _ _ xs -> fromMaybe 1 (patternMeter <$> head xs)
  _ -> 1

litInt :: TPat Int -> Maybe Int
litInt = case _ of
  TPat_Atom (Located _ i) -> Just i
  _ -> Nothing

ratToInt :: TPat R.Rational -> Int
ratToInt = case _ of
  TPat_Atom (Located _ r) -> max 1 (Int.round (R.toNumber r))
  _ -> 1

-- ---------------------------------------------------------------------------
-- Onsets — true fractional event times over one cycle
-- ---------------------------------------------------------------------------

-- | The source pattern's onset fractions in [0,1), sorted and deduped. Only the
-- | beginning of each event counts (we read `whole.start`), so sustained or
-- | clipped events never produce a phantom retrigger.
laneSourceOnsetsAt :: Balistes -> Int -> Array Number
laneSourceOnsetsAt b i = sourceOnsets (M.padSource b i)

sourceOnsets :: String -> Array Number
sourceOnsets src = case parseMiniPattern src of
  Right pat -> uniqSort (mapMaybe onsetFrac (queryArc pat (R.fromInt 0) (R.fromInt 1)))
  Left _ -> []

onsetFrac :: forall a. Event a -> Maybe Number
onsetFrac e = case eventWhole e of
  Just (Arc { start }) ->
    let f = R.toNumber start
    in if f >= 0.0 && f < 1.0 then Just f else Nothing
  Nothing -> Nothing

-- | The merged onset fractions of the whole lane — source `stack`ed with the
-- | clicked overlay (each click at cell k is an onset at k/meter). This is what
-- | the scheduler fires.
laneCycleOnsetsAt :: Balistes -> Int -> Array Number
laneCycleOnsetsAt b i =
  let
    m = laneMeterAt b i
    clk = resizeBool m (M.padClicks b i)
    clickOns = catMaybes (mapWithIndex (\k on -> if on then Just (Int.toNumber k / Int.toNumber m) else Nothing) clk)
  in
    uniqSort (sourceOnsets (M.padSource b i) <> clickOns)

-- ---------------------------------------------------------------------------
-- Cell mask — which derived cells light up (for rendering)
-- ---------------------------------------------------------------------------

-- | A meter-length boolean: cell k is on if the source has an onset inside it,
-- | or the clicked overlay has it set. The faint click grid the row draws.
laneCellMaskAt :: Balistes -> Int -> Array Boolean
laneCellMaskAt b i =
  let
    m = laneMeterAt b i
    ons = sourceOnsets (M.padSource b i)
    srcCell = map (\cell -> any (\o -> bucket m o == cell) ons) (range 0 (m - 1))
    clk = resizeBool m (M.padClicks b i)
  in
    zipWith (||) srcCell clk

bucket :: Int -> Number -> Int
bucket m o = M.clampI 0 (m - 1) (Int.floor (o * Int.toNumber m))

-- ---------------------------------------------------------------------------
-- Mutation — meter-aware
-- ---------------------------------------------------------------------------

-- | Set a lane's mini-notation source, recompute its meter, and resize the
-- | clicked overlay to match (preserving overlapping cells).
setLaneSource :: Int -> String -> Balistes -> Balistes
setLaneSource i src b =
  let
    oldClicks = M.padClicks b i
    m = meterFor src oldClicks
  in
    M.setPadClicks i (resizeBool m oldClicks) (M.setPadSource i src b)

-- | Toggle one cell of a lane's clicked overlay.
toggleLaneClick :: Int -> Int -> Balistes -> Balistes
toggleLaneClick i cell b =
  let
    cs = M.padClicks b i
    cur = fromMaybe false (cs !! cell)
  in
    M.setPadClicks i (fromMaybe cs (updateAt cell (not cur) cs)) b

-- ---------------------------------------------------------------------------
-- Routing patterns — `s`-style multi-voice, atoms route to lanes by label
-- ---------------------------------------------------------------------------

-- | Which pad lane (if any) an atom name addresses — a case-insensitive match
-- | against the lane labels. Routing reaches the Tidal kit + OH, never the
-- | generative Grids voices (which aren't pad lanes).
lookupLane :: Balistes -> String -> Maybe Int
lookupLane b name =
  let key = Str.toLower (Str.trim name)
  in find (\i -> Str.toLower (M.padName b i) == key) (range 0 (M.padCount b - 1))

-- | The named onsets of a routing pattern over one cycle: each event's atom
-- | value and its true fractional start.
routeEvents :: String -> Array { name :: String, at :: Number }
routeEvents src = case parseMiniPattern src of
  Right pat -> mapMaybe namedOnset (queryArc pat (R.fromInt 0) (R.fromInt 1))
  Left _ -> []

namedOnset :: Event String -> Maybe { name :: String, at :: Number }
namedOnset e = case eventWhole e of
  Just (Arc { start }) ->
    let f = R.toNumber start
    in if f >= 0.0 && f < 1.0 then Just { name: eventValue e, at: f } else Nothing
  Nothing -> Nothing

-- | All routing onsets (across every routing slot) that resolve to pad lane `i`
-- | — the hits this lane receives from the routing layer, at true times.
routedOnsetsForLane :: Balistes -> Int -> Array Number
routedOnsetsForLane b i =
  uniqSort (concatMap forRoute b.routes)
  where
  forRoute src = mapMaybe (\ev -> if lookupLane b ev.name == Just i then Just ev.at else Nothing) (routeEvents src)

-- ---------------------------------------------------------------------------
-- helpers
-- ---------------------------------------------------------------------------

-- | Resize a boolean array to length n, padding with false / truncating.
resizeBool :: Int -> Array Boolean -> Array Boolean
resizeBool n xs
  | n <= 0 = []
  | otherwise = map (\k -> fromMaybe false (xs !! k)) (range 0 (n - 1))

uniqSort :: Array Number -> Array Number
uniqSort = nub <<< sort
