-- | **Runs: the stretches a machine played** (AC, 2026-10-03). The Review
-- | surface draws only time inside a run, so stopping and starting again
-- | leaves no gap: the new notes join the last at a seam, as on a tape that
-- | was stopped and started. The pauses are sliced out by the transport's
-- | own start and stop times, never guessed from gaps in the notes, and the
-- | notes keep their real times: only the drawing skips.
-- |
-- | A run is `{ from, to }` in the surface's microseconds, `to` Nothing while
-- | it is still going. The logbook keeps them newest first. With none (a
-- | logbook from before runs), the surface draws real time, as it did.
module Triggerfish.Capture.Runs
  ( Run
  , Axis
  , running
  , startRun
  , stopRun
  , axis
  ) where

import Prelude

import Data.Array (drop, foldl, head, reverse, uncons, (:))
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), fromMaybe)

type Run = { from :: Number, to :: Maybe Number }

-- | Whether the newest run is still going.
running :: Array Run -> Boolean
running runs = case head runs of
  Just { to: Nothing } -> true
  _ -> false

-- | The transport started at `now`: a new run, unless one is going.
startRun :: Number -> Array Run -> Array Run
startRun now runs = if running runs then runs else { from: now, to: Nothing } : runs

-- | The transport stopped at `now`: the run going ends.
stopRun :: Number -> Array Run -> Array Run
stopRun now runs = case uncons runs of
  Just { head: r@{ to: Nothing }, tail } -> r { to = Just now } : tail
  _ -> runs

-- | The surface's time axis: a real time to a fraction along it and back,
-- | how much played time it spans, and where the seams fall (fractions).
type Axis =
  { toFrac :: Number -> Number
  , fromFrac :: Number -> Number
  , span :: Number
  , seams :: Array Number
  }

-- | The axis over played time from `lo` to `lo + span` (played µs), for runs
-- | newest first.
axis :: Array Run -> { lo :: Number, span :: Number } -> Axis
axis runsNewest w =
  { toFrac: \t -> (played t - w.lo) / w.span
  , fromFrac: \x -> unplayed (w.lo + x * w.span)
  , span: w.span
  , seams: map (\r -> (played r.from - w.lo) / w.span) (drop 1 runs)
  }
  where
  runs = reverse runsNewest
  end r = fromMaybe infinity r.to
  -- played time up to `t`: the time inside runs before it
  played t = case runs of
    [] -> t
    _ -> sum (map (\r -> max 0.0 (min t (end r) - r.from)) runs)
  -- the real time at played time `p`
  unplayed p = case runs of
    [] -> p
    _ ->
      let
        step acc r = case acc.at of
          Just _ -> acc
          Nothing ->
            let len = end r - r.from
            in if p <= acc.played + len then acc { at = Just (r.from + (p - acc.played)) }
               else acc { played = acc.played + len, lastEnd = end r }
        out = foldl step { played: 0.0, at: Nothing, lastEnd: 0.0 } runs
      in fromMaybe (out.lastEnd + (p - out.played)) out.at

infinity :: Number
infinity = 1.0e300
