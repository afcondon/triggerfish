-- | **Vetula's own pattern vocabulary**, on the Tidal engine.
-- |
-- | Not Tidal's functions, so not in the engine (`tidal-engine`), and not
-- | under Tidal's names: `arpRate` and `arpIndexed` were `arpeggiate` and
-- | `arpWith` in Triggerfish's old vendored copy, but Tidal's `arpeggiate`
-- | spreads a chord across its event and `arpWith` takes a function on event
-- | lists, and neither takes a rate or an index figure. `withSampledArg` and
-- | `cycleRand` serve Lepidoptera's per-layer arguments and `prob` gate.
-- | Tidal's own `when` is the engine's `whenCycle`.
module Vetula.Pattern
  ( arpRate
  , arpIndexed
  , withSampledArg
  , cycleRand
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number (floor, sin)
import Haskell.Rational (fromInt, toNumber)
import Tidal.Pattern.Types (Arc(..), Event(..), Pattern, State(..), emptyContext, eventValue, pattern, query)

-- | Arpeggiate: explode each event's ARRAY value across that event's OWN whole.
-- | An event carrying `[a, b, c]` over arc `w` becomes singleton `[a]`, `[b]`, `[c]`,
-- | `[a]`… events — `rate` steps per cycle of `w`, cycling the array — each occupying
-- | an equal slice of `w`. Because the slices are cut from the event's own whole this
-- | composes with `slow`/`fast` for free: stretch the chord and its arp stretches with
-- | it, so `slow 8 (arpRate 2 p)` unfolds the arp over eight cycles. Only events
-- | whose ONSET falls in the query are emitted, so a multi-cycle chord schedules each
-- | note exactly once, at its moment — no per-cycle re-trigger. Empty arrays vanish;
-- | analog events pass through untouched. Ordering (up/down/updown) is the caller's
-- | job: pre-`map` the array into the order you want, then arpRate cycles through it.
arpRate :: forall a. Int -> Pattern (Array a) -> Pattern (Array a)
arpRate rate pat = pattern \(State st) ->
  let Arc q = st.arc
  in Array.concatMap (burst q) (query pat (State st))
  where
  burst q = case _ of
    Analog e -> [ Analog e ]
    Digital e ->
      let Arc w = e.whole
          notes = e.value
          m = Array.length notes
          d = w.stop - w.start
          n = max 1 (Int.round (toNumber d * Int.toNumber rate))
          sd = d / fromInt n
      in if m == 0 then []
         else Array.mapMaybe (step q w.start notes m sd) (Array.range 0 (n - 1))
  step q ws notes m sd j =
    let onset = ws + fromInt j * sd
        stop = onset + sd
    in if onset >= q.start && onset < q.stop
       then map
              (\note -> Digital
                 { context: emptyContext
                 , whole: Arc { start: onset, stop }
                 , part: Arc { start: onset, stop: min stop q.stop }
                 , value: [ note ]
                 })
              (Array.index notes (mod j m))
       else Nothing

-- | Arpeggiate with an explicit INDEX FIGURE. Where `arpRate` walks a chord's
-- | notes in their given order, this drives the arp from a second pattern `ip` of
-- | *selectors* `b`: for each chord event, `ip` is queried WITHIN that chord's own
-- | active slot, and each of its steps picks a note via `sel chord step`. So the
-- | figure is cycle-aligned and repeats once per cycle of the chord — under `slow 8`
-- | the harmony holds while the figure keeps ticking each bar. `sel` returns Nothing
-- | for a step that selects nothing (a rest); the caller owns index conventions
-- | (sorting, octave-wrap past the top). Composes with slow/fast for free, same as
-- | `arpRate`, because every emitted event keeps the figure-step's own arc.
arpIndexed :: forall a b. (Array a -> b -> Maybe a) -> Pattern b -> Pattern (Array a) -> Pattern (Array a)
arpIndexed sel ip pat = pattern \(State st) ->
  Array.concatMap
    (case _ of
        Analog e -> [ Analog e ]
        Digital e -> Array.mapMaybe (pick e.value) (query ip (State (st { arc = e.part }))))
    (query pat (State st))
  where
  pick ns = case _ of
    Analog _ -> Nothing
    Digital je -> map (\v -> Digital (je { value = [ v ] })) (sel ns je.value)

-- | Transform each event's value by an ARGUMENT sampled from a second pattern at that
-- | event's onset. For every event of `pat`, `argp` is queried within the event's part
-- | and its first atom taken (or `""` when the arg is a rest there); `f atom value`
-- | produces the new value. This is how a verb takes a pattern-valued argument
-- | (`transpose "0 7 <5 3>"`): the result keeps `pat`'s structure (the chords), and the
-- | arg is sampled per chord — so patterning the chords AND the arg interlock (the
-- | fractal). `f "" v` must yield the verb's default (a rest in the arg = no-op).
withSampledArg :: forall a. (String -> a -> a) -> Pattern String -> Pattern a -> Pattern a
withSampledArg f argp pat = pattern \(State st) ->
  map (go st) (query pat (State st))
  where
  go st = case _ of
    Analog e -> Analog e
    Digital e ->
      let s = case Array.head (query argp (State (st { arc = e.part }))) of
                Just ae -> eventValue ae
                Nothing -> ""
      in Digital (e { value = f s e.value })

-- | A deterministic pseudo-random value in [0,1) keyed on a cycle number — the classic
-- | hashed-sine. Deterministic (no `Math.random`, which the runtime blocks and which
-- | would break resumability): the same cycle always yields the same value, so a
-- | `prob` gate is stable across re-queries within a bar and reproducible across runs.
cycleRand :: Int -> Number
cycleRand c =
  let v = sin (Int.toNumber c * 12.9898 + 78.233) * 43758.5453
  in v - floor v
