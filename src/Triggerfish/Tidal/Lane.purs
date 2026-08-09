-- | Triggerfish.Tidal.Lane — the pure bridge between a Tidal mini-notation
-- | string and the three things a UI lane needs from it: its **meter** (visible
-- | cell count, derived structurally), its **onsets** (true fractional event
-- | times in [0,1)), and its **cell mask** (which derived cells light up).
-- |
-- | String-in, data-out — no app state. Shared by every lane-shaped surface:
-- | Selene's trigger-lane GenKind today, and the basis the Balistes kit will
-- | fold onto when it migrates. A trigger lane is NOT a drum lane: the atom
-- | names are irrelevant here (only onset *times* matter), so the same engine
-- | drives a gate to an envelope, a clock to Lubadh, or a note to a kit.
module Triggerfish.Tidal.Lane
  ( meterOf
  , onsetsOf
  , cellMaskOf
  , euclidOf
  , namedOnsetsOf
  ) where

import Prelude

import Data.Array (mapMaybe, nub, range, sort)
import Data.Array as Array
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

-- ---------------------------------------------------------------------------
-- Meter — the cell count, derived from the pattern's structure
-- ---------------------------------------------------------------------------

-- | The visible cell count for a source: structural meter of the parsed
-- | pattern, or 1 when blank/unparseable so a row never collapses.
meterOf :: String -> Int
meterOf src
  | Str.null (Str.trim src) = 1
  | otherwise = case parse src of
      Right t -> max 1 (patternMeter t)
      Left _ -> 1

-- | The top-level subdivision a pattern expresses: a flat sequence is its
-- | length; `x*n` is n; a euclid `x(k,steps)` is its steps; nesting counts only
-- | at the top level (`[x y] z` → 2).
patternMeter :: forall a. TPat a -> Int
patternMeter = case _ of
  -- a single-element sequence is transparent (the parser wraps `x*3` as
  -- `Seq [Fast 3 x]`); look inside so the meter is the child's, not 1.
  TPat_Seq _ [ single ] -> patternMeter single
  TPat_Seq _ xs -> max 1 (Array.length xs)
  TPat_Euclid _ _ stepsT _ _ -> max 1 (fromMaybe 1 (litInt stepsT))
  TPat_Fast _ rT x -> max 1 (ratToInt rT * patternMeter x)
  TPat_Slow _ _ x -> patternMeter x
  TPat_Stack _ xs -> fromMaybe 1 (patternMeter <$> Array.head xs)
  TPat_Polyrhythm _ _ xs -> fromMaybe 1 (patternMeter <$> Array.head xs)
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
onsetsOf :: String -> Array Number
onsetsOf src = case parseMiniPattern src of
  Right pat -> uniqSort (mapMaybe onsetFrac (queryArc pat (R.fromInt 0) (R.fromInt 1)))
  Left _ -> []

onsetFrac :: forall a. Event a -> Maybe Number
onsetFrac e = case eventWhole e of
  Just (Arc { start }) ->
    let f = R.toNumber start
    in if f >= 0.0 && f < 1.0 then Just f else Nothing
  Nothing -> Nothing

-- ---------------------------------------------------------------------------
-- Cell mask — which derived cells light up (for rendering)
-- ---------------------------------------------------------------------------

-- | A meter-length boolean: cell k is on iff the source has an onset inside it.
cellMaskOf :: String -> Array Boolean
cellMaskOf src =
  let
    m = meterOf src
    ons = onsetsOf src
  in
    map (\cell -> any (\o -> bucket m o == cell) ons) (range 0 (m - 1))

bucket :: Int -> Number -> Int
bucket m o = clamp 0 (m - 1) (Int.floor (o * Int.toNumber m))

-- ---------------------------------------------------------------------------
-- Structure detection — is this source a pure Euclid?  (for ring rendering)
-- ---------------------------------------------------------------------------

-- | If the source is a *single* top-level Euclid (`x(k,n)`, possibly wrapped in
-- | a one-element sequence by the parser), its k and n — so a trig jack carrying
-- | a Euclid can draw as the same ring PolyEuclid uses (and signal that it could
-- | be promoted to a delegatable PolyEuclid). Anything else is Nothing.
euclidOf :: String -> Maybe { k :: Int, n :: Int }
euclidOf src = case parse src of
  Right t -> go t
  Left _ -> Nothing
  where
  go = case _ of
    TPat_Seq _ [ single ] -> go single
    TPat_Euclid _ kT nT _ _ -> do
      k <- litInt kT
      n <- litInt nT
      pure { k, n }
    _ -> Nothing

-- ---------------------------------------------------------------------------
-- Named onsets — a lane-spanning route pattern's atoms + their true times
-- ---------------------------------------------------------------------------

-- | A routing pattern's events as (atom-name, fractional-onset) pairs over one
-- | cycle. A jack named `bd` receives the onsets whose name is `bd`; this is how
-- | `"bd sn cp sn"` distributes across the named jacks.
namedOnsetsOf :: String -> Array { name :: String, at :: Number }
namedOnsetsOf src = case parseMiniPattern src of
  Right pat -> mapMaybe namedOnset (queryArc pat (R.fromInt 0) (R.fromInt 1))
  Left _ -> []

namedOnset :: Event String -> Maybe { name :: String, at :: Number }
namedOnset e = case eventWhole e of
  Just (Arc { start }) ->
    let f = R.toNumber start
    in if f >= 0.0 && f < 1.0 then Just { name: eventValue e, at: f } else Nothing
  Nothing -> Nothing

-- ---------------------------------------------------------------------------
-- helpers
-- ---------------------------------------------------------------------------

uniqSort :: Array Number -> Array Number
uniqSort = nub <<< sort
