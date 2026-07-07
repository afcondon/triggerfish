-- | Triggerfish.Stellatus.Onsets — browser-side transient detection for the
-- | Amen-break import. Fetch a sample, decode it (Web Audio), run an energy-flux
-- | onset detector, and hand back the normalised transient positions plus a
-- | downsampled waveform for the ring.
-- |
-- | This is DELIBERATELY a frontend concern, upstream of the pure `Reef.Stellatus`
-- | Scene: the analysis produces primitive `begin`/`end` numbers (one per detected
-- | slice), exactly like the Tidal mini-notation parser produces primitive arcs.
-- | The BEAM never sees audio — it receives the resolved windows and plays them
-- | through SuperDirt. So the shipping cross-runtime wire is untouched.
-- |
-- | No sound is made here: decoding runs on an `OfflineAudioContext`, so we never
-- | grab the audio output (consistent with Stellatus being rig-only).
module Triggerfish.Stellatus.Onsets
  ( Detection
  , detect
  ) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Exception (Error)

-- | The result of analysing one buffer.
-- |   • `onsets` — sorted normalised transient positions in [0,1); always begins
-- |     with 0.0 (the loop start is a slice boundary). Consecutive pairs become a
-- |     slot's `begin`/`end`.
-- |   • `wave`   — downsampled abs-peak envelope (each 0..1), for drawing the real
-- |     waveform around the ring.
-- |   • `dur`    — the buffer length in seconds (for labelling / ms readouts).
type Detection =
  { onsets :: Array Number
  , wave :: Array Number
  , dur :: Number
  -- Per-slice timbre class (aligned with `onsets`): 0 = kick/low, 1 = snare/mid,
  -- 2 = hat/high. Colours the ring by hit type; later seeds the jump matrix.
  , classes :: Array Int
  }

foreign import detectImpl
  :: String
  -> Number
  -> (Error -> Effect Unit)
  -> (Detection -> Effect Unit)
  -> Effect Unit

-- | Detect transients in the sample at `url`. `sensitivity` in [0,1] lowers the
-- | peak-pick threshold (higher = more onsets). Runs in the browser; the result
-- | is cached and turned into Scene slots by the component.
detect :: String -> Number -> Aff Detection
detect url sensitivity = makeAff \cb -> do
  detectImpl url sensitivity (cb <<< Left) (cb <<< Right)
  pure nonCanceler
