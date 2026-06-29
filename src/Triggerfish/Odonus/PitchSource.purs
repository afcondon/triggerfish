-- | Triggerfish.Odonus.PitchSource — the single pluggable pitch source the
-- | quantization reframe describes: Odonus snaps its chromatic knob-values to
-- | ONE source, which is either a plain scale, a static chord progression, or a
-- | live Vetula voice. A scale is just the constant-set degenerate case of "a
-- | sequence of allowed pitch-sets"; Vetula yields borrowed/out-of-scale tones
-- | and key changes that are taken AS-IS (no re-snap).
-- |
-- | Today this type is a **print/parse intermediary**: it is *derived* from the
-- | model's chord overlay (`Odonus.chord`) plus the live Vetula follow id (which
-- | lives on the component State), and `applyPitchSource` writes back into those
-- | same fields. Promoting it to the model's stored truth — and collapsing
-- | `renderCell`'s scale-then-chord double snap into the one source-keyed snap —
-- | is a deliberate, audio-sensitive follow-up, kept out of the format work so
-- | the eDSL contract can land without changing how the running grid sounds.
-- | Because the Lepidoptera grammar speaks `PitchSource`, that later promotion
-- | won't touch the format.
module Triggerfish.Odonus.PitchSource
  ( PitchSource(..)
  , pitchSourceFrom
  , applyPitchSource
  ) where

import Prelude

import Data.Array (null)
import Data.Maybe (Maybe(..))
import Triggerfish.Odonus.Model as M

-- | The active source driving the final pitch snap.
data PitchSource
  = PScale                                 -- snap to the scale (the `scale:` line)
  | PChordsMcMullen (Array Int) Int        -- McMullen Yellow picks (key-relative), period
  | PChordsPCs (Array (Array Int)) Int     -- explicit PC sets (absolute), period
  | PVetula Int                            -- follow Vetula voice <id> (live feed)

derive instance eqPitchSource :: Eq PitchSource

-- | Read the active source from the model's harmony fields + the follow id. A
-- | live Vetula follow wins (it overrides the static progression each poll);
-- | otherwise the chord overlay decides — a non-empty feed is an explicit
-- | (absolute) progression, an empty feed falls back to the McMullen picks —
-- | and with the overlay off it's the plain scale.
pitchSourceFrom :: M.Odonus -> Maybe Int -> PitchSource
pitchSourceFrom o = case _ of
  Just fid -> PVetula fid
  Nothing ->
    if not o.chord.on then PScale
    else if not (null o.chord.feed) then PChordsPCs o.chord.feed o.chord.period
    else PChordsMcMullen o.chord.picks o.chord.period

-- | Install a source onto the model, returning the new `odo` plus the follow id
-- | the State should adopt. The live Vetula poll fills `chord.feed` every tick,
-- | so `PVetula` just arms the overlay and sets the follow; the scale and static
-- | cases clear the follow so no live poll overwrites them.
applyPitchSource :: PitchSource -> M.Odonus -> { odo :: M.Odonus, follow :: Maybe Int }
applyPitchSource src o = case src of
  PScale ->
    { odo: o { chord = o.chord { on = false, feed = [], ix = 0, phase = 0 } }, follow: Nothing }
  PChordsMcMullen picks per ->
    { odo: o { chord = o.chord { on = true, picks = picks, feed = [], period = per, ix = 0, phase = 0 } }
    , follow: Nothing }
  PChordsPCs sets per ->
    { odo: o { chord = o.chord { on = true, feed = sets, period = per, ix = 0, phase = 0 } }
    , follow: Nothing }
  PVetula fid ->
    { odo: o { chord = o.chord { on = true, ix = 0, phase = 0 } }, follow: Just fid }
