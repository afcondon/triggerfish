-- | `Vetula.Realise` — the FRONTEND chord→`Pattern` realiser for the Perform view.
-- |
-- | The Perform boxes carry a saved sequence (a list of chords, each an `Array Int`
-- | of MIDI notes) and realise it through the vendored Tidal engine
-- | (`Tidal.Pattern.Core`). Building the carrier as a real `Pattern Int` — rather
-- | than the ad-hoc beat-fire the first Perform slice used — is what lets the
-- | function-stack layers (`fast`/`ply`/`arp`/`every`/…) compose on it directly:
-- | every layer is a `Pattern a -> Pattern a`, the box's realiser is
-- | `foldl applyFx (fromChords …) stack`, and playback just QUERIES the resulting
-- | pattern per cycle and schedules the events (see `App.purs` PerfTick).
-- |
-- | This mirrors the pure `voicingAs*` helpers that live BEAM-side in
-- | `purerl-tidal/src/Tidal/Vetula/Pattern.purs`, ported to the frontend over raw
-- | `Array Int` note-sets (no `PitchedNote12`/`Voicing` dependency needed). The
-- | value carried is the MIDI note number itself.
-- |
-- | Convention here: **one pattern cycle = one Perform beat** (a chord's slot). A
-- | block chord fills the cycle; an arp fans its notes across it; `fast 2` doubles
-- | it; and so on — all by composition on the returned `Pattern`.
module Vetula.Realise
  ( chordStack
  , chordArp
  , fromChords
  ) where

import Prelude

import Tidal.Pattern.Core (cat, fastCat, stack)
import Tidal.Pattern.Types (Pattern)

-- | One chord as a BLOCK: every note sounds together, spanning the whole cycle.
chordStack :: Array Int -> Pattern Int
chordStack = stack <<< map pure

-- | One chord ARPEGGIATED: the notes spread evenly across the cycle (low→high, in
-- | the order given). `fastCat` squeezes each note into an equal slice.
chordArp :: Array Int -> Pattern Int
chordArp = fastCat <<< map pure

-- | A whole saved sequence: one chord (block) per cycle, in order, looping. `cat`
-- | plays element `cycle `mod` length` each cycle, so querying cycle `b` yields
-- | chord `b `mod` n`.
fromChords :: Array (Array Int) -> Pattern Int
fromChords = cat <<< map chordStack
