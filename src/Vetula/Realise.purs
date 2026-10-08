-- | `Vetula.Realise` — the FRONTEND chord→`Pattern` realiser for the Perform view.
-- |
-- | The Perform boxes carry a saved sequence (a list of chords, each an `Array Int`
-- | of MIDI notes) and realise it through Littorina's Tidal engine
-- | (`Tidal.Pattern.Core`). Building the carrier as a real `Pattern Int` — rather
-- | than the ad-hoc beat-fire the first Perform slice used — is what lets the
-- | function-stack layers (`fast`/`ply`/`arp`/`every`/…) compose on it directly:
-- | every layer is a `Pattern a -> Pattern a`, the box's realiser is
-- | `foldl applyFx (fromChords …) stack`, and playback just QUERIES the resulting
-- | pattern per cycle and schedules the events (see `App.purs` PerfTick).
-- |
-- | This mirrors the pure `voicingAs*` helpers that live BEAM-side in
-- | `architeuthis/src/Tidal/Vetula/Pattern.purs`, ported to the frontend over raw
-- | `Array Int` note-sets (no `PitchedNote12`/`Voicing` dependency needed). The
-- | value carried is the MIDI note number itself.
-- |
-- | Convention here: **one pattern cycle = one Perform beat** (a chord's slot).
-- |
-- | The carrier VALUE is a CHORD — `Pattern (Array Int)`, an `Array Int` of MIDI
-- | notes — not an individual note. This is deliberate: the pitch-shaping layers
-- | (voice / select / transpose) act on the chord *as a group*, so the grouping
-- | must survive down the stack. The chord→time explosion (block / arp / strum) is
-- | the terminal REALISATION step done at schedule time, not a stack layer — so
-- | every stack layer stays a uniform `Pattern (Array Int) -> Pattern (Array Int)`
-- | and drags anywhere.
module Vetula.Realise
  ( fromChords
  ) where

import Prelude

import Tidal.Pattern.Core (cat)
import Tidal.Pattern.Types (Pattern)

-- | A whole saved sequence: one chord per cycle, in order, looping. `cat` plays
-- | element `cycle `mod` length` each cycle, so querying cycle `b` yields chord
-- | `b `mod` n` as a single event carrying that chord's note-set.
fromChords :: Array (Array Int) -> Pattern (Array Int)
fromChords = cat <<< map pure
