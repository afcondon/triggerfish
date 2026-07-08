-- | Triggerfish.Midi.Routing — the single source of truth for MIDI output
-- | channel assignment across every instrument, and the one place the canonical
-- | 1..16 channel numbering converts to WebMIDI's 0..15.
-- |
-- | The model is identity ≠ destination (see docs/PLAN-midi-routing.md): a source
-- | has an identity; a routing table binds identity → destinations; instrument
-- | pages no longer carry channel numbers of their own. The default table below
-- | IS the standard Ableton project template.
-- |
-- | CONVENTION: channels are 1..16 as the hardware, Ableton, and link-spike all
-- | see them. link-spike's /midi/note/at takes 1..16 and subtracts one itself.
-- | The browser's Binnacle.Midi masks (channel & 0x0f) expecting 0..15, so we
-- | convert with `toWire` at exactly one boundary — the WebMIDI emit — and
-- | nowhere else subtracts 1.
module Triggerfish.Midi.Routing
  ( Destination(..)
  , SourceId(..)
  , odonusHeadChannel
  , vetulaDefaultChannel
  , drumsChannel
  , toWire
  , defaultRouting
  ) where

import Prelude
import Data.Maybe (Maybe(..))

-- | A heterogeneous output destination. Only `ToMidi` is honoured by the browser
-- | today; the modular kinds are rig-reachable and browser-pending a WS→OSC
-- | bridge (see the plan's open question on fan-out).
data Destination
  = ToMidi Int      -- canonical MIDI channel 1..16
  | ToEs9 Int       -- ES-9 CV/gate bus (1-indexed)
  | ToFh2 Int       -- FH-2 output (1-indexed)
  | ToOsc String    -- named OSC target (Stellatus / Sufflamen)

derive instance eqDestination :: Eq Destination

-- | What identifies a MIDI-emitting source in the routing table.
data SourceId
  = OdonusHead Int              -- head 0..3, fixed to channels 1..4
  | VetulaVoice (Maybe String)  -- Nothing = the default voice; Just = a named voice
  | Drums                       -- Balistes, all three drum tabs

derive instance eqSourceId :: Eq SourceId

-- | Odonus heads are fixed: head 0..3 → channels 1..4.
odonusHeadChannel :: Int -> Int
odonusHeadChannel h = 1 + h

-- | Vetula's default (unnamed) MIDI voice.
vetulaDefaultChannel :: Int
vetulaDefaultChannel = 5

-- | GM drums (Balistes — all three drum tabs share this).
drumsChannel :: Int
drumsChannel = 10

-- | The ONE browser-side conversion: canonical 1..16 → WebMIDI 0..15.
toWire :: Int -> Int
toWire ch = ch - 1

-- | The default routing table == the standard Ableton project template.
-- | (Named Vetula voices climb 6..8 as the config page assigns them.)
defaultRouting :: Array { source :: SourceId, dests :: Array Destination }
defaultRouting =
  [ { source: OdonusHead 0, dests: [ ToMidi 1 ] }
  , { source: OdonusHead 1, dests: [ ToMidi 2 ] }
  , { source: OdonusHead 2, dests: [ ToMidi 3 ] }
  , { source: OdonusHead 3, dests: [ ToMidi 4 ] }
  , { source: VetulaVoice Nothing, dests: [ ToMidi vetulaDefaultChannel ] }
  , { source: Drums, dests: [ ToMidi drumsChannel ] }
  ]
