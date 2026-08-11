-- | Driving a POLYPHONIC MODULAR INSTRUMENT — one whose voices share an output
-- | and a voice-count control — from a stream of notes.
-- |
-- | `Triggerfish.Midi.Routing.Destination` enumerates wires: `ToMidi 3`,
-- | `ToEs9 8`. That works when a source maps to a fixed destination, and it
-- | cannot express this case, where a note's destination is DECIDED at play
-- | time. On a Saïch, which physical oscillator sounds a note depends on which
-- | are already busy — so the destination is the instrument, and the allocator
-- | picks the wire.
-- |
-- | The pure part of that decision lives in `Reef.Voices` (so the BEAM gets the
-- | same allocator) and the pitch correction in `Reef.Calibration` (so the
-- | browser places a note exactly where the rig would). This module is only the
-- | Effect edge: turning the allocator's emissions into Binnacle verbs.
module Triggerfish.Poly
  ( Rig
  , saichRig
  , tablesFor
  , emitAll
  ) where

import Prelude

import Data.Array (findMap, index)
import Data.Foldable (traverse_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Binnacle.Output (cvOut, cvSlew)
import Binnacle.Transport (Socket)
import Reef.Calibration (Table, realiseNote)
import Reef.Voices (Action(..), Emit, Instrument, saich)
import Triggerfish.Amphora (LibItem)

-- | Where an instrument's voices actually reach, and how to correct them.
-- |
-- | `voiceBuses` is indexed by PHYSICAL VOICE, matching `Reef.Voices`' slot
-- | numbering, so `voiceBuses !! 0` is the bus the allocator's voice 0 drives.
-- | `tables` is indexed the same way; a `Nothing` means that voice is
-- | uncalibrated and will be driven at a nominal 1 V/oct, which is honest but
-- | audibly wrong on an analogue oscillator.
type Rig =
  { inst :: Instrument
  , voiceBuses :: Array Int
  , mixBus :: Int
  , tables :: Array (Maybe Table)
  }

-- | The Saïch as patched on 2026-08-11: voices on ES-9 panel jacks 1–4, mix CV
-- | on jack 5. es9-daemon buses are jack + 7.
-- |
-- | Tables are supplied rather than baked in, because which table applies
-- | depends on the module's shared coarse setting and that is not knowable from
-- | here — see `tablesFor`.
saichRig :: Array (Maybe Table) -> Rig
saichRig tables =
  { inst: saich
  , voiceBuses: [ 8, 9, 10, 11 ]
  , mixBus: 12
  , tables
  }

-- | Pick each voice's calibration table out of an Amphora `vco-calibrations`
-- | fetch, by label.
-- |
-- | A missing label yields `Nothing` rather than a failure: an uncalibrated
-- | voice is a normal state on a rig where modules move, and refusing to play
-- | would be a worse answer than playing slightly out of tune. The caller can
-- | see which are missing and say so.
tablesFor :: Array String -> Array LibItem -> Array (Maybe Table)
tablesFor labels items = map find labels
  where
  find label = findMap (match label) items
  match label it =
    if it.name == label then Just { label, points: parsePoints it.payload }
    else Nothing

-- | Calibration payloads are the canonical JSON DeepStar writes. Parsing lives
-- | in FFI because the payload is a string of arbitrary JSON and reef's codec
-- | would need the whole table schema to read four fields of it.
foreign import parsePoints :: String -> Array { volts :: Number, hz :: Number }

-- | Send one allocator emission.
-- |
-- | Pitch goes out immediately and un-slewed: the voice it lands on is either
-- | silent (about to fade in) or holding a note that has just ended, so there
-- | is nothing to glide from and a slew would only smear the arrival.
-- |
-- | The voice count goes out as a SLEW, and that is the note-off envelope. The
-- | Saïch's mixer crossfades rather than steps — its 2→3 fade is 1.1 V wide —
-- | so travelling between plateaus over `rampMs` fades the departing voice out
-- | at that rate. A migrating note is sounding on its old voice throughout,
-- | which is what covers the move; cutting instead would leave a hole.
emitOne :: Socket -> Rig -> Number -> Emit -> Effect Unit
emitOne sock rig nowMs e = case e.action of
  Pitch voice note ->
    case index rig.voiceBuses voice of
      Nothing -> pure unit
      Just bus ->
        cvOut sock { bus, value: normalise (voltsFor rig voice note) }
  Mix _ volts ->
    let lagSec = max 0.0 (e.atMs - nowMs) / 1000.0
    in cvSlew sock { bus: rig.mixBus, value: normalise volts, lagSec }

-- | Send a batch in order. Order is load-bearing: `Reef.Voices` puts every
-- | pitch change before the voice-count change that fades a voice out, so that
-- | a migrating note is covered by its own old voice rather than by a hole.
emitAll :: Socket -> Rig -> Number -> Array Emit -> Effect Unit
emitAll sock rig nowMs = traverse_ (emitOne sock rig nowMs)

-- | MIDI note → volts for a specific physical voice, through that voice's own
-- | measured curve. This is the whole reason the browser needs a realiser: the
-- | four Saïch voices differ by up to 16 cents at the same voltage, so a note
-- | migrating between them would audibly shift pitch without it.
voltsFor :: Rig -> Int -> Int -> Number
voltsFor rig voice note =
  case fromMaybe Nothing (index rig.tables voice) of
    Just t -> realiseNote t (toNumber note)
    Nothing -> nominalVolts note

-- | es9-daemon takes −1.0..+1.0 as ±10 V.
normalise :: Number -> Number
normalise volts = volts / 10.0

-- | An uncalibrated voice: a straight 1 V/oct from C2 = 0 V, matching what the
-- | sweeps assume as their base. Wrong by whatever the oscillator's tracking
-- | error is, and knowingly so.
nominalVolts :: Int -> Number
nominalVolts note = (toNumber note - 36.0) / 12.0

