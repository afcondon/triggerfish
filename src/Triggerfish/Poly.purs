-- | Driving a POLYPHONIC MODULAR INSTRUMENT — one that sounds several notes
-- | through a single set of jacks — from a stream of notes.
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
  , ringsRig
  , tablesFor
  , withOrder
  , emitAll
  ) where

import Prelude

import Data.Array (findMap, index)
import Data.Foldable (traverse_)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Timer (setTimeout)
import Binnacle.Output (cvOut, cvSlew, fireAt)
import Binnacle.Transport (Socket)
import Reef.Calibration (Table, realiseNote)
import Reef.Voices (Action(..), Emit, Instrument, Silencing(..), rings, saich)
import Reef.Voices as RV
import Triggerfish.Amphora (LibItem)
import Triggerfish.Routing.Model (InstrumentId(..), polyJacks)

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
  , gateBuses :: Array Int
  -- ^ Per-voice gate buses, same indexing. Required for a `PerVoiceGate`
  -- instrument and empty for any other, since `Reef.Voices` only emits `Gate`
  -- for that capability. A `PerVoiceGate` rig with no gate buses is a
  -- configuration error, not a runtime one — see `saichRig` for the shape.
  , ctrlBus :: Int
  -- ^ The one jack that makes a note audible, whatever the instrument means by
  -- that: the Saïch's voice-count CV, Rings' STRUM. Which it is follows from
  -- `inst.silencing`, so this stays a single field rather than a sum whose
  -- constructor would have to be kept in step with it.
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
  let js = polyJacks Saich
  in
    { inst: saich
    , voiceBuses: js.voiceBuses
    -- The Saich silences by voice count, not by gate, so it has none. Its
    -- oscillators cannot be gated at all — that is why the mix CV exists.
    , gateBuses: []
    , ctrlBus: js.ctrlBus
    , tables
    }

-- | Rings in polyphonic mode: ONE pitch jack and one STRUM, because it holds its
-- | own voices and we never address them.
-- |
-- | One table, not four, and that is the whole difference calibration makes
-- | here — the Saïch's four oscillators disagree with EACH OTHER by 16 cents, so
-- | a migrating note shifts pitch without per-voice correction. Rings has no
-- | such spread to correct, only its own CV input's error, which every note
-- | shares.
ringsRig :: Array (Maybe Table) -> Rig
ringsRig tables =
  let js = polyJacks Rings
  in
    { inst: rings
    , voiceBuses: js.voiceBuses
    , gateBuses: []
    , ctrlBus: js.ctrlBus
    , tables
    }

-- | Set how voices are seated. The allocator is shared by every route into an
-- | instrument, so this is a property of the instrument in use rather than of
-- | one route — the caller reconciles the routes and states one answer.
-- |
-- | Ignored by an instrument that allocates for itself: it has one bus and its
-- | voices are not addressable from here, so there is no seating to order.
-- | Silently, because the request comes from a saved routing rather than from a
-- | control anyone can see — the picker never offers it — and dropping a leg
-- | over it would be a worse answer than playing the notes.
withOrder :: RV.Order -> Rig -> Rig
withOrder o rig = case rig.inst.silencing of
  SelfAllocating _ -> rig
  _ -> rig { inst = rig.inst { order = o } }

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
        deferBy (e.atMs - nowMs) $
          cvOut sock { bus, value: normalise (voltsFor rig voice note) }
  Gate voice on ->
    case index rig.gateBuses voice of
      Nothing -> pure unit
      Just bus -> cvOut sock { bus, value: normalise (if on then gateVolts else 0.0) }
  Mix _ volts ->
    let lagSec = max 0.0 (e.atMs - nowMs) / 1000.0
    in cvSlew sock { bus: rig.ctrlBus, value: normalise volts, lagSec }
  Trigger _ durMs -> case rig.inst.silencing of
    -- Deferred to its PITCH's moment, not its own, and given the settle as
    -- `fire-at`'s delay. That is what keeps the guarantee: the browser's timer
    -- can be several milliseconds late, and if the trigger were scheduled
    -- sample-accurately while its pitch waited on `setTimeout`, a late pitch
    -- would let the trigger sample the PREVIOUS note. Sending both in one tick
    -- and letting es9-daemon hold the gap makes the jitter common to the pair.
    --
    -- Same delay as the pitch's `setTimeout`, registered after it, so it goes
    -- second — JS fires equal deadlines in registration order.
    SelfAllocating sa ->
      deferBy (e.atMs - sa.settleMs - nowMs) $
        fireAt sock
          { bus: rig.ctrlBus
          , value: normalise gateVolts
          , durMs
          , delayMs: sa.settleMs
          }
    _ -> pure unit
  -- PRE-EXISTING GAP, made explicit rather than left to a catch-all: `Rig` has
  -- no decay bus, so the browser has nowhere to put this. `Reef.Voices` emits
  -- `Decay` only for a profile carrying a `DecayMap` (today: Rings), so a Rings
  -- note played from the browser is currently un-shaped where the same note
  -- from the BEAM is shaped. Giving `Rig` a `decayBus :: Maybe Int` is the fix;
  -- it is a routing change, not this one.
  Decay _ _ -> pure unit

-- | Run an effect `ms` from now, or immediately if that is already past.
-- |
-- | A millisecond of slop counts as now: `setTimeout 0` still costs a turn of
-- | the event loop, and nothing here is improved by taking one.
deferBy :: Number -> Effect Unit -> Effect Unit
deferBy ms act
  | ms <= 1.0 = act
  | otherwise = void (setTimeout (round ms) act)

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

-- | Eurorack gates are nominally +5 V; anything above about 2 V reads high on
-- | every module here, so 5 is the safe convention rather than a measured value.
gateVolts :: Number
gateVolts = 5.0

-- | es9-daemon takes −1.0..+1.0 as ±10 V.
normalise :: Number -> Number
normalise volts = volts / 10.0

-- | An uncalibrated voice: a straight 1 V/oct from C2 = 0 V, matching what the
-- | sweeps assume as their base. Wrong by whatever the oscillator's tracking
-- | error is, and knowingly so.
nominalVolts :: Int -> Number
nominalVolts note = (toNumber note - 36.0) / 12.0

