-- | `Triggerfish.Routing.Out` — the emit side of the routing table: resolve a
-- | source's legs against the MIDI ports actually present, and fan one musical
-- | event out to all of them.
-- |
-- | This is the module that makes the table load-bearing rather than decorative.
-- | Before it, each machine opened its own port by a hardcoded name and sent to
-- | one place; now every machine asks the table where its output goes, and the
-- | answer is data the player can edit.
-- |
-- | ## Why every port is opened, not two
-- |
-- | With a fixed routing you can open the ports you know you need. With an
-- | editable one you cannot — the table may name any port — so `openAll` takes a
-- | handle on every output and `outFor` resolves by the same substring rule
-- | `Binnacle.Midi.findOutput` uses. That also means adding a destination in the
-- | UI works immediately, with no MIDI re-initialisation.
-- |
-- | ## Offsets are applied here
-- |
-- | Per-leg `offsetMs` is added to the scheduled time at exactly this point, so
-- | there is one place where "these two kicks are meant to be simultaneous"
-- | becomes "and here is the trim that makes them sound simultaneous". See
-- | `Model.Leg` for why that is not optional.
module Triggerfish.Routing.Out
  ( Outs
  , openAll
  , outFor
  , portNames
  , ResolvedLeg
  , resolveLegs
  , fanNote
  , fanNoteAt
  ) where

import Prelude

import Data.Array (find, mapMaybe)
import Data.Foldable (sum)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), contains)
import Data.Traversable (traverse)
import Effect (Effect)

import Binnacle.Midi as Midi
import Reef.Rample as Rample
import Triggerfish.Routing.Model (Leg, Source, Table, Wire, liveLegsFor, wireOf)

-- | Every MIDI output port, by name. Built once when MIDI access arrives.
type Outs = Array { name :: String, out :: Midi.MidiOut }

-- | Open a handle on every output port. `findOutput` matches on a substring, and
-- | a full port name is a substring of itself, so looking each name up by itself
-- | is exact (and avoids widening the FFI to enumerate handles).
openAll :: Midi.MidiAccess -> Effect Outs
openAll access = do
  names <- Midi.outputNames access
  rows <- traverse (\n -> map (map { name: n, out: _ }) (Midi.findOutput access n)) names
  pure (mapMaybe identity rows)

portNames :: Outs -> Array String
portNames = map _.name

-- | Resolve a port needle to a handle, by the SAME substring rule the emit path
-- | has always used — so the router's idea of "found" cannot drift from what
-- | actually receives notes. First match wins, as `findOutput` does.
outFor :: Outs -> String -> Maybe Midi.MidiOut
outFor outs needle = map _.out (find (\r -> contains (Pattern needle) r.name) outs)

-- | A leg matched against the world: either it can emit, or it cannot and we
-- | know which leg and why.
type ResolvedLeg =
  { leg :: Leg
  , wire :: Maybe Wire          -- Nothing = not browser-emittable (the ES-9 kinds)
  , out :: Maybe Midi.MidiOut   -- Nothing = the named port is absent
  }

resolveLegs :: Outs -> Table -> Source -> Array ResolvedLeg
resolveLegs outs tbl src = map res (liveLegsFor tbl src)
  where
  res leg =
    let w = wireOf leg.dest
    in { leg, wire: w, out: w >>= \x -> outFor outs x.port }

-- | Fan one note out to every live, reachable leg of a source, at an ABSOLUTE
-- | performance-clock time. Returns how many legs emitted.
-- |
-- | `note` is the source's own pitch; a leg whose destination selects BY note (an
-- | FH-2 gate MCV) overrides it — which is why the override lives in `Wire`
-- | rather than every caller special-casing gates.
fanNoteAt
  :: Outs
  -> Table
  -> Source
  -> { note :: Int, velocity :: Int, atMs :: Number, durMs :: Number }
  -> Effect Int
fanNoteAt outs tbl src ev =
  map sum (traverse send (resolveLegs outs tbl src))
  where
  send r = case r.wire, r.out of
    Just w, Just o -> do
      let atMs = ev.atMs + r.leg.offsetMs
      case w.rample of
        -- The ordinary case: the pitch IS the note.
        Nothing -> do
          Midi.scheduleNoteAtMs o
            { channel: w.channel - 1
            , note: fromMaybe ev.note w.noteOverride
            , velocity: ev.velocity
            , atMs
            , durMs: ev.durMs
            }
          pure 1
        -- The Rample case: the pitch is a slice, so it goes ahead of the note
        -- as a CC and the note is only the trigger. A pitch the card does not
        -- hold is REFUSED rather than clamped — a silently transposed note is
        -- harder to notice than a missing one, and the leg reports 0 emitted
        -- so the monitor can say so.
        Just rp ->
          -- `Reef.Rample` in the browser and `Reef.Rample` on the BEAM are the
          -- same module, so the two runtimes cannot disagree about which slice
          -- a G4 is. A chromatic run is the `pitchOfSlot0` case; a card laid
          -- out in some other order (a kalimba in tine order) would supply
          -- `slotPitches` instead, which is why this asks reef rather than
          -- subtracting here.
          case Rample.slotFor
                 { velocity: Nothing
                 , slots: rp.slots
                 , pitchOfSlot0: Just rp.pitchOfSlot0
                 , slotPitches: Nothing
                 } ev.note of
            Nothing -> pure 0
            Just slot -> do
              Midi.sendCCAtMs o
                { channel: w.channel - 1
                , controller: Rample.startCC rp.voice
                , value: Rample.ccForSlot slot rp.slots
                , atMs: atMs - Int.toNumber rp.settleMs
                }
              Midi.scheduleNoteAtMs o
                { channel: w.channel - 1
                , note: rp.trigger
                , velocity: ev.velocity
                , atMs
                , durMs: ev.durMs
                }
              pure 1
    _, _ -> pure 0

-- | Delay-relative variant, for the emit paths that think in "ms from now".
fanNote
  :: Outs
  -> Table
  -> Source
  -> { note :: Int, velocity :: Int, delayMs :: Number, durMs :: Number }
  -> Effect Int
fanNote outs tbl src ev =
  map sum (traverse send (resolveLegs outs tbl src))
  where
  send r = case r.wire, r.out of
    Just w, Just o -> do
      Midi.scheduleNote o
        { channel: w.channel - 1
        , note: fromMaybe ev.note w.noteOverride
        , velocity: ev.velocity
        , delayMs: ev.delayMs + r.leg.offsetMs
        , durMs: ev.durMs
        }
      pure 1
    _, _ -> pure 0
