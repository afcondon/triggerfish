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
  , drumRouting
  , voiceRouting
  , vetulaRouting
  , drumsOrbit
  , auditionLine
  , sendAll
  ) where

import Prelude

import Data.Array (filter, find, mapMaybe, mapWithIndex, range)
import Data.Foldable (sum)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..), contains)
import Data.Traversable (traverse)
import Effect (Effect)

import Binnacle.Midi as Midi
import Binnacle.Time (perfNow)
import Reef.Rample as Rample
import Reef.Routing as RR
import Simple.JSON (writeJSON)
import Triggerfish.Routing.Model (Destination(..), Leg, Source(..), Table, Wire, cardLegs, liveLegsFor, wireOf)

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

-- | The drum lanes as `Reef.Routing` carries them, for the rig and for the
-- | browser's own drum emit alike: every live leg that is a MIDI wire on a port
-- | that exists, by its whole name. The needle is resolved HERE, by the same
-- | substring rule as `outFor`, because the rig matches names exactly: pushing
-- | "IAC" would reach nothing there.
-- |
-- | A MIDI leg the browser cannot emit (the ES-9 kinds) or whose port is
-- | absent is left out, so the rig plays exactly what the browser would. A
-- | sample leg goes in as a voice: only the rig plays those, in Rig mode.
drumRouting :: Outs -> Table -> Array Int -> RR.DrumRouting
drumRouting outs tbl notes =
  { notes
  , lanes: mapWithIndex (\lane _ -> mapMaybe resolve (legsOf lane)) notes
  , voices: mapWithIndex (\lane _ -> mapMaybe voice (legsOf lane)) notes
  }
  where
  legsOf lane = liveLegsFor tbl (SDrumLane lane)
  voice leg = case leg.dest of
    DSample d -> Just
      { s: d.set, n: d.n
      , begin: Int.toNumber d.begin / 100.0, end: Int.toNumber d.end / 100.0
      , speed: if d.reverse then -1.0 else 1.0
      , gain: Int.toNumber d.gain / 100.0
      , orbit: drumsOrbit, chop: d.chop, offsetMs: leg.offsetMs
      }
    _ -> Nothing
  resolve = resolveLeg outs

-- | One table leg as `Reef.Routing` carries it: a MIDI wire on a port that
-- | exists, by its whole name; nothing for a kind the browser cannot emit or a
-- | port that is absent.
resolveLeg :: Outs -> Leg -> Maybe RR.Leg
resolveLeg outs = resolveLegIn (map _.name outs)

-- | `resolveLeg` given only the ports' names, as a page without MIDI handles
-- | (the dashboard) knows them.
resolveLegIn :: Array String -> Leg -> Maybe RR.Leg
resolveLegIn names leg = do
  w <- wireOf leg.dest
  found <- find (contains (Pattern w.port)) names
  pure
    { port: found
    , channel: w.channel
    , note: fromMaybe (-1) w.noteOverride
    , offsetMs: leg.offsetMs
    , rample: maybe [] (\r -> [ { voice: r.voice, slots: r.slots, pitchOfSlot0: r.pitchOfSlot0, settleMs: r.settleMs } ]) w.rample
    }

-- | A melodic machine's voices (Odonus's heads, in order) as the rig is told
-- | to play them: each voice's live MIDI legs, resolved as the drum lanes' are.
-- | The kinds that allocate across voices (a poly instrument, a Rample played
-- | polyphonically) are the page's to play and are left out.
voiceRouting :: Outs -> Table -> Array Source -> RR.VoiceRouting
voiceRouting outs tbl sources = { voices: map (\src -> mapMaybe (resolveLeg outs) (liveLegsFor tbl src)) sources }

-- | Vetula's sixteen channels as the rig's card player plays them: voice
-- | `ch - 1` is the card on channel `ch`, down its row's live legs (or its
-- | channel on the default port, with no row).
vetulaRouting :: Array String -> Table -> RR.VoiceRouting
vetulaRouting names tbl =
  { voices: map (\ch -> mapMaybe (resolveLegIn names) (filter _.on (cardLegs names tbl ch))) (range 1 16) }

-- | The SuperDirt orbit drum voices play on: an effects chain of their own,
-- | apart from Conspicillum's 0 and its sends 10 and 11, on the main outputs.
drumsOrbit :: Int
drumsOrbit = 1

-- | The rig line that plays a sample destination once, now, as it is set: the
-- | router's ▶. Other destinations have nothing to audition this way.
auditionLine :: Destination -> Maybe String
auditionLine = case _ of
  DSample d -> Just $ "dirt-play " <> writeJSON
    { s: d.set, n: d.n
    , begin: Int.toNumber d.begin / 100.0, end: Int.toNumber d.end / 100.0
    , speed: if d.reverse then -1.0 else 1.0, gain: Int.toNumber d.gain / 100.0
    , orbit: drumsOrbit
    }
  _ -> Nothing

-- | Send what `Reef.Routing` decided, each `atMs` from now. Returns how many
-- | notes went out. A `Play` is the rig's to send; the browser has no OSC.
sendAll :: Outs -> Array RR.Send -> Effect Int
sendAll outs sends = do
  now <- perfNow
  map sum (traverse (one now) sends)
  where
  byName name = map _.out (find (\r -> r.name == name) outs)
  one now = case _ of
    RR.Note n -> case byName n.port of
      Nothing -> pure 0
      Just o -> do
        Midi.scheduleNoteAtMs o
          { channel: n.channel - 1, note: n.note, velocity: n.velocity, atMs: now + n.atMs, durMs: n.durMs }
        pure 1
    RR.Control c -> case byName c.port of
      Nothing -> pure 0
      Just o -> do
        Midi.sendCCAtMs o { channel: c.channel - 1, controller: c.controller, value: c.value, atMs: now + c.atMs }
        pure 0
    RR.Play _ -> pure 0
