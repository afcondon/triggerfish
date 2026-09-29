-- | `Triggerfish.Routing.Model` — the unified routing table: which sources fan
-- | out to which destinations, on what device, with what offset.
-- |
-- | The model, and the whole point of the module:
-- |
-- |   **a source fans out to a SET of destinations, and a destination's shape
-- |   depends on its device.**
-- |
-- | See `docs/DESIGN-routing.md`. Everything here is placement — rig-facing fact
-- | about where sound comes out — and deliberately NOT part of any machine's
-- | musical model. `Reef.Odonus.Head` and the Balistes brains co-simulate with the
-- | BEAM and carry musical facts only; a saved pattern must not drag a patch
-- | cable along with it.
-- |
-- | ## Why one table rather than per-machine constants
-- |
-- | Before this, placement lived in: `Odonus.Grid`'s `midiPortName`/`envPortName`,
-- | `Balistes.Types.drumChannel`, `Midi.Routing`'s three `*Channel` functions, the
-- | shell's `routing`/`audition` maps, Selene's doc text, and a hardcoded `KIT`
-- | table in *another repo* (`fh2-config/scripts/apply-drum-breakout.mjs`). Each
-- | was locally sensible; together they meant no surface could answer "where does
-- | this go", and two of them were already wrong in ways nothing could see.
-- |
-- | ## Granularity: this table is per-JACK, Selene's targets are per-BLOCK
-- |
-- | `Triggerfish.Rig.Target` is block-granular — `FH2 0` is a bank of eight, and
-- | that is right for Selene, whose polysignals fill a whole 8-wide block at once.
-- | A drum lane wants ONE gate and an Odonus head wants ONE envelope, so the
-- | destinations here address individual jacks. The two coexist rather than one
-- | replacing the other: a Selene block claim and a drum's jack claim contend for
-- | the same hardware, which is exactly what `claims` below is for.
module Triggerfish.Routing.Model
  ( Source(..)
  , sourceLabel
  , sourceKey
  , Destination(..)
  , InstrumentId(..)
  , instrumentLabel
  , polyJacks
  , destLabel
  , destShortLabel
  , destDevice
  , Device(..)
  , deviceLabel
  , Leg
  , Route
  , Table
  , defaultTable
  , defaultTableFor
  , defaultPort
  , legsFor
  , liveLegsFor
  , setLegs
  , modifyLeg
  , removeLeg
  , addLeg
  , setDestField
  , Reach(..)
  , Ports
  , reachOf
  , reachNote
  , unreachable
  , Claim
  , claims
  , conflicts
  , Wire
  , RampleWire
  , wireOf
  , outputsOf
  , carriesLine
  , fh2Port
  , fh2GateChannel
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (concatMap, filter, find, findIndex, head, length, mapMaybe, mapWithIndex, nub, snoc, updateAt, (!!))
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Tuple (Tuple(..), snd)
import Data.String (Pattern(..), contains)
import Data.String.Common (joinWith)

import Triggerfish.Selene.Layout (Output, outputKey)

-- ---------------------------------------------------------------------------
-- Sources
-- ---------------------------------------------------------------------------

-- | What is emitting. One constructor per thing a player would think of as
-- | "a voice you could point somewhere".
-- |
-- | `SDrumLane` is indexed by `Balistes.Pattern.canonKit` position, NOT by brain.
-- | All three brains (Grids, Rytm, the Tidal rack) are already laid against that
-- | same 16-lane kit, so routing is a property of the LANE and survives a brain
-- | switch — which is what you want, since the patch cable didn't move when you
-- | changed how the kick is being decided. It also means Grids' three lanes are
-- | not a different case: it simply lights three of the sixteen rows.
data Source
  = SOdonusHead Int      -- 0..3
  | SDrumLane Int        -- canonKit index 0..15
  | SVetulaVoice String  -- "" = the default (unnamed) voice
  | SSeleneBank String   -- a Selene destination, by its stable alias

derive instance eqSource :: Eq Source

sourceLabel :: Source -> String
sourceLabel = case _ of
  SOdonusHead h -> "Odonus " <> fromMaybe (show (h + 1)) (romans !! h)
  SDrumLane i -> "Drums " <> show i
  SVetulaVoice "" -> "Vetula (default)"
  SVetulaVoice nm -> "Vetula · " <> nm
  SSeleneBank a -> "Selene · " <> a
  where
  romans = [ "I", "II", "III", "IV" ]

-- | A stable string key. Used by the store, so its spelling is a wire format:
-- | changing one orphans that row's saved routing (harmless — it falls back to
-- | the default — but it silently loses a user's edit, so don't).
sourceKey :: Source -> String
sourceKey = case _ of
  SOdonusHead h -> "odonus.head." <> show h
  SDrumLane i -> "drums.lane." <> show i
  SVetulaVoice nm -> "vetula.voice." <> nm
  SSeleneBank a -> "selene.bank." <> a

-- ---------------------------------------------------------------------------
-- Destinations
-- ---------------------------------------------------------------------------

-- | Where a source's output goes. The shape differs per device because the
-- | devices genuinely differ — flattening these into "a channel number" is what
-- | made the FH-2 envelope bug possible (same channel NUMBER, different device,
-- | completely different meaning).
data Destination
  -- | A MIDI note on a port, canonical channel 1..16. `port` is a substring
  -- | matched against WebMIDI's port names, as `Binnacle.Midi.findOutput` does.
  = DMidi { port :: String, channel :: Int }
  -- | An FH-2 polyenv envelope, 1..8. Addressed by MIDI channel on the FH-2's own
  -- | port — envelope N listens on channel N — so firing it is sending the note
  -- | there. The channel is NOT free: polyenv owns 1..8 by construction, which is
  -- | why this is its own constructor rather than a `DMidi` with a channel.
  | DFh2Env { slot :: Int }
  -- | An FH-2 note-filtered trigger MCV driving one FHX-8GT jack. `note` is a
  -- | SELECTOR the MCV matches on, not a pitch; `jack` is where the gate appears.
  -- | The two are only correct relative to each other, which is the argument for
  -- | one table owning both.
  | DFh2Gate { note :: Int, jack :: Int }
  -- | An ES-9 gate, on gate block `block` (0-based), jack 1..8.
  | DEs9Gate { block :: Int, jack :: Int }
  -- | An ES-9 CV bus (1-indexed), e.g. a calibrated pitch route.
  | DEs9Cv { bus :: Int }
  -- | The `continuo` MIDI port, kept separate from `DMidi` because it is a fixed
  -- | rig fixture rather than a port you pick.
  | DContinuo { channel :: Int }
  -- | A Squarp Rample voice playing a SLICED card, where a pitch is not a note.
  -- |
  -- | Every other MIDI destination here sends the pitch it is handed. This one
  -- | cannot. A note reaching a Rample selects a voice and a layer, never a
  -- | pitch, so a melody sent to it as notes arrives as rhythm with the tune
  -- | thrown away. Pitch on this module is the START POINT — which slice of a
  -- | concatenated sample to play — so one event becomes two messages in a
  -- | fixed order: `CC(voice * 10 + 4)` naming the slice, then the trigger note
  -- | `settleMs` later. That order is load-bearing. A trigger arriving first
  -- | plays the PREVIOUS slice, which sounds like a wrong note rather than like
  -- | a fault, and so is the kind of bug you chase in the music instead of in
  -- | the code.
  -- |
  -- | `slots` and `pitchOfSlot0` are facts about the CARD, not about the
  -- | module: they come from the `_msm/index.json` that `msm kit build` writes
  -- | when it compiles one. The defaults describe the piano at bank P0
  -- | (SLICER /64, slice 0 = C2 = 36).
  -- |
  -- | Four voices are four legs on ONE port and ONE channel differing only in
  -- | `voice` — NOT four channels, which is the natural guess and is wrong.
  | DRample
      { port :: String
      , channel :: Int
      , voice :: Int           -- 1..4
      , trigger :: Int         -- the SP note that fires this voice
      , slots :: Int           -- the card's SLICER division
      , pitchOfSlot0 :: Int    -- MIDI note of slice 0
      , settleMs :: Int        -- how far AHEAD of the note the CC must land
      }
  -- | The WHOLE Rample as one polyphonic instrument, its four voices allocated
  -- | at play time.
  -- |
  -- | `DRample` names a wire — this note always fires that voice. This cannot be
  -- | said that way, for the same reason `DPoly` cannot: which voice sounds a
  -- | note depends on which are already ringing. A Rample voice is monophonic
  -- | and a struck slice rings until something takes the voice back, so ONE
  -- | voice playing a sustaining instrument cuts every note with the next.
  -- | Four interchangeable voices ARE the polyphony.
  -- |
  -- | `Reef.Voices.rample` holds the decision (round-robin, steal the
  -- | longest-idle, leave holes) and its measured 40 ms settle; the triggers are
  -- | the card's `SETTINGS > SPx` notes in voice order.
  | DRamplePoly
      { port :: String
      , channel :: Int
      , triggers :: Array Int   -- SPx note per voice, voice order
      , slots :: Int
      , pitchOfSlot0 :: Int
      }
  -- | A POLYPHONIC INSTRUMENT, whose voices are allocated at play time.
  -- |
  -- | Every other constructor here names a WIRE — this note always goes to that
  -- | channel, that bus, that jack. A shared-output instrument cannot be said
  -- | that way: on a Saïch, which of four oscillators sounds a note depends on
  -- | which are already busy, so the destination is the instrument and the
  -- | allocator picks the wire. `Reef.Voices` holds that decision; the jacks it
  -- | picks between are `polyJacks`.
  -- |
  -- | `DEs9Cv` is the degenerate case of this — a one-voice instrument with no
  -- | way to silence itself is exactly a plain CV bus — and the two are worth
  -- | collapsing eventually, but not in the change that introduces this one.
  | DPoly { inst :: InstrumentId, sortByPitch :: Boolean }
  -- | A SAMPLE, played by SuperDirt on the rig: sample `n` of the Quadrat set
  -- | `set` (a set's directory is a SuperDirt bank), its splice window as
  -- | percentages, reverse, gain in percent, and `chop`, which plays the window
  -- | as that many slices across the step. Rig mode only: a browser cannot send
  -- | OSC, so in Local mode the lane plays its other legs and not this one.
  -- | Integers throughout, because the editor and the store deal in them.
  | DSample
      { set :: String
      , n :: Int
      , begin :: Int
      , end :: Int
      , reverse :: Boolean
      , gain :: Int
      , chop :: Int
      }

derive instance eqDestination :: Eq Destination

-- | A polyphonic instrument the rig knows how to drive. An ADT rather than a
-- | string so a typo is a build error instead of a route that silently never
-- | sounds.
data InstrumentId = Saich | Rings

derive instance eqInstrumentId :: Eq InstrumentId

instrumentLabel :: InstrumentId -> String
instrumentLabel = case _ of
  Saich -> "Saïch"
  Rings -> "Rings"

-- | Which es9-daemon buses an instrument occupies: one per voice it exposes,
-- | plus the single control jack that makes a note audible.
-- |
-- | Both shapes here are "n pitch jacks and one control jack", but the control
-- | jack means opposite things — the Saïch's says how many voices reach the
-- | output, Rings' says take the pitch on the bus NOW — so `ctrlLabel` carries
-- | which, for anything that has to name it.
-- |
-- | Lives here rather than beside the driving code because it is ROUTING — what
-- | is patched where — and because the claims report has to know an instrument
-- | speaks for five jacks, not one.
polyJacks :: InstrumentId -> { voiceBuses :: Array Int, ctrlBus :: Int, ctrlLabel :: String }
polyJacks = case _ of
  -- Patched 2026-08-11: voices on ES-9 panel jacks 1-4, mix CV on jack 5.
  -- es9-daemon bus = panel jack + 7.
  Saich -> { voiceBuses: [ 8, 9, 10, 11 ], ctrlBus: 12, ctrlLabel: "mix" }
  -- Rings allocates internally, so however many voices it holds it presents ONE
  -- pitch input — panel jack 6 — and one STRUM, jack 7.
  Rings -> { voiceBuses: [ 13 ], ctrlBus: 14, ctrlLabel: "strum" }

-- | The physical thing a destination lands on. Capacity is per-device, so this
-- | is what `claims` groups by.
data Device = DevMidi String | DevFh2 | DevEs9 | DevContinuo | DevSuperDirt

derive instance eqDevice :: Eq Device

deviceLabel :: Device -> String
deviceLabel = case _ of
  DevMidi p -> p
  DevFh2 -> "FH-2"
  DevEs9 -> "ES-9"
  DevContinuo -> "continuo"
  DevSuperDirt -> "SuperDirt"

destDevice :: Destination -> Device
destDevice = case _ of
  DMidi d -> DevMidi d.port
  DFh2Env _ -> DevFh2
  DFh2Gate _ -> DevFh2
  DEs9Gate _ -> DevEs9
  DEs9Cv _ -> DevEs9
  DContinuo _ -> DevContinuo
  DRample d -> DevMidi d.port
  DRamplePoly d -> DevMidi d.port
  DPoly _ -> DevEs9
  DSample _ -> DevSuperDirt

destLabel :: Destination -> String
destLabel = case _ of
  DMidi d -> d.port <> " ch " <> show d.channel
  DFh2Env d -> "FH-2 envelope " <> show d.slot
  DFh2Gate d -> "FH-2 gate → FHX-8GT jack " <> show d.jack <> " (note " <> show d.note <> ")"
  DEs9Gate d -> "ES-9 GT " <> show d.block <> " jack " <> show d.jack
  DEs9Cv d -> "ES-9 CV bus " <> show d.bus
  DPoly d ->
    let js = polyJacks d.inst
    in instrumentLabel d.inst
         <> (if d.sortByPitch then " · lowest note on voice 1" else "")
         <> " (" <> show (length js.voiceBuses)
         <> " voices, ES-9 buses " <> joinWith "/" (map show js.voiceBuses)
         <> ", " <> js.ctrlLabel <> " " <> show js.ctrlBus <> ")"
  DContinuo d -> "continuo ch " <> show d.channel
  DRample d ->
    d.port <> " ch " <> show d.channel <> " · Rample voice " <> show d.voice
      <> " (note " <> show d.trigger <> ", slice of " <> show d.slots
      <> " from " <> show d.pitchOfSlot0 <> ", CC " <> show (d.voice * 10 + 4)
      <> " " <> show d.settleMs <> "ms early)"
  DSample d ->
    "SuperDirt " <> d.set <> ":" <> show d.n <> " " <> show d.begin <> "–" <> show d.end <> "%"
      <> (if d.reverse then " reversed" else "")
      <> (if d.chop > 1 then " chop " <> show d.chop else "")
      <> " gain " <> show d.gain <> "% (Rig mode)"
  DRamplePoly d ->
    d.port <> " ch " <> show d.channel <> " · Rample, 4 voices allocated"
      <> " (notes " <> joinWith "/" (map show d.triggers)
      <> ", slice of " <> show d.slots <> " from " <> show d.pitchOfSlot0 <> ")"

-- | For the table cells, where the column already says which machine it is.
destShortLabel :: Destination -> String
destShortLabel = case _ of
  DMidi d -> d.port <> " " <> show d.channel
  DFh2Env d -> "env " <> show d.slot
  DFh2Gate d -> "8gt " <> show d.jack
  DEs9Gate d -> "GT" <> show d.block <> "/" <> show d.jack
  DEs9Cv d -> "cv " <> show d.bus
  DPoly d -> instrumentLabel d.inst <> (if d.sortByPitch then " ↓" else "")
  DContinuo d -> "cont " <> show d.channel
  DRample d -> "ramp v" <> show d.voice
  DRamplePoly _ -> "Rample x4"
  DSample d -> d.set <> ":" <> show d.n

-- ---------------------------------------------------------------------------
-- The table
-- ---------------------------------------------------------------------------

-- | One leg of a fan-out.
-- |
-- | `offsetMs` exists because **doubling is the point and doubling flams.** The
-- | paths are not the same length — an FH-2 gate goes browser → WebMIDI → FH-2,
-- | an ES-9 gate goes browser → rig WS → es9-daemon → CoreAudio buffer, and
-- | Ableton adds its own input buffer on top. Two kicks 5 ms apart is an audible
-- | flam, so a doubled kick with no per-leg trim works perfectly and sounds
-- | wrong — the worst available outcome, because the player blames the drums.
-- | Defaults to 0.0; DeepStar's calibration tables are where real defaults should
-- | come from once this is wired to them.
-- |
-- | `on` mutes a leg without deleting it, because trying the double and backing
-- | it out is the actual gesture. A muted leg keeps its offset and its place.
type Leg =
  { dest :: Destination
  , offsetMs :: Number
  , on :: Boolean
  }

type Route = { source :: Source, legs :: Array Leg }

type Table = Array Route

-- | The MIDI port a default table sends to, given the ports that exist: the IAC
-- | bus if there is one (the rig's Ableton template listens there), otherwise
-- | the first output. `""` when none are known yet, which port matching reads
-- | as "the first output there is" (a name matches by substring).
-- |
-- | Not "IAC" by fiat: on a machine without it switched on, or on Windows, a
-- | default that names IAC reaches nothing. See `docs/kb/plans/dashboard.md`,
-- | "Zero install".
defaultPort :: Array String -> String
defaultPort ports = fromMaybe "" (find (contains (Pattern "IAC")) ports <|> head ports)

-- | The default table before any port is known: every MIDI leg on `""`, the
-- | first output. A page replaces it with `defaultTableFor` its ports, once, at
-- | first run, and saves that, so the choice is made once and shown.
defaultTable :: Table
defaultTable = defaultTableFor []

-- | The default table == the standard Ableton project template, plus the FH-2
-- | envelope and drum-gate bindings the rig is actually patched for, with its
-- | MIDI legs on `defaultPort` of these ports.
-- |
-- | Odonus heads: note to the MIDI port on channels 1..4, envelope N to the
-- | FH-2. This is what shipped on 2026-08-08 as hardcoded constants; it is data
-- | now.
-- |
-- | Drum lanes: all sixteen to GM channel 10 on the MIDI port, which is what
-- | Balistes has always done. The first four ALSO drive FHX-8GT jacks 1..4, which
-- | reproduces `apply-drum-breakout.mjs`'s `KIT` table — the same four rows, now
-- | somewhere a player can change them.
defaultTableFor :: Array String -> Table
defaultTableFor ports =
  odonus <> drums <> [ { source: SVetulaVoice "", legs: [ midiLeg iac 5 ] } ]
  where
  iac = defaultPort ports
  midiLeg port ch = { dest: DMidi { port, channel: ch }, offsetMs: 0.0, on: true }
  envLeg slot = { dest: DFh2Env { slot }, offsetMs: 0.0, on: true }
  gateLeg note jack = { dest: DFh2Gate { note, jack }, offsetMs: 0.0, on: true }
  odonus =
    map (\h -> { source: SOdonusHead h, legs: [ midiLeg iac (h + 1), envLeg (h + 1) ] })
      [ 0, 1, 2, 3 ]
  -- canonKit order: BD SD CP RS CH PH OH LT MT HT RD RB CR CW TB SH.
  -- The breakout script's four: BD→1, SD→2, HH(CH)→3, CP→4.
  kitNotes = [ 36, 38, 39, 37, 42, 44, 46, 41, 47, 50, 51, 53, 49, 56, 54, 70 ]
  gateOf i = case i of
    0 -> [ gateLeg 36 1 ]
    1 -> [ gateLeg 38 2 ]
    2 -> [ gateLeg 39 4 ]
    4 -> [ gateLeg 42 3 ]
    _ -> []
  drums =
    mapWithIndex (\i _ -> { source: SDrumLane i, legs: [ midiLeg iac 10 ] <> gateOf i })
      kitNotes

-- | Every leg declared for a source, muted ones included (the editor wants those).
legsFor :: Table -> Source -> Array Leg
legsFor tbl src = maybe [] _.legs (find (\r -> r.source == src) tbl)

-- | Only the legs that should actually emit. This is what an emit path calls, so
-- | that "muted" is honoured in exactly one place.
liveLegsFor :: Table -> Source -> Array Leg
liveLegsFor tbl src = filter _.on (legsFor tbl src)

-- | Replace one source's legs, appending the row if it isn't in the table yet —
-- | so a Vetula voice or Selene bank that appears at runtime can be routed
-- | without the table having to have predicted it.
setLegs :: Source -> Array Leg -> Table -> Table
setLegs src legs tbl = case findIndex (\r -> r.source == src) tbl of
  Just i -> mapWithIndex (\j r -> if j == i then r { legs = legs } else r) tbl
  Nothing -> snoc tbl { source: src, legs }

-- | Edit one leg in place. Out-of-range index is a no-op rather than an error:
-- | the editor and the table can race a render, and dropping a stale click is
-- | better than throwing under the player.
modifyLeg :: Source -> Int -> (Leg -> Leg) -> Table -> Table
modifyLeg src i f tbl = setLegs src (mapWithIndex (\j l -> if j == i then f l else l) (legsFor tbl src)) tbl

removeLeg :: Source -> Int -> Table -> Table
removeLeg src i tbl = setLegs src (mapWithIndex Tuple (legsFor tbl src) # filter (\(Tuple j _) -> j /= i) # map snd) tbl

addLeg :: Source -> Destination -> Table -> Table
addLeg src d tbl = setLegs src (snoc (legsFor tbl src) { dest: d, offsetMs: 0.0, on: true }) tbl

-- | Set one numeric field of a destination, by name, clamped to what the hardware
-- | actually has. Clamping here rather than in the UI means a typed value can
-- | never address a jack that isn't there — the editor is a wire like any other.
setDestField :: String -> Int -> Destination -> Destination
setDestField field v = case _ of
  DMidi d -> case field of
    "channel" -> DMidi d { channel = clamp 1 16 v }
    _ -> DMidi d
  DFh2Env d -> case field of
    "slot" -> DFh2Env d { slot = clamp 1 8 v }
    _ -> DFh2Env d
  DFh2Gate d -> case field of
    "note" -> DFh2Gate d { note = clamp 0 127 v }
    "jack" -> DFh2Gate d { jack = clamp 1 8 v }
    _ -> DFh2Gate d
  DEs9Gate d -> case field of
    "block" -> DEs9Gate d { block = clamp 0 7 v }
    "jack" -> DEs9Gate d { jack = clamp 1 8 v }
    _ -> DEs9Gate d
  DEs9Cv d -> case field of
    "bus" -> DEs9Cv d { bus = clamp 1 16 v }
    _ -> DEs9Cv d
  DContinuo d -> case field of
    "channel" -> DContinuo d { channel = clamp 1 16 v }
    _ -> DContinuo d
  DRample d -> case field of
    "channel" -> DRample d { channel = clamp 1 16 v }
    "voice" -> DRample d { voice = clamp 1 4 v }
    "trigger" -> DRample d { trigger = clamp 0 127 v }
    "slots" -> DRample d { slots = clamp 1 128 v }
    "pitchOfSlot0" -> DRample d { pitchOfSlot0 = clamp 0 127 v }
    "settleMs" -> DRample d { settleMs = clamp 0 500 v }
    _ -> DRample d
  DRamplePoly d -> case field of
    "channel" -> DRamplePoly d { channel = clamp 1 16 v }
    "slots" -> DRamplePoly d { slots = clamp 1 128 v }
    "pitchOfSlot0" -> DRamplePoly d { pitchOfSlot0 = clamp 0 127 v }
    "trig1" -> setTrig 0
    "trig2" -> setTrig 1
    "trig3" -> setTrig 2
    "trig4" -> setTrig 3
    _ -> DRamplePoly d
    where
    setTrig i = DRamplePoly d { triggers = fromMaybe d.triggers (updateAt i (clamp 0 127 v) d.triggers) }
  -- A polyphonic instrument has no numeric field to nudge: WHICH jack a note
  -- reaches is the allocator's decision, and the set it chooses between is a
  -- property of how the module is patched, not of this route.
  DPoly d -> DPoly d
  -- The window keeps at least one percent, so it never plays nothing.
  DSample d -> case field of
    "n" -> DSample d { n = clamp 0 999 v }
    "begin" -> DSample d { begin = clamp 0 (d.end - 1) v }
    "end" -> DSample d { end = clamp (d.begin + 1) 100 v }
    "reverse" -> DSample d { reverse = v /= 0 }
    "gain" -> DSample d { gain = clamp 0 200 v }
    "chop" -> DSample d { chop = clamp 1 16 v }
    _ -> DSample d

-- ---------------------------------------------------------------------------
-- Reachability — can this leg actually emit, right now?
-- ---------------------------------------------------------------------------

-- | Whether a destination is reachable from the browser as things stand.
-- |
-- | This exists because of the failure shape this rig keeps producing: something
-- | load-bearing is dead and every surface still reads OK. A leg that cannot emit
-- | must SAY so — `Midi.findOutput` returning `Nothing` otherwise just means
-- | silence, and silence is indistinguishable from a musical decision.
data Reach
  = Reachable
  | NoPort String   -- named port absent from WebMIDI
  | NeedsRig        -- only emittable through the rig WS (browsers can't send UDP)
  | NotBuilt        -- a destination the table can name that nothing sends to yet

derive instance eqReach :: Eq Reach

-- | What the caller knows about the world. Kept as plain data so `reachOf` stays
-- | pure and testable rather than reaching for `Effect`.
type Ports =
  { found :: Array String  -- WebMIDI output port names present
  , rigUp :: Boolean       -- the Binnacle socket is connected
  }

reachOf :: Ports -> Destination -> Reach
reachOf ports = case _ of
  DMidi d -> portReach d.port
  -- Both FH-2 kinds are emitted as MIDI notes on the FH-2's own port, so the
  -- port is what gates them. (Their CONFIG side — which jack an MCV drives, what
  -- shape an envelope has — is the daemon's, applied out of band.)
  DFh2Env _ -> portReach fh2Port
  DFh2Gate _ -> portReach fh2Port
  -- The ES-9 CV/gate generators live in es9-daemon behind OSC over UDP, which a
  -- browser cannot speak, so these would have to go through the rig. But no
  -- code, browser or rig, sends a leg of either kind yet: they used to read
  -- Reachable whenever the rig was up, and a drum lane routed to an ES-9 gate
  -- stayed silent with the router saying all was well (2026-09-28).
  DEs9Gate _ -> NotBuilt
  DEs9Cv _ -> NotBuilt
  -- Played by the rig voice, so only through the rig.
  DSample _ -> if ports.rigUp then Reachable else NeedsRig
  -- Same route as any other ES-9 CV: the allocator runs in the browser, but the
  -- voltages it decides on still travel over the rig WS to es9-daemon.
  DPoly _ -> if ports.rigUp then Reachable else NeedsRig
  DContinuo _ -> portReach "continuo"
  DRample d -> portReach d.port
  DRamplePoly d -> portReach d.port
  where
  -- Substring, matching `Binnacle.Midi.findOutput`'s `indexOf` semantics, so the
  -- router's idea of "found" cannot disagree with the emit path's.
  portReach p = if isJust (find (contains (Pattern p)) ports.found) then Reachable else NoPort p

-- | Every live leg of a source that cannot currently emit, with the reason.
-- |
-- | The one function every surface should ask before rendering a source as
-- | healthy. `Midi.findOutput` returning `Nothing` produces silence, and silence
-- | is indistinguishable from a musical decision — so "nothing came out" has to
-- | be derivable from state rather than inferred by the player.
unreachable :: Ports -> Table -> Source -> Array { dest :: Destination, why :: Reach }
unreachable ports tbl src =
  mapMaybe check (liveLegsFor tbl src)
  where
  check l = case reachOf ports l.dest of
    Reachable -> Nothing
    why -> Just { dest: l.dest, why }

reachNote :: Reach -> String
reachNote = case _ of
  Reachable -> ""
  NoPort p -> "no '" <> p <> "' port"
  NeedsRig -> "needs the rig"
  NotBuilt -> "nothing sends here yet"

-- ---------------------------------------------------------------------------
-- Lowering — every reachable destination is a note on a port
-- ---------------------------------------------------------------------------

-- | The FH-2's own USB MIDI port. Same needle fh2-config uses.
fh2Port :: String
fh2Port = "FH-2"

-- | The MIDI channel the FH-2's note-filtered trigger MCVs listen on. Mirrors
-- | `CH` in `fh2-config/scripts/apply-drum-breakout.mjs`, which is what actually
-- | configures them — so if that changes, this must, and the pair is worth
-- | keeping in view. (Once the router can push the MCV config itself, this
-- | becomes a field of `DFh2Gate` rather than a constant.)
fh2GateChannel :: Int
fh2GateChannel = 10

-- | What an emit path actually needs: a port, a canonical channel, and possibly
-- | a note number that replaces the source's own.
-- |
-- | The unification worth noticing: **every destination reachable from the
-- | browser today is a MIDI note on some port.** An FH-2 envelope is a note on
-- | the FH-2 port at channel = slot; an FH-2 gate is a note on the FH-2 port at
-- | the MCV listen channel whose PITCH is the selector the MCV matches. So one
-- | emit primitive serves all of them, and Odonus and Balistes stop having
-- | separate output code.
-- |
-- | `Nothing` means "not emittable from the browser" — the ES-9 kinds, which live
-- | behind es9-daemon's OSC over UDP. Those are `NeedsRig` in `reachOf`, and a
-- | caller that silently skipped them without saying so would be reproducing the
-- | exact failure this module exists to prevent.
type Wire =
  { port :: String
  , channel :: Int          -- canonical 1..16
  , noteOverride :: Maybe Int
  -- `Just` only for a Rample, whose pitch does not travel in the note. Carried
  -- on the wire rather than handled by a separate emit path so that a Rample
  -- leg keeps every property of an ordinary one — per-leg offset, mute,
  -- reachability, fan-out — and no caller has to special-case it.
  , rample :: Maybe RampleWire
  }

-- | What a Rample needs in order to hear a pitch: the arithmetic lives in
-- | `Reef.Rample`, and these are its inputs. Same module the purerl-tidal sink
-- | calls, so the browser and the BEAM cannot drift about which slice a G4 is.
type RampleWire =
  { voice :: Int
  , trigger :: Int
  , slots :: Int
  , pitchOfSlot0 :: Int
  , settleMs :: Int
  }

-- | Whether this destination carries a musical LINE — something for which
-- | legato, portamento, ties and ratchets are meaningful — as opposed to a
-- | TRIGGER, which is fired once and has no pitch continuity to preserve.
-- |
-- | An FH-2 envelope or gate is a trigger: sliding into it means nothing, and
-- | retriggering it per ratchet would be a different musical decision from the
-- | one the player made in the note grid. A MIDI or continuo destination is a
-- | line. Odonus branches on this so its expressive logic runs where it applies
-- | and nowhere else.
carriesLine :: Destination -> Boolean
carriesLine = case _ of
  DMidi _ -> true
  DContinuo _ -> true
  -- A TRIGGER, despite being pitched. Each note is a fresh start-point CC and a
  -- fresh strike of a slice; there is no continuous pitch to slide along, so
  -- legato and portamento have nothing to act on. Saying `true` here would let
  -- Odonus tie notes together and suppress exactly the retriggers that ARE the
  -- notes.
  DRample _ -> false
  DRamplePoly _ -> false
  DFh2Env _ -> false
  DFh2Gate _ -> false
  DEs9Gate _ -> false
  DEs9Cv _ -> true      -- a pitch CV bus is a line; glide is exactly what it wants
  -- Also a line: its voices are pitched and legato within a voice is meaningful.
  -- The subtlety is that a note MIGRATING between voices must not glide — the
  -- arriving oscillator would audibly slide in from whatever it held before — so
  -- `Triggerfish.Poly` emits migration pitches un-slewed regardless of this.
  DPoly _ -> true
  -- One strike of a sample: nothing to slide along.
  DSample _ -> false

wireOf :: Destination -> Maybe Wire
wireOf = case _ of
  DMidi d -> Just { port: d.port, channel: d.channel, noteOverride: Nothing, rample: Nothing }
  DFh2Env d -> Just { port: fh2Port, channel: d.slot, noteOverride: Nothing, rample: Nothing }
  -- The jack is not addressed here: it is baked into the MCV by fh2-config, and
  -- what selects it from this side is the NOTE. That asymmetry is the reason
  -- `DFh2Gate` carries both halves — see its comment.
  DFh2Gate d -> Just { port: fh2Port, channel: fh2GateChannel, noteOverride: Just d.note, rample: Nothing }
  DContinuo d -> Just { port: "continuo", channel: d.channel, noteOverride: Nothing, rample: Nothing }
  -- The trigger note is the note, always; the PITCH becomes a CC, and the emit
  -- path reads it off `rample` rather than from `note`.
  DRample d -> Just
    { port: d.port
    , channel: d.channel
    , noteOverride: Just d.trigger
    , rample: Just
        { voice: d.voice, trigger: d.trigger, slots: d.slots
        , pitchOfSlot0: d.pitchOfSlot0, settleMs: d.settleMs
        }
    }
  DEs9Gate _ -> Nothing
  DEs9Cv _ -> Nothing
  -- Not a MIDI wire, and not one wire at all: the allocator chooses among
  -- several per note. `Triggerfish.Poly` drives it instead.
  DPoly _ -> Nothing
  -- Not one wire either: the allocator picks a voice per note, so this is
  -- driven by the Rample pass rather than the per-leg fan-out.
  DRamplePoly _ -> Nothing
  -- Not MIDI: a /dirt/play from the rig (`Reef.Routing`'s voice legs).
  DSample _ -> Nothing

-- ---------------------------------------------------------------------------
-- Claims — what is spoken for, and by whom
-- ---------------------------------------------------------------------------

-- | One occupied hardware slot, and who occupies it.
type Claim = { device :: Device, slot :: String, by :: Array Source }

-- | The PHYSICAL OUTPUT a destination lands on, in `Selene.Layout`'s address
-- | vocabulary (device / bank / 0-based slot), or `Nothing` for destinations that
-- | are not a jack at all.
-- |
-- | This is the structured form of what `claims` renders as prose, and both use
-- | it, so the output-backward view and the conflict report cannot disagree about
-- | where something lands.
-- |
-- | Note the two FH-2 kinds go to DIFFERENT banks — an envelope to the FH-2's own
-- | panel, a trigger out the FHX-8GT — which is the distinction that was collapsed
-- | and had to be fixed on the rack.
outputsOf :: Destination -> Array Output
outputsOf = case _ of
  DFh2Env d -> [ { device: "fh2", bank: "main", slot: d.slot - 1 } ]
  DFh2Gate d -> [ { device: "fh2", bank: "gt0", slot: d.jack - 1 } ]
  DEs9Gate d -> [ { device: "es9", bank: "gt" <> show d.block, slot: d.jack - 1 } ]
  DEs9Cv d -> [ es9CvOutput d.bus ]
  -- A polyphonic instrument spends EVERY jack it can allocate onto, plus the one
  -- that decides how many are audible. Reporting only one would under-report the
  -- spend, and the failure that hides is the quiet one: something else claiming
  -- voice 3's bus while the allocator still believes it owns it.
  DPoly d ->
    let js = polyJacks d.inst
    in map es9CvOutput (snoc js.voiceBuses js.ctrlBus)
  DMidi _ -> []
  DContinuo _ -> []
  DRample _ -> []
  DRamplePoly _ -> []
  DSample _ -> []

-- | es9-daemon's `/cv <bus>`: buses 8..15 ARE the ES-9's eight panel jacks, so
-- | bus 8 is panel jack 1. Confirmed twice — `reference_es9_channel_mapping` and
-- | DeepStar's CALIBRATION.md bus map ("bus 8 drives the Tona on ES-9 output 1"),
-- | and used live by `reef_voice`'s odonus_cv routes (pitch_bus 8 = out 1).
-- |
-- | Anything outside 8..15 is an expander bus whose map we have not established;
-- | it gets its own bank name rather than being folded into the panel, so a wrong
-- | guess shows up as an unknown bank instead of silently colliding with jack 1.
es9CvOutput :: Int -> Output
es9CvOutput bus
  | bus >= 8 && bus <= 15 = { device: "es9", bank: "main", slot: bus - 8 }
  | otherwise = { device: "es9", bank: "cv?", slot: bus }


-- | Every hardware slot the table spends, grouped so the same slot claimed twice
-- | shows both claimants.
-- |
-- | **This REPORTS; it does not enforce.** The daemons already own admission —
-- | es9-daemon has capability and overlap checks with `!` eviction, fh2-config
-- | has `PortClaim`. A second opinion computed here would be a second thing to
-- | drift, and the lesson of the output-range bug is exactly that: the client
-- | that duplicates a policy silently overrides it. So the router's job is to
-- | make the spend visible before the player hears it, not to arbitrate.
-- |
-- | MIDI destinations are deliberately excluded: a channel costs nothing and
-- | several sources sharing one is legal and often wanted (layering).
claims :: Table -> Array Claim
claims tbl = map collect (nub (map _.slot spent))
  where
  spent = concatMap legsOf tbl
  legsOf r = concatMap (slotOf r.source) (filter _.on r.legs)
  -- **An FH-2 leg spends TWO resources, and they are not the same resource.**
  -- Collapsing them into one was a real bug: envelopes and drum triggers appeared
  -- to fight over the FH-2's main panel jacks, which is exactly backwards from
  -- what the rig does.
  --
  --   * the **MCV** (0..15) — the scarce internal thing. polyenv allocates MCV
  --     0..7, envelope N on MCV N-1. The drum breakout's note-filtered triggers
  --     use MCV 0..3. So they DO collide here, and this is the collision that
  --     matters: applying a polyenv takes the drum gates away.
  --
  --   * the **physical output**, which is different hardware for each. polyenv
  --     lands on the FH-2's own panel jack N. A drum trigger is routed out the
  --     **FHX-8GT** (`output = jack + 64`, jack 1 = 65) precisely so the FH-2's
  --     CV-capable main jacks stay free — see the header of
  --     `fh2-config/scripts/apply-drum-breakout.mjs`, which is the source of
  --     truth for this config.
  --
  -- So both claims are issued. The MCV claim catches the cross-family collision;
  -- the output claim catches two legs aimed at one jack, which the MCV claim
  -- cannot see because they would be on different MCVs.
  slotOf src leg = case leg.dest of
    DFh2Env d -> [ { device: DevFh2, slot: mcvSlot (d.slot - 1), by: [ src ] } ] <> jack leg
    DFh2Gate d -> [ { device: DevFh2, slot: mcvSlot (d.jack - 1), by: [ src ] } ] <> jack leg
    DEs9Gate _ -> jack leg
    DEs9Cv _ -> jack leg
    DPoly _ -> jack leg
    DMidi _ -> []
    DContinuo _ -> []
    DRample _ -> []
    DRamplePoly _ -> []
    DSample _ -> []
    where
    -- The output half, from the one structured definition, so this and the
    -- backward view cannot drift about where a destination lands.
    jack lg = map (\o -> { device: destDevice lg.dest, slot: outputKey o, by: [ src ] })
      (outputsOf lg.dest)

  -- The MCV a drum trigger uses is `jack - 1` only because the breakout table
  -- happens to pair slot 0..3 with jack 1..4. That table says "EDIT HERE to
  -- re-map", so the two can be pulled apart — at which point this has to learn
  -- the slot rather than derive it.
  mcvSlot n = "MCV " <> show n
  collect s =
    let here = filter (\c -> c.slot == s) spent
    in { device: fromMaybe DevFh2 (map _.device (here !! 0))
       , slot: s
       , by: concatMap _.by here
       }

-- | Claims wanted by more than one source. Worth showing, not blocking: two
-- | sources on one gate is usually a mistake and occasionally a deliberate OR.
conflicts :: Table -> Array Claim
conflicts = filter (\c -> length c.by > 1) <<< claims
