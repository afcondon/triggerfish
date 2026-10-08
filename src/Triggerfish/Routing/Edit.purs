-- | Every edit the router can make to the routing table, as data, and the one
-- | function that makes it.
-- |
-- | These were the `Rt*` actions of the Triggerfish shell. They are here so that
-- | any page carrying a router (Triggerfish's ⌥1, Balistes on its own page) edits
-- | the table the same way. The page still owns the table: it applies an edit,
-- | saves, and pushes the result to its machines.
module Triggerfish.Routing.Edit
  ( Edit(..)
  , Context
  , apply
  , resetSources
  , newDest
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (find, foldl, head)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number as Number
import Data.String as String
import Triggerfish.Balistes.Pattern as P
import Triggerfish.Routing.Model as RM
import Triggerfish.SampleSets (SampleSet)

-- | What a new leg's defaults are drawn from: the MIDI ports that exist, and the
-- | sample sets.
type Context =
  { ports :: Array String
  , sampleSets :: Array SampleSet
  }

-- | One edit, on leg `i` of a source. Field values arrive as the text typed, so
-- | a half-typed number is no edit at all rather than a zero.
data Edit
  = ToggleLeg RM.Source Int
  | RemoveLeg RM.Source Int
  | AddLeg RM.Source String
  | SetField RM.Source Int String String
  | SetOffset RM.Source Int String
  | SetPort RM.Source Int String
  | SetSampleSet RM.Source Int String

-- | The table after the edit, or `Nothing` when there is nothing to change: a
-- | field mid-typing, or an unknown kind of leg.
apply :: Context -> Edit -> RM.Table -> Maybe RM.Table
apply ctx edit tbl = case edit of
  ToggleLeg src i -> Just (modify src i \l -> l { on = not l.on })
  RemoveLeg src i -> Just (RM.removeLeg src i tbl)
  AddLeg src kind -> (\d -> RM.addLeg src d tbl) <$> newDest ctx src kind
  SetField src i field v ->
    (\n -> modify src i \l -> l { dest = RM.setDestField field n l.dest }) <$> Int.fromString v
  SetOffset src i v -> (\n -> modify src i \l -> l { offsetMs = n }) <$> Number.fromString v
  SetPort src i port -> Just (modify src i \l -> l { dest = setPort port l.dest })
  SetSampleSet src i set -> Just (modify src i \l -> l { dest = chooseSet set l.dest })
  where
  modify src i f = RM.modifyLeg src i f tbl
  chooseSet set = case _ of
    RM.DSample d -> RM.DSample d { set = set, n = 0 }
    d -> d

-- | These sources back to the shipped defaults for these ports, leaving every
-- | other source as it is: a page that shows one machine's routes restores only
-- | those.
resetSources :: Array String -> Array RM.Source -> RM.Table -> RM.Table
resetSources ports srcs tbl =
  foldl (\t src -> RM.setLegs src (RM.legsFor (RM.defaultTableFor ports) src) t) tbl srcs

setPort :: String -> RM.Destination -> RM.Destination
setPort port = case _ of
  RM.DMidi d -> RM.DMidi d { port = port }
  RM.DRample d -> RM.DRample d { port = port }
  RM.DRamplePoly d -> RM.DRamplePoly d { port = port }
  -- No port of their own: the FH-2 and continuo are fixed rig fixtures reached
  -- by name, and the ES-9 kinds are not MIDI at all.
  d@(RM.DFh2Env _) -> d
  d@(RM.DFh2Gate _) -> d
  d@(RM.DEs9Gate _) -> d
  d@(RM.DEs9Cv _) -> d
  d@(RM.DPoly _) -> d
  d@(RM.DContinuo _) -> d
  d@(RM.DSample _) -> d

-- | A freshly-added destination of the given kind, with sensible starting values
-- | for THIS source. A new FH-2 gate on a drum lane starts on that lane's own
-- | canonKit note, because the note is the selector the MCV matches — starting it
-- | at 0 would add a leg that silently never fires.
newDest :: Context -> RM.Source -> String -> Maybe RM.Destination
newDest ctx src = case _ of
  "midi" -> Just (RM.DMidi { port: firstPort, channel: 1 })
  "fh2env" -> Just (RM.DFh2Env { slot: 1 })
  "fh2gate" -> Just (RM.DFh2Gate { note: laneNote, jack: 1 })
  "es9gate" -> Just (RM.DEs9Gate { block: 0, jack: 1 })
  "es9cv" -> Just (RM.DEs9Cv { jack: 1, gate: 0 })
  "poly-saich" -> Just (RM.DPoly { inst: RM.Saich, sortByPitch: false })
  "poly-saich-sorted" -> Just (RM.DPoly { inst: RM.Saich, sortByPitch: true })
  -- No sorted variant: Rings has one pitch bus, so there is no seating to sort.
  "poly-rings" -> Just (RM.DPoly { inst: RM.Rings, sortByPitch: false })
  "continuo" -> Just (RM.DContinuo { channel: 1 })
  -- A drum set first, if there is one, since this is most often a drum lane.
  "sample" -> Just (RM.DSample
    { set: maybe "" _.name (find (\x -> String.contains (String.Pattern "drum") x.name) ctx.sampleSets <|> head ctx.sampleSets)
    , n: 0, begin: 0, end: 100, reverse: false, gain: 100, chop: 1 })
  -- One entry per voice rather than one entry plus a voice field, because the
  -- trigger note is NOT derivable from the voice in general — it is whatever
  -- the module's SETTINGS > SPx say — and offering the four the card is set up
  -- for beats making the player look them up. Defaults describe the piano at
  -- P0: SLICER /64, slice 0 = C2, SP1-4 = 60..63, 40 ms settle (measured).
  "rample-1" -> Just (rample 1 60)
  "rample-2" -> Just (rample 2 61)
  "rample-3" -> Just (rample 3 62)
  "rample-4" -> Just (rample 4 63)
  -- The whole module as one instrument: one leg per HEAD, not per voice, and
  -- the allocator decides which voice sounds each note.
  "rample-poly" -> Just (RM.DRamplePoly
    { port: firstPort, channel: 1, triggers: [ 60, 61, 62, 63 ]
    , slots: 64, pitchOfSlot0: 36 })
  _ -> Nothing
  where
  firstPort = RM.defaultPort ctx.ports
  rample voice trigger = RM.DRample
    { port: firstPort, channel: 1, voice, trigger
    , slots: 64, pitchOfSlot0: 36, settleMs: 40 }
  laneNote = case src of
    RM.SDrumLane i -> P.laneNote i
    _ -> 36
