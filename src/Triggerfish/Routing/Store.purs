-- | `Triggerfish.Routing.Store` — localStorage persistence for the unified
-- | routing table.
-- |
-- | Routing is a PREFERENCE: the user's decision about where a machine's output
-- | goes, which nothing on the backend can contradict. So it persists, unlike
-- | anything that asserts what is currently playing (see
-- | `Triggerfish.Transport.Store` for the full form of that rule).
-- |
-- | The encoding is tagged strings rather than the `Destination` sum written
-- | directly. localStorage is a WIRE — an envelope written by an older or newer
-- | build must degrade to "no stored routing, use the defaults" rather than
-- | decode into a destination that means something else. A `DFh2Gate` misread as
-- | a `DEs9Gate` would silently send drums to the wrong module.
-- |
-- | Replaces: `Triggerfish.Odonus.RouteStore` (which held only the envelope
-- | pips), and the shell's ad-hoc `routing` map.
module Triggerfish.Routing.Store
  ( save
  , load
  ) where

import Prelude

import Data.Array (mapMaybe, unsnoc)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.String (Pattern(..), split, joinWith)
import Data.String as Str
import Effect (Effect)

import Triggerfish.Routing.Model (Destination(..), InstrumentId(..), Leg, Route, Source(..), Table, sourceKey)

-- | The on-disk shape. Sources and destinations are flat strings so that a row
-- | this build doesn't understand can be dropped individually, rather than
-- | poisoning the whole table.
type Saved = { routes :: Array { source :: String, legs :: Array StoredLeg } }

type StoredLeg = { dest :: String, offsetMs :: Number, on :: Boolean }

storeKey :: String
storeKey = "triggerfish.routing.v1"

foreign import _save :: String -> String -> Effect Unit
foreign import _load :: forall a. String -> Effect (Nullable a)
foreign import _stringify :: forall a. a -> String

-- ---------------------------------------------------------------------------
-- Source codec — mirrors `Model.sourceKey`, which is the canonical spelling
-- ---------------------------------------------------------------------------

sourceOf :: String -> Maybe Source
sourceOf s = case split (Pattern ".") s of
  [ "odonus", "head", n ] -> SOdonusHead <$> Int.fromString n
  [ "drums", "lane", n ] -> SDrumLane <$> Int.fromString n
  -- A Vetula voice name may itself contain dots, so rejoin the tail rather than
  -- requiring exactly three segments.
  _ -> case Str.stripPrefix (Pattern "vetula.voice.") s of
    Just nm -> Just (SVetulaVoice nm)
    Nothing -> map SSeleneBank (Str.stripPrefix (Pattern "selene.bank.") s)

-- The WRITE side is `Model.sourceKey` — this module only decodes. Restating the
-- spelling here would be a second copy to drift, which is precisely how the
-- envelope round-trip broke earlier today.

-- ---------------------------------------------------------------------------
-- Destination codec
-- ---------------------------------------------------------------------------

-- | `kind:field|field`. Ints are re-validated on the way in rather than trusted:
-- | an out-of-range envelope slot or jack would address hardware that isn't
-- | there, and a note going somewhere unintended is worse than a dropped leg.
destStr :: Destination -> String
destStr = case _ of
  DMidi d -> "midi:" <> d.port <> "|" <> show d.channel
  DFh2Env d -> "fh2env:" <> show d.slot
  DFh2Gate d -> "fh2gate:" <> show d.note <> "|" <> show d.jack
  DEs9Gate d -> "es9gate:" <> show d.block <> "|" <> show d.jack
  DEs9Cv d -> "es9cv:" <> show d.bus
  -- Only the instrument's NAME is stored. Which jacks it occupies is a fact
  -- about how the rack is patched, not about this preference, and baking the
  -- buses in here would let a saved route go on claiming jacks the module no
  -- longer uses. `Routing.Model.polyJacks` is the one place that knows.
  DPoly d -> "poly:" <> instKey d.inst <> "|" <> (if d.sortByPitch then "1" else "0")
  DContinuo d -> "continuo:" <> show d.channel

instKey :: InstrumentId -> String
instKey = case _ of
  Saich -> "saich"

instOf :: String -> Maybe InstrumentId
instOf = case _ of
  "saich" -> Just Saich
  _ -> Nothing

destOf :: String -> Maybe Destination
destOf s = case Str.indexOf (Pattern ":") s of
  Nothing -> Nothing
  Just i ->
    let kind = Str.take i s
        rest = Str.drop (i + 1) s
        parts = split (Pattern "|") rest
    in case kind, parts of
      "midi", [ p, c ] -> (\ch -> DMidi { port: p, channel: ch }) <$> inRange 1 16 c
      -- A port name containing "|" would split wrong; rejoin all but the last.
      "midi", _ -> case unsnocStr parts of
        Just { init, last } -> (\ch -> DMidi { port: joinWith "|" init, channel: ch }) <$> inRange 1 16 last
        Nothing -> Nothing
      "fh2env", [ n ] -> (\slot -> DFh2Env { slot }) <$> inRange 1 8 n
      "fh2gate", [ n, j ] -> (\note jack -> DFh2Gate { note, jack }) <$> inRange 0 127 n <*> inRange 1 8 j
      "es9gate", [ b, j ] -> (\block jack -> DEs9Gate { block, jack }) <$> inRange 0 7 b <*> inRange 1 8 j
      "es9cv", [ n ] -> (\bus -> DEs9Cv { bus }) <$> inRange 1 16 n
      -- The one-part form predates the sort flag; read it as unsorted rather
      -- than dropping the leg, so an older saved routing still plays.
      "poly", [ n ] -> (\inst -> DPoly { inst, sortByPitch: false }) <$> instOf n
      "poly", [ n, f ] -> (\inst -> DPoly { inst, sortByPitch: f == "1" }) <$> instOf n
      "continuo", [ c ] -> (\channel -> DContinuo { channel }) <$> inRange 1 16 c
      _, _ -> Nothing
  where
  inRange lo hi t = case Int.fromString t of
    Just n | n >= lo && n <= hi -> Just n
    _ -> Nothing
  unsnocStr = unsnoc

-- ---------------------------------------------------------------------------

save :: Table -> Effect Unit
save tbl = _save storeKey (_stringify env)
  where
  env :: Saved
  env = { routes: map row tbl }
  row r = { source: sourceKey r.source, legs: map leg r.legs }
  leg l = { dest: destStr l.dest, offsetMs: l.offsetMs, on: l.on }

-- | Load the stored table, or `Nothing` if absent / unparseable. Individual rows
-- | and legs that fail to decode are DROPPED rather than failing the load: a
-- | destination kind this build doesn't know is a leg that can't be honoured, and
-- | losing it is better than losing every other route with it.
load :: Effect (Maybe Table)
load = do
  ms <- map toMaybe (_load storeKey)
  pure case ms of
    Nothing -> Nothing
    Just (s :: Saved) -> Just (mapMaybe row s.routes)
  where
  row r = case sourceOf r.source of
    Nothing -> Nothing
    Just src -> Just ({ source: src, legs: mapMaybe leg r.legs } :: Route)
  leg :: StoredLeg -> Maybe Leg
  leg l = case destOf l.dest of
    Nothing -> Nothing
    Just d -> Just { dest: d, offsetMs: l.offsetMs, on: l.on }
