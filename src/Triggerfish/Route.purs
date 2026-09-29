-- | **URL routing for the rack** — `#vetula/hunt/tonnetz`, `#odonus/review`.
-- |
-- | A route is `machine × stage`, and it only became worth writing once the Stage
-- | collapse (docs/DESIGN-stages.md) gave each machine ONE closed mode type.
-- | Before that Vetula's mode was `view` plus a nested `captureView` flag while
-- | Odonus's was a differently-named, differently-shaped `OdonusView`; a URL would
-- | have had to encode two per-machine schemes with no common shape.
-- |
-- | **The shell owns the machine segment; each machine owns the rest.** This
-- | module never learns what a stage is — it carries `stage :: Array String`
-- | opaquely and hands it to the machine, which parses its own vocabulary
-- | (`Vetula.App.stagePath` / `stageFromPath`, and Odonus's pair). Adding a stage,
-- | or a machine with a stage vocabulary of its own, touches that machine only.
-- | Same decoupling as the action-polymorphic view modules.
-- |
-- | **The hash is an EFFECT of state, never a second source of truth.** It is
-- | written when the shell changes machine or a machine reports a stage change,
-- | and read only at startup and on a genuine `hashchange` (a pasted URL, a
-- | bookmark). Writing goes through `history.replaceState`, which does not emit
-- | `hashchange`, so the app cannot hear its own writes — the click → state →
-- | hash → event → state loop is impossible by construction rather than by a
-- | guard flag.
module Triggerfish.Route
  ( Route
  , routeOf
  , machineOf
  , stageOf
  , print
  , parse
  , machineSlug
  , machineFromSlug
  , readHash
  , writeHash
  , onHashChange
  ) where

import Prelude

import Data.Array (drop, filter, head)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), joinWith, split, stripPrefix, toLower, trim)
import Effect (Effect)
import Triggerfish.Transport (Which(..))

-- | A parsed location: which machine, and that machine's own stage path (empty =
-- | "wherever it already is", so `#vetula` is a valid route that switches machine
-- | without disturbing the stage).
newtype Route = Route { machine :: Which, stage :: Array String }

routeOf :: Which -> Array String -> Route
routeOf machine stage = Route { machine, stage }

machineOf :: Route -> Which
machineOf (Route r) = r.machine

stageOf :: Route -> Array String
stageOf (Route r) = r.stage

-- | The URL name for a machine. Lower-case full names rather than the three-letter
-- | internal tags: a URL is read by a person, and `#odonus/review` says what it is
-- | where `#odo/review` needs the decoder ring.
machineSlug :: Which -> String
machineSlug = case _ of
  Odo -> "odonus"
  Bal -> "balistes"
  Sel -> "selene"
  Vet -> "vetula"
  Tid -> "tidal"
  Suf -> "sufflamen"

machineFromSlug :: String -> Maybe Which
machineFromSlug s = case toLower (trim s) of
  "odonus" -> Just Odo
  -- Balistes has its own page (balistes.html), so an old link to it here is
  -- ignored rather than opening an empty pane.
  "balistes" -> Nothing
  -- The Selene rack has its own page too (selene.html).
  "selene" -> Nothing
  "vetula" -> Just Vet
  "tidal" -> Just Tid
  "sufflamen" -> Just Suf
  _ -> Nothing

-- | `Route → "vetula/hunt/tonnetz"` (no leading `#` — `writeHash` adds it).
print :: Route -> String
print (Route r) = joinWith "/" ([ machineSlug r.machine ] <> r.stage)

-- | `"vetula/hunt/tonnetz" → Route`. Tolerant on purpose, since the input may be
-- | hand-typed: a leading `#`, leading/trailing slashes, empty segments and case
-- | are all forgiven. An unknown machine yields `Nothing` and the caller leaves
-- | the app where it is, rather than throwing the user somewhere arbitrary.
parse :: String -> Maybe Route
parse raw =
  let trimmed = trim raw
      unhashed = fromMaybe trimmed (stripPrefix (Pattern "#") trimmed)
      segs = filter (_ /= "") (map trim (split (Pattern "/") unhashed))
  in case head segs >>= machineFromSlug of
       Nothing -> Nothing
       Just machine -> Just (Route { machine, stage: map toLower (drop 1 segs) })

foreign import readHashImpl :: Effect String
foreign import writeHashImpl :: String -> Effect Unit
foreign import onHashChangeImpl :: (String -> Effect Unit) -> Effect Unit

-- | The current fragment, `#` stripped.
readHash :: Effect String
readHash = readHashImpl

-- | Replace the fragment without touching history (see the module note).
writeHash :: String -> Effect Unit
writeHash = writeHashImpl

-- | Listen for fragment changes the app did not make.
onHashChange :: (String -> Effect Unit) -> Effect Unit
onHashChange = onHashChangeImpl
