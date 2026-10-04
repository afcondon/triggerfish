-- | **Bosun, as the Dashboard sees it**: the Atlantis group's supervisor
-- | (`bosun supervise --held`, :3994). Its `/state` says which daemons are up,
-- | so the chart can light a lamp on each and break the streams that run
-- | through one that is down; its `/control` starts, stops and restarts them.
-- |
-- | Nothing here is needed to play: with Bosun out of reach the chart simply
-- | shows no lamps.
module Triggerfish.Bosun
  ( Health
  , Service
  , Lamp(..)
  , state
  , control
  , lampOf
  , nodeOf
  , placesOf
  , watchers
  , breaksStreams
  , Asked
  , Outcome(..)
  , outcome
  , restartTip
  ) where

import Prelude

import Data.Array (find)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)

-- | `since`: when it last changed state (Unix ms). A restart that worked moves
-- | it, or `restarts`; Bosun's "ok" alone does not say a process moved.
type Service = { id :: String, state :: String, restarts :: Int, gaveUp :: Boolean, since :: Number }

-- | `desired` is the group's own wish (`up` once raised, `down` while held).
type Health = { desired :: String, phase :: String, services :: Array Service }

foreign import stateImpl :: (Nullable Health -> Effect Unit) -> Effect Unit
foreign import controlImpl :: String -> String -> (Boolean -> Effect Unit) -> Effect Unit

-- | The group's state, or Nothing when Bosun is out of reach.
state :: Aff (Maybe Health)
state = makeAff \done -> do
  stateImpl (done <<< Right <<< toMaybe)
  pure nonCanceler

-- | A `/control` verb (`restart`, `spawn`, `stop`, `up`, `down`), for one
-- | service or ("") the group. True when Bosun accepted it.
control :: String -> String -> Aff Boolean
control verb service = makeAff \done -> do
  controlImpl verb service (done <<< Right)
  pure nonCanceler

-- | What a lamp says: up, on its way (starting, or waiting to retry), or down
-- | (stopped, or given up).
data Lamp = Up | Coming | Down

derive instance Eq Lamp

lampOf :: Service -> Lamp
lampOf s
  | s.gaveUp = Down
  | s.state == "running" || s.state == "ready" = Up
  | s.state == "starting" || s.state == "in-backoff" || s.state == "restarting" = Coming
  | otherwise = Down

-- | The chart's node for a service, where it has one.
nodeOf :: String -> Maybe String
nodeOf = case _ of
  "architeuthis" -> Just "engine"
  "diaphus" -> Just "diaphus"
  "es9-daemon" -> Just "d-es9"
  "superdirt" -> Just "d-dirt"
  "fh2-daemon" -> Just "fh2"
  "continuo" -> Just "continuo"
  "friends-of-itajara" -> Just "foi"
  _ -> Nothing

-- | Where a service's lamp stands on the chart, in the X-ray and out of it:
-- | its own node, or the node it serves. A machine's page server lamps that
-- | machine (`m:<slot>`): the page cannot be opened without it. Every page,
-- | the Dashboard's included, is served from :3023, so triggerfish-frontend
-- | lamps the Browser. Empty for a watcher (`watchers`) and for a service no
-- | one has placed yet, which the X-ray lists beside the chart rather than
-- | hide.
placesOf :: String -> Array String
placesOf = case _ of
  "friends-of-itajara" -> [ "foi", "m:quadrat" ]
  "fh2-drumkit" -> [ "fh2" ]
  "amphora" -> [ "sets" ]
  "triggerfish-frontend" -> [ "browser" ]
  "conspicillum-frontend" -> [ "m:conspicillum" ]
  "limulus" -> [ "m:limulus" ]
  id -> maybe [] pure (nodeOf id)

-- | The services that watch the rig and carry none of it: no line runs
-- | through them, so they stand above the chart, with Bosun.
watchers :: Array String
watchers = [ "deepstar" ]

-- | A restart asked of Bosun: when, and the service as it was.
type Asked = { service :: String, at :: Number, before :: Maybe Service }

data Outcome = Waiting | Moved | NothingMoved

derive instance Eq Outcome

-- | Bosun's "ok" means it took the request, not that a process moved (twice
-- | measured: `bosun-supervise-orphans`). So a restart is "waiting" until
-- | `/state` shows the service move, and "nothing moved" after 20 s.
outcome :: Number -> Maybe Health -> Asked -> Outcome
outcome now h a = case a.before, find (\s -> s.id == a.service) (maybe [] _.services h) of
  Just b, Just s | s.restarts /= b.restarts || s.since /= b.since -> Moved
  _, _ | now - a.at > 20000.0 -> NothingMoved
  _, _ -> Waiting

-- | What a restart costs, said before it is pressed.
restartTip :: String -> String
restartTip = case _ of
  "architeuthis" -> "Every page loses the rig for a few seconds; the rig's loops and marks are lost."
  "diaphus" -> "The rig's MIDI stops while it restarts. macOS may ask again for Local Network permission."
  "fh2-daemon" -> "Power the FH-2 on first: with it unplugged this gives up again after five tries."
  "triggerfish-frontend" -> "It serves every page, this one included; pages already open keep running."
  id -> "Ask Bosun to restart " <> id <> "."

-- | Whether a stream through this service's node stops when it is down. The
-- | FH-2's daemon only configures it (notes reach the FH-2 over MIDI without
-- | it), and Continuo's absence already shows as a missing port.
breaksStreams :: String -> Boolean
breaksStreams id = id /= "fh2-daemon" && id /= "continuo"
