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
  , breaksStreams
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
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

-- | Whether a stream through this service's node stops when it is down. The
-- | FH-2's daemon only configures it (notes reach the FH-2 over MIDI without
-- | it), and Continuo's absence already shows as a missing port.
breaksStreams :: String -> Boolean
breaksStreams id = id /= "fh2-daemon" && id /= "continuo"
