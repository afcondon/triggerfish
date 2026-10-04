-- | **DeepStar, as the Dashboard sees it**: the rig doctor's checks
-- | (`deepstar serve`, :3027, `GET /doctor`). Bosun knows whether a daemon
-- | runs; DeepStar knows what Bosun cannot: whether the ES-9 is on the USB
-- | bus at all (es9-daemon notices neither its leaving nor its return), and
-- | whether a daemon's control socket actually answers.
-- |
-- | Nothing here is needed to play: with DeepStar out of reach the chart
-- | shows only Bosun's lamps.
module Triggerfish.DeepStar
  ( Check
  , doctor
  , es9Absent
  , refusing
  ) where

import Prelude

import Data.Array (any, filter)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)

type Check = { name :: String, status :: String, detail :: String }

foreign import doctorImpl :: (Nullable (Array Check) -> Effect Unit) -> Effect Unit

-- | The doctor's checks, or Nothing when DeepStar is out of reach.
doctor :: Aff (Maybe (Array Check))
doctor = makeAff \done -> do
  doctorImpl (done <<< Right <<< toMaybe)
  pure nonCanceler

-- | The ES-9 is not on the bus (its own check, not a daemon's word).
es9Absent :: Array Check -> Boolean
es9Absent = any (\c -> c.name == "ES-9 present" && c.status == "down")

-- | The daemons whose control socket does not answer, by their check names.
refusing :: Array Check -> Array String
refusing cs = map _.name (filter (\c -> c.status == "down" && (c.name == "es9-daemon" || c.name == "fh2-daemon")) cs)
