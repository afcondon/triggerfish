-- | **What the Dashboard cannot learn from Bosun**, asked of the Atlantis
-- | origin's server (`/api/rig/checks`, Friends of Itajara's `server.mjs`).
-- | Bosun knows whether a daemon runs. These say whether a daemon's control
-- | socket answers, whether es9-daemon's audio stream is reaching the ES-9
-- | (its own `status`), and whether the looper's latency figure was measured
-- | with the audio programs running now.
-- |
-- | Nothing here is needed to play: with the server out of reach the chart
-- | shows only Bosun's lamps. (DeepStar's `/doctor` answered these until it
-- | was retired, 2026-11-14.)
module Triggerfish.RigChecks
  ( Check
  , checks
  , es9Absent
  , refusing
  , latencyStale
  ) where

import Prelude

import Data.Array (any, filter, find)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)

-- | `status` is `ok`, `down` (it is wrong) or `unknown` (it could not be asked).
type Check = { name :: String, status :: String, detail :: String }

foreign import checksImpl :: (Nullable (Array Check) -> Effect Unit) -> Effect Unit

-- | The checks, or Nothing when the server is out of reach.
checks :: Aff (Maybe (Array Check))
checks = makeAff \done -> do
  checksImpl (done <<< Right <<< toMaybe)
  pure nonCanceler

-- | es9-daemon says its stream to the ES-9 is lost (unplugged or powered
-- | down). It reopens the device by itself when the ES-9 returns.
es9Absent :: Array Check -> Boolean
es9Absent = any (\c -> c.name == "ES-9 present" && c.status == "down")

-- | The daemons whose control socket does not answer, by their check names.
refusing :: Array Check -> Array String
refusing cs = map _.name (filter (\c -> c.status == "down" && (c.name == "es9-daemon" || c.name == "fh2-daemon")) cs)

-- | The looper's latency figure can no longer be believed, and why.
latencyStale :: Array Check -> Maybe String
latencyStale cs = _.detail <$> find (\c -> c.name == "looper latency" && c.status == "down") cs
