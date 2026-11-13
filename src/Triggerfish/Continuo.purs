-- | **What Continuo is hosting**: each instrument and the MIDI channel it
-- | listens on, as Continuo itself says (its `status`, relayed by the Friends
-- | server at `/api/continuo`, since a page cannot open its control socket).
-- | The routing matrix draws a column for each instrument from this, so a
-- | voice can only be sent where something is listening.
module Triggerfish.Continuo (Layer, Mix(..), layers, mix, split, channels, instrumentOn) where

import Prelude

import Data.Array (all, filter, nub, null, sort)
import Data.Either (Either(..))
import Data.String (joinWith)
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)

-- | `channel` is 1..16, or 0 for an instrument that plays every channel.
-- | `levelDb` is its level in Continuo's mix (0 is the instrument's own) and
-- | `pan` its place, -1 left to 1 right.
type Layer = { name :: String, channel :: Int, levelDb :: Number, pan :: Number }

-- | One change to an instrument's place in the mix.
data Mix = Level Number | Pan Number

foreign import layersImpl :: String -> (Array Layer -> Effect Unit) -> Effect Unit
foreign import mixImpl :: forall body. String -> body -> (Array Layer -> Effect Unit) -> Effect Unit

-- | Set the level or pan of the instrument at `layer` (1 is the first).
-- | Answers with the instruments as they now are; none if it did not take.
mix :: Int -> Mix -> Aff (Array Layer)
mix layer change = makeAff \done -> do
  case change of
    Level db -> mixImpl "/api/continuo/mix" { layer, levelDb: db } (done <<< Right)
    Pan p -> mixImpl "/api/continuo/mix" { layer, pan: p } (done <<< Right)
  pure nonCanceler

-- | Every instrument, or none when Continuo does not answer.
layers :: Aff (Array Layer)
layers = makeAff \done -> do
  layersImpl "/api/continuo" (done <<< Right)
  pure nonCanceler

-- | Whether every instrument has a channel of its own, so a route can name an
-- | instrument. False when Continuo is not answering, or when an instrument
-- | plays every channel, where any channel sounds and the channel is the route.
split :: Array Layer -> Boolean
split ls = not (null ls) && all (\l -> l.channel >= 1) ls

-- | The channels something listens on, in order.
channels :: Array Layer -> Array Int
channels ls = sort (nub (map _.channel ls))

-- | What plays a channel: one instrument, or several layered on it.
instrumentOn :: Array Layer -> Int -> String
instrumentOn ls ch = joinWith " + " (map _.name (filter (\l -> l.channel == ch) ls))
