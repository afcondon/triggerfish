-- | `Triggerfish.Odonus.RouteStore` — localStorage persistence for Odonus's
-- | per-head rig routing (`VoiceCfg`: which FH-2 polyenv envelopes each head
-- | fires).
-- |
-- | Separate from `Triggerfish.Odonus.Store` on purpose. That store holds the
-- | MUSICAL artefact — the live patch, the scene library, the preset bank, each
-- | as Lepidoptera text you can hand to Calypso or ship to the BEAM. Routing is
-- | rig-facing PLACEMENT: it says which socket the sound comes out of, and a
-- | patch carried to another rig should not drag this along. Same split that
-- | keeps `VoiceCfg` out of `Reef.Odonus.Head`.
-- |
-- | Persisted at all because it is a PREFERENCE — the user's decision about
-- | where a head's envelope trigger goes, which nothing on the backend can
-- | contradict. Nothing here is a claim about what is currently playing.
-- |
-- | **This store is temporary.** Step 4 of `docs/DESIGN-routing.md` consolidates
-- | every routing fact (the shell's `routing` / `audition`, Selene's targets, and
-- | this) into one table with one store. It exists now only so envelope
-- | assignments survive the reloads that testing involves.
module Triggerfish.Odonus.RouteStore
  ( save
  , load
  ) where

import Prelude

import Data.Array (filter, nub)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)

import Triggerfish.Odonus.Grid.Types (VoiceCfg)

-- | The stored envelope: one `envs` array per head, in head order.
type Saved = { voices :: Array { envs :: Array Int } }

storeKey :: String
storeKey = "triggerfish.odonus.route.v1"

foreign import _save :: String -> String -> Effect Unit
foreign import _load :: forall a. String -> Effect (Nullable a)
foreign import _stringify :: forall a. a -> String

-- | Persist the head routing (best-effort — the FFI swallows quota / private
-- | mode, as the sibling stores do).
save :: Array VoiceCfg -> Effect Unit
save vs = _save storeKey (_stringify { voices: map (\v -> { envs: v.envs }) vs })

-- | Load the stored routing, or `Nothing` if absent / unparseable.
-- |
-- | Slots are re-clamped to 1..8 on the way in rather than trusted: localStorage
-- | is a wire, and an out-of-range slot would address a MIDI channel no polyenv
-- | listens on — a note going somewhere unintended, which is worse than a
-- | dropped assignment. The caller keeps its own default for `Nothing`, and
-- | tolerates an array shorter or longer than its head count.
load :: Effect (Maybe (Array VoiceCfg))
load = do
  ms <- map toMaybe (_load storeKey)
  pure case ms of
    Nothing -> Nothing
    Just (s :: Saved) -> Just (map clean s.voices)
  where
  clean v = { envs: nub (filter (\n -> n >= 1 && n <= 8) v.envs) }
