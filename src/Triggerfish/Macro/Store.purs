-- | `Triggerfish.Macro.Store` — localStorage persistence for the macro-tidal
-- | per-machine lanes (the Tidal-like sequencer). Shell-level, like the scene
-- | store: a lane is a mini-notation string keyed by its machine's lane tag
-- | (`odo`/`bal`/`sel`/`vet`), plus the shared bars-per-step. All strings +
-- | one int — the same lightweight-envelope discipline as the other stores.
module Triggerfish.Macro.Store
  ( Saved
  , save
  , load
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)

-- | What we persist: each lane as { machine-tag, text }, and bars-per-step.
type Saved =
  { lanes :: Array { machine :: String, text :: String }
  , bars :: Int
  }

storeKey :: String
storeKey = "triggerfish.macro.v1"

foreign import _save :: String -> String -> Effect Unit
foreign import _load :: forall a. String -> Effect (Nullable a)
foreign import _stringify :: forall a. a -> String

-- | Persist the lanes + bars (best-effort — the FFI swallows quota / private-mode).
save :: Saved -> Effect Unit
save s = _save storeKey (_stringify s)

-- | Load the stored lanes, or `Nothing` if absent / unparseable.
load :: Effect (Maybe Saved)
load = map toMaybe (_load storeKey)
