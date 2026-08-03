-- | `Triggerfish.Scenes.Store` — localStorage persistence for the rig-wide SCENE
-- | grid. Scenes span every machine, so unlike the per-machine preset stores this
-- | one is shell-level (`triggerfish.scenes`). A scene cell is a glyph **alias**
-- | (the transferable content identity); the on-disk envelope encodes an
-- | leave-as-is cell as the empty string, mirroring the preset stores' "empty name
-- | = anonymous" convention — so it's all strings, no bespoke codecs.
module Triggerfish.Scenes.Store
  ( Saved
  , save
  , load
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Triggerfish.Scenes (Scene)

-- | What we persist: the whole scene grid, in order.
type Saved = { scenes :: Array Scene }

-- | The on-disk shape: each cell an alias string, `""` = leave-as-is; each scene's
-- | `name` a string, `""` = unnamed.
type Envelope = { scenes :: Array { name :: String, cells :: Array String } }

storeKey :: String
storeKey = "triggerfish.scenes.v1"

foreign import _save :: String -> String -> Effect Unit
foreign import _load :: forall a. String -> Effect (Nullable a)
foreign import _stringify :: forall a. a -> String

-- | Persist the grid (best-effort — the FFI swallows quota / private-mode errors).
save :: Saved -> Effect Unit
save s = _save storeKey (_stringify env)
  where
  env :: Envelope
  env = { scenes: map encodeScene s.scenes }
  encodeScene sc =
    { name: fromMaybe "" sc.name
    , cells: map (fromMaybe "") sc.cells
    }

-- | Load the grid, or `Nothing` if absent / unparseable.
load :: Effect (Maybe Saved)
load = do
  mEnv <- _load storeKey
  pure $ toMaybe (mEnv :: Nullable Envelope) <#> \env ->
    { scenes: map decodeScene env.scenes }
  where
  decodeScene e =
    { name: if e.name == "" then Nothing else Just e.name
    , cells: map (\a -> if a == "" then Nothing else Just a) e.cells
    }
