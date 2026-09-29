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

import Data.Array (deleteAt, length)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Triggerfish.Scenes (Scene)

-- | What we persist: the whole scene grid, in order.
type Saved = { scenes :: Array Scene }

-- | The on-disk shape: each cell an alias string, `""` = leave-as-is; each scene's
-- | `name` a string, `""` = unnamed.
type Envelope = { scenes :: Array { name :: String, cells :: Array String } }

-- | v2 has three columns (Odonus, Selene, Vetula). v1 had four, Balistes second.
storeKey :: String
storeKey = "triggerfish.scenes.v2"

legacyKey :: String
legacyKey = "triggerfish.scenes.v1"

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

-- | Load the grid, or `Nothing` if absent / unparseable. A grid saved only under
-- | v1 is read with each scene's Balistes cell (column 1) dropped, so the other
-- | machines keep their presets; it is written back as v2 on the next save, and
-- | v1 is left as it was.
load :: Effect (Maybe Saved)
load = do
  mEnv <- toMaybe <$> (_load storeKey :: Effect (Nullable Envelope))
  case mEnv of
    Just env -> pure (Just (decode identity env))
    Nothing -> do
      mOld <- toMaybe <$> (_load legacyKey :: Effect (Nullable Envelope))
      pure (decode dropBalistes <$> mOld)
  where
  decode f env = { scenes: map (decodeScene <<< f) env.scenes }
  dropBalistes e =
    if length e.cells == 4 then e { cells = fromMaybe e.cells (deleteAt 1 e.cells) } else e
  decodeScene e =
    { name: if e.name == "" then Nothing else Just e.name
    , cells: map (\a -> if a == "" then Nothing else Just a) e.cells
    }
