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

import Data.Array (catMaybes, head, last, length)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Triggerfish.Scenes (Scene)

-- | What we persist: the whole scene grid, in order.
type Saved = { scenes :: Array Scene }

-- | The on-disk shape: each cell an alias string, `""` = leave-as-is; each scene's
-- | `name` a string, `""` = unnamed.
type Envelope = { scenes :: Array { name :: String, cells :: Array String } }

-- | v3 has two columns (Odonus, Vetula). v2 had three (Odonus, Selene, Vetula);
-- | v1 four (Odonus, Balistes, Selene, Vetula).
storeKey :: String
storeKey = "triggerfish.scenes.v3"

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

-- | Load the grid, or `Nothing` if absent / unparseable. A grid saved under an
-- | older key is read with the departed machines' cells dropped, so Odonus and
-- | Vetula keep their presets; it is written back as v3 on the next save, and the
-- | older key is left as it was.
load :: Effect (Maybe Saved)
load = do
  v3 <- older storeKey
  v2 <- older "triggerfish.scenes.v2"
  v1 <- older "triggerfish.scenes.v1"
  pure case v3, v2, v1 of
    Just env, _, _ -> Just (decode (keep 2) env)
    _, Just env, _ -> Just (decode (keep 3) env)
    _, _, Just env -> Just (decode (keep 4) env)
    _, _, _ -> Nothing
  where
  older key = toMaybe <$> (_load key :: Effect (Nullable Envelope))
  decode f env = { scenes: map (decodeScene <<< f) env.scenes }
  -- Odonus is always the first column and Vetula the last, so a grid of `n`
  -- columns keeps those two. A scene of any other width is kept as it is.
  keep n e =
    if n == 2 || length e.cells /= n then e
    else e { cells = catMaybes [ head e.cells, last e.cells ] }
  decodeScene e =
    { name: if e.name == "" then Nothing else Just e.name
    , cells: map (\a -> if a == "" then Nothing else Just a) e.cells
    }
