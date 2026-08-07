-- | `Triggerfish.Transport.Store` — localStorage persistence for the shell's
-- | authority `Mode` (Solo ⟷ Atlantis), and for that alone.
-- |
-- | Why only the mode. The shell's transport state is `mode` + `armed`
-- | (see `Triggerfish.Transport`), and the two are different KINDS of fact:
-- |
-- |   * `mode` is a PREFERENCE — how this browser relates to the rig. The
-- |     user picks it, nothing on the backend can contradict it, and
-- |     restoring it is simply remembering a choice. Safe on load too:
-- |     Atlantis mutes local emit, so coming up in Atlantis makes no sound
-- |     until something is published.
-- |
-- |   * `armed` is a CLAIM ABOUT WHAT IS RUNNING, and its truth lives in the
-- |     BEAM. Restoring it across a reload — especially across a BEAM restart,
-- |     which wipes every voice — would have the UI assert something false.
-- |     That is the desync that makes a dead rig look live, so `armed` is
-- |     deliberately NOT persisted and always boots empty.
-- |
-- | Same discipline as the sibling stores, which restore the scene grid and
-- | the macro lanes but never `sceneRun` / `macroOn`: persist what the user
-- | chose, never what was playing.
module Triggerfish.Transport.Store
  ( Saved
  , save
  , load
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)

import Triggerfish.Transport (Mode(..))

-- | The stored envelope. A tagged string rather than the `Mode` itself —
-- | localStorage is a wire, and an unknown tag from an older/newer build
-- | must degrade to `Nothing` rather than decode into the wrong authority.
type Saved = { mode :: String }

storeKey :: String
storeKey = "triggerfish.transport.v1"

foreign import _save :: String -> String -> Effect Unit
foreign import _load :: forall a. String -> Effect (Nullable a)
foreign import _stringify :: forall a. a -> String

toTag :: Mode -> String
toTag = case _ of
  Solo -> "solo"
  Atlantis -> "atlantis"

fromTag :: String -> Maybe Mode
fromTag = case _ of
  "solo" -> Just Solo
  "atlantis" -> Just Atlantis
  _ -> Nothing

-- | Persist the authority mode (best-effort — the FFI swallows quota /
-- | private-mode, as the sibling stores do).
save :: Mode -> Effect Unit
save m = _save storeKey (_stringify { mode: toTag m })

-- | Load the stored mode, or `Nothing` if absent / unparseable / unknown tag.
-- | The caller keeps its own default (Solo) for that case.
load :: Effect (Maybe Mode)
load = do
  ms <- map toMaybe (_load storeKey)
  pure case ms of
    Nothing -> Nothing
    Just (s :: Saved) -> fromTag s.mode
