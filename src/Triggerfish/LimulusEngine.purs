-- | Which engine Limulus sends Tidal to, "architeuthis" or "ghci": a choice
-- | made on the Dashboard (and in Limulus), kept in the origin's storage.
module Triggerfish.LimulusEngine (load, save, onChange) where

import Prelude

import Effect (Effect)

foreign import load :: Effect String
foreign import save :: String -> Effect Unit
foreign import onChange :: (String -> Effect Unit) -> Effect Unit
