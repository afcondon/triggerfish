-- | Balistes on its own page (`balistes.html`), in the standalone shell
-- | (`Triggerfish.Standalone`). Its kit lanes are routed on the dashboard.
-- |
-- | Bundle: `spago bundle --module Triggerfish.Balistes.Main --outfile public/balistes.js`.
module Triggerfish.Balistes.Main (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Triggerfish.Balistes.Component as Balistes
import Triggerfish.Standalone as Standalone
import Triggerfish.Transport (Which(..))

main :: Effect Unit
main = Standalone.run
  { which: Bal
  , nameplate: "Triggerfish · Model Balistes"
  , component: Balistes.component
  , chipOf: case _ of
      Balistes.IdentityChanged cv -> Just cv
      Balistes.LaneEdited _ -> Nothing
  , armOf: const Nothing
  , markOf: const Nothing
  , playable: true
  , spaceHears: false
  , capturable: true
  }
