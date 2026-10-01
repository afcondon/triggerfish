-- | Balistes on its own page (`balistes.html`), in the standalone shell
-- | (`Triggerfish.Standalone`). Its router shows the sixteen kit lanes.
-- |
-- | Bundle: `spago bundle --module Triggerfish.Balistes.Main --outfile public/balistes.js`.
module Triggerfish.Balistes.Main (main) where

import Prelude

import Data.Array ((..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Triggerfish.Balistes.Component as Balistes
import Triggerfish.Routing.Model as RM
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
  , router: Just
      { title: "Routing · Balistes kit"
      , note: "shared with Triggerfish's router; sample legs sound in Atlantis only"
      , sources: map RM.SDrumLane (0 .. 15)
      , restoreLabel: "restore default kit routing"
      }
  , armOf: const Nothing
  , follow: const Nothing
  }
