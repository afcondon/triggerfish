-- | The Selene rack on its own page (`selene.html`), in the standalone shell
-- | (`Triggerfish.Standalone`). No router: each destination's target is set in
-- | the rack itself, and Selene does not read the routing table.
-- |
-- | Selene has no rig voice, so it sounds Local in Solo and Atlantis alike: it
-- | drives the ES-9 and FH-2 from the browser either way.
-- |
-- | Bundle: `spago bundle --module Triggerfish.Selene.Main --outfile public/selene.js`.
module Triggerfish.Selene.Main (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Triggerfish.Selene.Component as Selene
import Triggerfish.Standalone as Standalone
import Triggerfish.Transport (Which(..))

main :: Effect Unit
main = Standalone.run
  { which: Sel
  , nameplate: "Triggerfish · Selene rack"
  , component: Selene.component
  , chipOf: \(Selene.IdentityChanged cv) -> Just cv
  , router: Nothing
  , armOf: const Nothing
  }
