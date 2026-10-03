-- | Odonus on its own page (`odonus.html`), in the standalone shell
-- | (`Triggerfish.Standalone`). Its router shows the four heads.
-- |
-- | What it quantises to is the harmony routes' business (`routing/harmony`,
-- | edited on the dashboard, applied on the rig by `odonus_feeds`); the page
-- | takes nothing from Vetula's tab directly.
-- |
-- | Bundle: `spago bundle --module Triggerfish.Odonus.Main --outfile public/odonus.js`.
module Triggerfish.Odonus.Main (main) where

import Prelude

import Data.Array ((..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Triggerfish.Odonus.Grid as Odonus
import Triggerfish.Routing.Model as RM
import Triggerfish.Standalone as Standalone
import Triggerfish.Transport (Which(..))

main :: Effect Unit
main = Standalone.run
  { which: Odo
  , nameplate: "Triggerfish · Model Odonus"
  , component: Odonus.component
  , chipOf: case _ of
      Odonus.IdentityChanged cv -> Just cv
      Odonus.StageChanged _ -> Nothing
  , router: Just
      { title: "Routing · Odonus heads"
      , note: "shared with every page's router"
      , sources: map RM.SOdonusHead (0 .. 3)
      , restoreLabel: "restore default head routing"
      }
  , armOf: const Nothing
  }
