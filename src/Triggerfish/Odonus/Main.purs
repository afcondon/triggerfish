-- | Odonus on its own page (`odonus.html`), in the standalone shell
-- | (`Triggerfish.Standalone`). Its heads are routed on the dashboard.
-- |
-- | What it quantises to is the harmony routes' business (`routing/harmony`,
-- | edited on the dashboard, applied on the rig by `odonus_feeds`); the page
-- | takes nothing from Vetula's tab directly.
-- |
-- | Bundle: `spago bundle --module Triggerfish.Odonus.Main --outfile public/odonus.js`.
module Triggerfish.Odonus.Main (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Triggerfish.Odonus.Grid as Odonus
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
      Odonus.Marked _ -> Nothing
  , armOf: const Nothing
  , playable: true
  , capturable: false
  , markOf: case _ of
      Odonus.Marked at -> Just at
      _ -> Nothing
  }
