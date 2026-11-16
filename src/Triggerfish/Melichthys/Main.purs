-- | Melichthys on its own page (`melichthys.html`): Odonus's heads over one
-- | long drawn line (`Reef.Melichthys`), in the standalone shell.
-- |
-- | It is Odonus's own component being a second machine
-- | (`Triggerfish.Odonus.Grid`, `Line`): the same heads, scope, key, feel,
-- | presets and capture, with the line where the grids are, and its own place
-- | in this browser's storage. The rig has no voice for it yet, so the page
-- | plays it in both modes, through the routes Odonus's heads have.
-- |
-- | Bundle: `spago bundle --module Triggerfish.Melichthys.Main --outfile public/melichthys.js`.
module Triggerfish.Melichthys.Main (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Triggerfish.Odonus.Grid as Odonus
import Triggerfish.Standalone as Standalone
import Triggerfish.Transport (Which(..))

main :: Effect Unit
main = Standalone.run
  { which: Mel
  , nameplate: "Triggerfish · Model Melichthys"
  , component: Odonus.component Odonus.Line
  , chipOf: case _ of
      Odonus.IdentityChanged cv -> Just cv
      Odonus.StageChanged _ -> Nothing
      Odonus.Marked _ -> Nothing
  , armOf: const Nothing
  , playable: true
  , spaceHears: false
  , capturable: false
  , limulusOf: const false
  , rigOnly: false
  , markOf: case _ of
      Odonus.Marked at -> Just at
      _ -> Nothing
  }
