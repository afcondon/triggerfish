-- | Vetula on its own page (`vetula.html`), in the standalone shell
-- | (`Triggerfish.Standalone`), through `Triggerfish.Vetula.Page`, which fits
-- | Vetula to the shell and sends its harmonic context out on the tab bus for
-- | Odonus to follow.
-- |
-- | No router: Vetula does not read the routing table.
-- |
-- | Bundle: `spago bundle --module Triggerfish.Vetula.Main --outfile public/vetula.js`.
module Triggerfish.Vetula.Main (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Triggerfish.Standalone as Standalone
import Triggerfish.Transport (Which(..))
import Triggerfish.Vetula.Page as Page

main :: Effect Unit
main = Standalone.run
  { which: Vet
  , nameplate: "Triggerfish · Model Vetula"
  , component: Page.component
  , chipOf: case _ of
      Page.Chip cv -> Just cv
      Page.Armed _ -> Nothing
  , router: Nothing
  , armOf: case _ of
      Page.Armed on -> Just on
      Page.Chip _ -> Nothing
  }
