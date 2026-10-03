-- | Vetula on its own page (`vetula.html`), in the standalone shell
-- | (`Triggerfish.Standalone`), through `Triggerfish.Vetula.Page`, which fits
-- | Vetula to the shell. Its cards are routed on the dashboard (the Notes
-- | matrix), which keeps their routes on the stage for the rig.
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
      Page.Marked _ -> Nothing
  , armOf: case _ of
      Page.Armed on -> Just on
      Page.Chip _ -> Nothing
      Page.Marked _ -> Nothing
  , markOf: case _ of
      Page.Marked at -> Just at
      _ -> Nothing
  }
