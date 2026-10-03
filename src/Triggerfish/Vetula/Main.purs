-- | Vetula on its own page (`vetula.html`), in the standalone shell
-- | (`Triggerfish.Standalone`), through `Triggerfish.Vetula.Page`, which fits
-- | Vetula to the shell and sends its harmonic context out on the tab bus for
-- | Odonus to follow.
-- |
-- | Its router shows the cards, one row a channel, routed as Odonus's heads
-- | are; the rig plays them through those routes (`vetula_cards`), so this
-- | page keeps them on the stage as the dashboard does.
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
  , router: Just
      { title: "Routing · Vetula cards"
      , note: "shared with every page's router"
      , sources: []
      , cards: true
      , restoreLabel: "restore default card routing"
      }
  , armOf: case _ of
      Page.Armed on -> Just on
      Page.Chip _ -> Nothing
  }
