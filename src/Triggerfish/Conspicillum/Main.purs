-- | Conspicillum on its own page (`conspicillum/index.html`, served at
-- | `/conspicillum/`), in the standalone shell (`Triggerfish.Standalone`),
-- | as the machines are (2026-10-29; it was a repo and a page of its own).
-- |
-- | Bundle: `spago bundle --module Triggerfish.Conspicillum.Main --outfile public/conspicillum.js`.
module Triggerfish.Conspicillum.Main (main) where

import Prelude

import Conspicillum.App as Conspicillum
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Triggerfish.Standalone as Standalone
import Triggerfish.Transport (Which(..))

main :: Effect Unit
main = Standalone.run
  { which: Con
  , nameplate: "Triggerfish · Model Conspicillum"
  , component: Conspicillum.component
  , chipOf: const Nothing
  , armOf: case _ of
      Conspicillum.Armed on -> Just on
      _ -> Nothing
  , markOf: const Nothing
  , playable: true
  , spaceHears: false
  , capturable: true
  , limulusOf: case _ of
      Conspicillum.WantsLimulus -> true
      _ -> false
  , rigOnly: true
  }
