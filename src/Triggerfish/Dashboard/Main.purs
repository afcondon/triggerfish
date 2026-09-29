-- | The dashboard's page (`dashboard.html`).
module Triggerfish.Dashboard.Main (main) where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Triggerfish.Dashboard as Dashboard

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  void $ runUI Dashboard.component unit body
