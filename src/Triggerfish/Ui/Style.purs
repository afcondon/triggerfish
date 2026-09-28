-- | The two styling primitives every machine's view uses: the `style`
-- | attribute, and `engrave`, the engraved-label type of the Hainbach panels.
-- |
-- | They lived in `Triggerfish.Odonus.Grid.Widgets`, which made Balistes,
-- | Selene and Sufflamen depend on Odonus for a string, and left five other
-- | modules keeping private copies to avoid that. Here they depend on nothing,
-- | so a machine can leave the page without taking Odonus along.
module Triggerfish.Ui.Style
  ( style
  , engrave
  ) where

import Halogen as H
import Halogen.HTML.Properties as HP

style :: forall r i. String -> HP.IProp r i
style = HP.attr (H.AttrName "style")

engrave :: String
engrave = "font-family:Georgia,'Times New Roman',serif;letter-spacing:0.12em;text-transform:uppercase;color:#5a564b"
