-- | **The stage slot of a Triggerfish machine**: what it has loaded and whether
-- | it is sounding, recorded on the rig so other pages can see it.
-- |
-- | The rig's stage (purerl-tidal's `tidal_stage`, `docs/kb/plans/the-stage.md`)
-- | holds one entry per instrument and announces every change to its
-- | subscribers. Conspicillum's entry rides on its scene push. A Triggerfish
-- | machine's sound is not one scene push, so its shell records the entry
-- | directly with `stage-put <slot> <json>`. The dashboard is the reader it is
-- | for: it draws each machine's chip, edited or clean, and whether it plays.
-- |
-- | The chip travels as its Rebus alias, which any page can turn back into the
-- | icons (`Rebus.rebusFromAnyAlias`). There is no `base` yet: the banks are in
-- | localStorage, not Amphora.
module Triggerfish.Stage
  ( slotOf
  , putLine
  ) where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (toNullable)
import Simple.JSON (writeJSON)
import Triggerfish.Glyph (ChipView)
import Triggerfish.Transport (Which(..))

-- | The machine's slot on the stage, or `Nothing` for one that has none. The
-- | rig accepts only these names.
slotOf :: Which -> Maybe String
slotOf = case _ of
  Odo -> Just "odonus"
  Vet -> Just "vetula"
  Bal -> Just "balistes"
  Sel -> Just "selene"
  Tid -> Nothing
  Suf -> Nothing

-- | The `stage-put` line recording a machine's chip and whether it sounds.
-- | Shells send it only when it differs from the last one sent, so an
-- | unchanged machine costs the wire nothing.
putLine :: String -> Maybe ChipView -> Boolean -> String
putLine slot chip playing =
  "stage-put " <> slot <> " "
    <> writeJSON
      { alias: toNullable (map _.glyph.alias chip)
      , edited: maybe false _.diverged chip
      , playing
      }
