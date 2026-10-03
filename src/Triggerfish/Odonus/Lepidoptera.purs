-- | Triggerfish.Odonus.Lepidoptera — the eDSL print/parse for an Odonus
-- | **patch**, the whole authored setup. Since 2026-10-03 it lives in reef
-- | (`Reef.Odonus.Patch`), one source for the page and the rig, so a patch
-- | evaluated in Limulus (`odonus $ odonusPatch "live" { … }`) is read on the
-- | BEAM exactly as the page writes it; this module keeps the old name.
module Triggerfish.Odonus.Lepidoptera
  ( module Reef.Odonus.Patch
  ) where

import Reef.Odonus.Patch (OdonusPatch, printPatch, parsePatch)
