-- | The always-on performance logbook (#151) now lives, machine-agnostically, in
-- | `Triggerfish.Capture.Logbook` (#28) — every capturing machine shares one
-- | engine. This module re-exports it under its historical Odonus name so existing
-- | importers (Scope, Scenes, Grid) are unchanged.
module Triggerfish.Odonus.Logbook (module Triggerfish.Capture.Logbook) where

import Triggerfish.Capture.Logbook
