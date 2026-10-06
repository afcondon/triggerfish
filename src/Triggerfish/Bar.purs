-- | **The page's own controls in the shared bar** (AC, 2026-10-05): a machine's
-- | stage tabs, its ◆ mark with the counts and clear, and the rebus of what it
-- | has loaded, drawn by the shell (`Triggerfish.Standalone`) in its top bar,
-- | as the drawer's rows are (`Triggerfish.Browser`). The machine says what to
-- | show (`SourceQuery.AskBar`) and is told what was pressed
-- | (`SourceQuery.BarAction`): `stage:ID`, `mark`, `clear`, `rebus`, `chip:ID`. A machine
-- | that answers nothing has none of these in the bar.
module Triggerfish.Bar
  ( Bar
  , Tab
  ) where

import Triggerfish.Glyph (GlyphIcon)

type Tab = { id :: String, label :: String, active :: Boolean, tip :: String }

-- | `marks`: the counts beside ◆ mark ("" for no mark controls in this
-- | stage). `icons`: the rebus of what is loaded, `rebusTip` its name.
-- | `chips`: small settings the page wants on show at every stage (Vetula's
-- | sound, 2026-10-06); a press arrives as `chip:ID`.
type Bar =
  { tabs :: Array Tab
  , marks :: String
  , icons :: Array GlyphIcon
  , rebusTip :: String
  , chips :: Array Tab
  }
