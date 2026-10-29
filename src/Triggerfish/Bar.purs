-- | **The page's own controls in the shared bar** (AC, 2026-10-05): a machine's
-- | tabs, its chips, its dropdowns and ⓘ (and a ◆ mark and a rebus, which no
-- | machine fills since marking moved to the river), drawn by the shell
-- | (`Triggerfish.Standalone`) in its top bar,
-- | as the drawer's rows are (`Triggerfish.Browser`). The machine says what to
-- | show (`SourceQuery.AskBar`) and is told what was pressed
-- | (`SourceQuery.BarAction`): `stage:ID`, `mark`, `clear`, `rebus`, `chip:ID`,
-- | `pick:ID:VALUE`, `help`, and `drop:ID TEXT` (a drag of a kind the tab
-- | `accepts` let go on it, TEXT being the drag's `<kind> <payload>`). A machine
-- | that answers nothing has none of these in the bar.
module Triggerfish.Bar
  ( Bar
  , Tab
  , TabLook(..)
  , Chip
  , Picker
  ) where

import Prelude

import Halogen.Widgets.Select as Select
import Triggerfish.Glyph (GlyphIcon)

-- | A view. `enabled` false draws it dimmed and unpressable, its tip saying
-- | why (Vetula's score with no progression, 2026-10-27). `look` is how it
-- | is lit while active: the view's own design language. `accepts`: the
-- | drag kinds (`Triggerfish.Drag`) it takes dropped on it, dimmed or not:
-- | a progression dropped on Vetula's score opens it there (AC, 2026-10-29).
type Tab = { id :: String, label :: String, active :: Boolean, enabled :: Boolean, look :: TabLook, tip :: String, accepts :: Array String }

-- | `Warm`: the workspace's gold (Vetula's lattice). `Clay`: a terracotta
-- | beside it (Vetula's banks). `Ink`: black on white, as an engraved
-- | score (Vetula's score). `Night`: white on black, for a view that is
-- | mostly black (Vetula's review river). AC, 2026-10-27.
data TabLook = Warm | Clay | Ink | Night

derive instance eqTabLook :: Eq TabLook

-- | `marks`: the counts beside ◆ mark ("" for no mark controls in this
-- | stage). `icons`: the rebus of what is loaded, `rebusTip` its name.
-- | `chips`: small things the page wants on show at every stage (Vetula's
-- | progression, 2026-10-06); a press arrives as `chip:ID`. `pickers`: its
-- | dropdowns, at the right; `help`: the ⓘ's tip ("" for none), pressed as
-- | `help`; `helpPulse`: the ⓘ pulses once, to say where help lives (as
-- | Vetula's welcome card closes).
type Bar =
  { tabs :: Array Tab
  , marks :: String
  , icons :: Array GlyphIcon
  , rebusTip :: String
  , chips :: Array Chip
  , pickers :: Array Picker
  , help :: String
  , helpPulse :: Boolean
  }

-- | A dropdown at the bar's right, before Panic (Vetula's key and scale,
-- | 2026-10-08): the shell draws it, and a choice arrives as `pick:ID:VALUE`.
type Picker = { id :: String, input :: Select.Input }

-- | A chip: a label, with a rebus before it if it has one. `active` draws it
-- | lit; `attention` draws it asking to be pressed (an unsaved progression).
type Chip = { id :: String, label :: String, tip :: String, icons :: Array GlyphIcon, active :: Boolean, attention :: Boolean }
