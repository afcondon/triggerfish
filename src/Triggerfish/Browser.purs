-- | **The browser**: what a machine keeps, as the drawer on the left of its
-- | page shows it (docs/kb/plans/the-deck.md, 2026-10-05: the suite's one
-- | layout, browser | instrument | Limulus, after Ableton's browser).
-- |
-- | The shared bar (`Triggerfish.Standalone`) owns the drawer and asks the
-- | machine for its rows (`SourceQuery.AskBrowser`); a machine that answers
-- | nothing has no drawer. Rows lead with a **name**: a rebus tells a handful
-- | apart at a glance and never labels a library (AC), so the glyph is an
-- | accent beside the name.
module Triggerfish.Browser
  ( Browser
  , Item
  , Recall
  , defaultRecall
  , stamp
  ) where

import Effect (Effect)
import Triggerfish.Glyph (GlyphIcon)

-- | `modes`: the machine recalls in four ways (`Recall`), so each row carries
-- | the 2×2 square and the drawer its key (Odonus). `keep`: what the drawer's
-- | keep button says (`BrowserKeep`): "keep" for a preset, "save scene" for
-- | Vetula, whose `c` captures something else.
-- | `notice`: a word from the machine after something it can take back (a
-- | rack replaced), shown with an undo (`BrowserUndo`); "" for none.
type Browser = { title :: String, modes :: Boolean, keep :: String, notice :: String, rows :: Array Item }

-- | One kept thing. `slot` is the machine's own index for it; `icons` its
-- | rebus, as the machine draws it (a preset's coloured pair; Vetula's
-- | session triple in one colour); `tag` what kind it is, shown beside the
-- | name (Balistes's Grids or Rytm); `current` whether it is what the machine
-- | has loaded now.
-- | `section`: the heading it is listed under ("" for none; rows of a
-- | section are kept together, in the order sections first appear).
-- | `builtin`: not the user's (a Selene block): not renamed. `drag`: what a
-- | drag of it carries (a Selene module's line), "" for none.
type Item =
  { slot :: Int, name :: String, icons :: Array GlyphIcon, tag :: String, current :: Boolean
  , section :: String, builtin :: Boolean, drag :: String
  -- | what can be done to it, shown on hover (`BrowserAction`); "delete"
  -- | asks first
  , actions :: Array String }

-- | How to recall it (Odonus): `frozen`, its generators paused; `inKey`,
-- | keeping the key the machine is in now rather than the saved one. A machine
-- | without modes ignores both.
type Recall = { frozen :: Boolean, inKey :: Boolean }

defaultRecall :: Recall
defaultRecall = { frozen: false, inKey: false }

-- | The time now, `HH:MM`: a capture is named "Odonus · 21:14" until renamed.
foreign import stamp :: Effect String
