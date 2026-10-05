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
  , Row
  , Recall
  , defaultRecall
  , stamp
  ) where

import Effect (Effect)

-- | `modes`: the machine recalls in four ways (`Recall`), so each row carries
-- | the 2×2 square and the drawer its key (Odonus).
type Browser = { title :: String, modes :: Boolean, rows :: Array Row }

-- | One kept thing. `slot` is the machine's own index for it; `alias` its
-- | rebus; `tag` what kind it is, shown beside the name (Balistes's Grids or
-- | Rytm); `current` whether it is what the machine has loaded now.
type Row = { slot :: Int, name :: String, alias :: String, tag :: String, current :: Boolean }

-- | How to recall it (Odonus): `frozen`, its generators paused; `inKey`,
-- | keeping the key the machine is in now rather than the saved one. A machine
-- | without modes ignores both.
type Recall = { frozen :: Boolean, inKey :: Boolean }

defaultRecall :: Recall
defaultRecall = { frozen: false, inKey: false }

-- | The time now, `HH:MM`: a capture is named "Odonus · 21:14" until renamed.
foreign import stamp :: Effect String
