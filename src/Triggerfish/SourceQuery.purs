-- | The queries every instrument answers for the shell:
-- |
-- |   * `AskSource` — "hand me your current source as a string" (the TIDAL tab
-- |     aggregate; pull-based, asked when the tab opens / on refresh).
-- |   * `SyncFree start tempo` — "adopt this shared free-run baseline (start
-- |     micros, BPM)", so the modules share one downbeat with no rig (the
-- |     laptop-only analogue of all locking to one Link anchor). Modules with
-- |     no clock (Selene's skeleton) ignore it.
-- |
-- | (Vetula, vendored from its own standalone app, defines its own structurally
-- | similar source query — see Vetula.App.SourceQuery — and isn't on this clock.)
module Triggerfish.SourceQuery (Query(..)) where

data Query a
  = AskSource (String -> a)
  | SyncFree Number Number a
  | FeedChords (Array (Array Int)) a
