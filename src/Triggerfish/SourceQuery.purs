-- | The one query every instrument answers so the shell can build the TIDAL
-- | tab: "hand me your current source as a string". Pull-based — the shell asks
-- | when it needs the aggregate (on opening the tab / a refresh), rather than
-- | every module pushing on every edit. (Vetula, being vendored from its own
-- | standalone app, defines a structurally-identical query in its own namespace
-- | rather than importing this — see Vetula.App.SourceQuery.)
module Triggerfish.SourceQuery (Query(..)) where

data Query a = AskSource (String -> a)
