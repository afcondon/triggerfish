-- | **Vetula's cards in the routing table, and the table on the stage.** A
-- | card is a source like an Odonus head (`RM.cardSource`, by channel); its
-- | routes are played by the rig (`vetula_cards`), which reads them from the
-- | stage object `vetula/routing`. Whoever shows the cards' routes (the
-- | dashboard's Notes matrix, Vetula's own router) keeps the two in step:
-- | a row for every card on the stage, and the table's Vetula part written
-- | back whenever it changes.
module Triggerfish.Routing.VetulaSync
  ( cardChannels
  , sync
  , stageLine
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Map as Map
import Data.String (Pattern(..), stripPrefix)
import Data.String as String
import Reef.Routing as RR
import Triggerfish.Router as Router
import Triggerfish.Routing.Model as RM
import Triggerfish.Routing.Out as RO

-- | The channels of the cards on the stage (a card's line starts `chN`).
cardChannels :: Router.Router -> Array Int
cardChannels r = Array.sort (Array.nub (Array.mapMaybe channel (Array.fromFoldable (Map.values r.cards))))
  where
  channel line = Array.head (String.split (Pattern " ") (String.trim line)) >>= stripPrefix (Pattern "ch") >>= Int.fromString

-- | The table with a row for every card (each new one on its own channel of
-- | the default port), and the Vetula routing it gives, as the stage keeps it.
sync :: Array String -> Router.Router -> RM.Table -> { table :: RM.Table, json :: String }
sync ports r tbl =
  let table = RM.seedCards ports (cardChannels r) tbl
  in { table, json: RR.encodeVoiceRouting (RO.vetulaRouting ports table) }

stageLine :: String -> String
stageLine json = "stage-text vetula/routing " <> json
