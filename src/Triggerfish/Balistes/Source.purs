-- | Triggerfish.Balistes.Source — the reflective `balistes { … }` cell: the live
-- | Grids state rendered as a read-only purerl-tidal cell, the growing spec of
-- | the BEAM `balistes_voice`. Everything here is authored by direct
-- | manipulation (X/Y/densities/randomness from the pad+knobs, push from the
-- | groove knobs, ratchets from dragging a cell), so it is print-only — there is
-- | no editable body any more (the Tidal kit + its editor moved to Selene's
-- | POLYTRIG). The shell's TIDAL tab gathers this via `AskSource`.
module Triggerfish.Balistes.Source
  ( headerText
  ) where

import Prelude

import Data.Array (concatMap, filter, mapMaybe, null, range)
import Data.Maybe (Maybe(..))
import Data.String.Common (joinWith, toLower)
import Triggerfish.Balistes.Model as M

-- | The whole reflective cell: the faithful firmware config, the groove push,
-- | and any active ratchets — each block omitted when it has nothing to say.
headerText :: M.Balistes -> String
headerText b =
  joinWith "\n" (filter (_ /= "") [ configBlock b, grooveBlock b, ratchetBlock b ])

-- The faithful firmware config — what the BEAM `balistes` cell already takes.
configBlock :: M.Balistes -> String
configBlock b =
  "balistes \"kit\" iac 10 $ balistesConfig\n"
    <> "  { x          = pure " <> show b.x <> "\n"
    <> "  , y          = pure " <> show b.y <> "\n"
    <> "  , fillBd     = pure " <> show b.densBd <> "\n"
    <> "  , fillSd     = pure " <> show b.densSd <> "\n"
    <> "  , fillHh     = pure " <> show b.densHh <> "\n"
    <> "  , randomness = pure " <> show b.randomness <> "\n"
    <> "  }"

signed :: Int -> String
signed n = if n > 0 then "+" <> show n else show n

-- Per-lane timing push (Triggerfish extension).
grooveBlock :: M.Balistes -> String
grooveBlock b =
  let bd = M.pushOf 0 b
      sd = M.pushOf 1 b
      hh = M.pushOf 2 b
  in
    if bd == 0 && sd == 0 && hh == 0 then ""
    else "\n-- groove\n"
      <> "push  bd " <> signed bd <> "  sd " <> signed sd <> "  hh " <> signed hh <> "   -- ms"

-- Active ratchets on the three Grids lanes.
ratchetBlock :: M.Balistes -> String
ratchetBlock b =
  let
    forLane lane = mapMaybe
      ( \step ->
          let n = M.ratchetAt b lane step
          in if n > 1 then Just (toLower (M.instName lane) <> ":" <> show step <> "×" <> show n) else Nothing
      )
      (range 0 31)
    rs = concatMap forLane [ 0, 1, 2 ]
  in
    if null rs then "" else "\n-- ratchets\nratchet  " <> joinWith "  " rs
