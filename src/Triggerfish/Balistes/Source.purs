-- | Triggerfish.Balistes.Source — render the live module state as a purerl-tidal
-- | `balistes { … }` cell. Read-only: this is the *growing spec* of the BEAM
-- | virtual module (the same role Odonus's eDSL panel plays). The top block is
-- | the faithful `balistesConfig` the BEAM already understands; everything below
-- | the divider is a Triggerfish extension the BEAM cell doesn't have *yet* — so
-- | the panel literally shows what `Tidal.Balistes`/`balistes_engine` must grow
-- | to round-trip what you can now play here.
-- |
-- | The pad lanes print as mini-notation-ish strings (`x···` per 16th) — the
-- | shape a Tidal-authored lane will take, since each lane is a future Tidal
-- | source merged (`stack`) with whatever you've clicked in.
module Triggerfish.Balistes.Source (sourceText) where

import Prelude

import Data.Array (concatMap, filter, mapMaybe, null, range, replicate)
import Data.Foldable (any)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Common (joinWith, toLower)
import Triggerfish.Balistes.Model as M

sourceText :: M.Balistes -> String
sourceText b =
  joinWith "\n" (filter (_ /= "") [ configBlock b, grooveBlock b, ratchetBlock b, padBlock b ])

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

-- The pad lanes as real Tidal: each lane's typed mini-notation `source`,
-- `stack`ed with its clicked overlay (rendered as mini-notation at the lane's
-- derived meter). Only the programmed lanes print.
padBlock :: M.Balistes -> String
padBlock b =
  let
    nm i = padR 3 (toLower (M.padName b i))
    laneLine i =
      let
        src = String.trim (M.padSource b i)
        clk = M.padClicks b i
        hasClk = any identity clk
        clkStr = clicksMini clk
      in
        case src /= "", hasClk of
          false, false -> Nothing
          true, false -> Just ("  " <> nm i <> " \"" <> src <> "\"")
          false, true -> Just ("  " <> nm i <> " \"" <> clkStr <> "\"")
          true, true -> Just ("  " <> nm i <> " stack [\"" <> src <> "\", \"" <> clkStr <> "\"]")
    lines = mapMaybe laneLine (range 0 (M.padCount b - 1))
  in
    if null lines then "" else "\n-- pads\n" <> joinWith "\n" lines

-- A clicked overlay as a flat mini-notation string at its own meter.
clicksMini :: Array Boolean -> String
clicksMini = joinWith "" <<< map (\c -> if c then "x" else "·")

padR :: Int -> String -> String
padR n s = if String.length s >= n then s else s <> joinWith "" (replicate (n - String.length s) " ")
