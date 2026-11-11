-- | **A loop of hits, played on the Midifighter 3D's pads.** Samplr's slicer
-- | without the touchscreen (`docs/CONSPICILLUM-DESIGN.md`, §Recording play):
-- | each pad struck while recording is kept as (bar, place in the bar,
-- | sample), and the loop plays them back every time round.
-- |
-- | **Ephemeral, and a prototype.** A take belongs to the layer that is
-- | playing, never to a preset; it is gone when cleared or when the page
-- | closes. It reaches the rig as text: `notation` writes it as mini-notation,
-- | and `stack` sets it beside the preset's own `n "…"` in the scene alone. So
-- | the rig evaluates it like any pattern, the page draws it like any pattern,
-- | and nothing new was needed to replay it. The price is a grid: a hit is
-- | kept to the nearest 64th of a bar.
-- |
-- | Bars are counted as Tidal's `<…>` counts them, by the cycle number modulo
-- | the loop's length, so the bar a hit is kept in is the bar it replays in.
module Conspicillum.Take
  ( Take
  , Hit
  , empty
  , slots
  , record
  , clear
  , lengthen
  , notation
  , stack
  ) where

import Prelude

import Data.Array (drop, filter, groupBy, nub, range, snoc, sortBy, zipWith, (!!))
import Data.Array.NonEmpty as NonEmpty
import Data.Int (floor, round, toNumber)
import Data.Maybe (Maybe(..))
import Data.String (joinWith, trim)

type Hit = { bar :: Int, slot :: Int, index :: Int }

type Take = { recording :: Boolean, bars :: Int, hits :: Array Hit }

empty :: Take
empty = { recording: false, bars: 1, hits: [] }

-- | The grid a hit is kept to: 64 to the bar.
slots :: Int
slots = 64

-- | Keep sample `index`, struck `bars` bars after Link's zero (the page's
-- | clock: beats over four). A hit within half a slot of the next bar belongs
-- | to that bar's first slot.
record :: Number -> Int -> Take -> Take
record bars index take =
  let
    cycle0 = floor bars
    slot0 = round ((bars - toNumber cycle0) * toNumber slots)
    cycle = if slot0 >= slots then cycle0 + 1 else cycle0
    slot = if slot0 >= slots then 0 else slot0
  in
    take { hits = nub (snoc take.hits { bar: posMod cycle take.bars, slot, index }) }

clear :: Take -> Take
clear take = take { hits = [] }

-- | The next length, 1 → 2 → 4 → 1 bars. Longer copies what is there into
-- | the new bars, as Samplr's doubling does; shorter keeps the bars that
-- | remain.
lengthen :: Take -> Take
lengthen take = case take.bars of
  1 -> grow 2
  2 -> grow 4
  _ -> take { bars = 1, hits = filter (\h -> h.bar < 1) take.hits }
  where
  grow n = take
    { bars = n
    , hits = range 0 (n / take.bars - 1) >>= \k -> take.hits <#> \h -> h { bar = h.bar + k * take.bars }
    }

-- | The take as mini-notation, or "" when it holds nothing. One bar is
-- | `[~@5 0@11 [2,3]@48]`: a rest up to the first hit, then each hit (or each
-- | set struck in the same slot) held until the next. More bars are `<…>`.
notation :: Take -> String
notation take
  | take.hits == [] = ""
  | take.bars == 1 = barText take 0
  | otherwise = "<" <> joinWith " " (range 0 (take.bars - 1) <#> barText take) <> ">"

-- | One bar of a take, or `~` when it holds nothing.
barText :: Take -> Int -> String
barText take b =
  let
    inBar = sortBy (comparing _.slot <> comparing _.index) (filter (\h -> h.bar == b) take.hits)
    groups = groupBy (\x y -> x.slot == y.slot) inBar <#> \g ->
      { slot: (NonEmpty.head g).slot, indices: NonEmpty.toArray g <#> _.index }
    ends = drop 1 (groups <#> _.slot) <> [ slots ]
    events = zipWith (\g end -> struck g.indices <> "@" <> show (end - g.slot)) groups ends
    lead = case groups !! 0 of
      Just g | g.slot > 0 -> [ "~@" <> show g.slot ]
      _ -> []
  in
    if groups == [] then "~" else "[" <> joinWith " " (lead <> events) <> "]"

-- | Samples struck in one slot: one, or a stack.
struck :: Array Int -> String
struck = case _ of
  [ i ] -> show i
  several -> "[" <> joinWith "," (map show several) <> "]"

-- | The preset's pattern with a take beside it, as one pattern. Either may be
-- | empty.
stack :: String -> String -> String
stack pattern take
  | take == "" = pattern
  | trim pattern == "" = take
  | otherwise = "[" <> trim pattern <> ", " <> take <> "]"

posMod :: Int -> Int -> Int
posMod a n = let r = mod a n in if r < 0 then r + n else r
