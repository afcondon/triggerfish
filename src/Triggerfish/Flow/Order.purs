-- | `Triggerfish.Flow.Order` — the order within each column, from the wiring.
-- |
-- | The chart's columns were ordered by a fixed list (`nodeRank`), so it held
-- | still as machines opened and played; but a fixed list is right only for
-- | the routing it was written for. A Rample fed from the first port drawn
-- | second crosses everything between (AC, 2026-10-04).
-- |
-- | So the columns after the engine (Rig out, Interface, Instrument) follow
-- | their wiring: each node goes to the mean position of what it is joined
-- | to (the barycentre step of Sugiyama's layered drawing), swept left to
-- | right, back, and forward again. The fixed list is where this starts and
-- | the tie-break, and every sweep is kept only if it crosses less, so the
-- | result never crosses more than the fixed list did. The engine's column
-- | and everything left of it keep the fixed list: they are the landmarks.
-- |
-- | What is weighed is which nodes are joined, not how many streams run, and
-- | bones (the X-ray's idle wiring) are not weighed: the order moves when the
-- | routing does, never when something starts or stops playing.
module Triggerfish.Flow.Order
  ( Order
  , orderOf
  , rankIn
  , crossings
  , wired
  ) where

import Prelude

import Data.Array (concatMap, elem, filter, foldl, length, mapWithIndex, nub, sortWith)
import Data.Array as Array
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
import Triggerfish.Flow (Column(..), Flow, Link, Node, nodeRank)

-- | Each node's place within its column, from 0 at the top.
type Order = Map.Map String Int

-- | The columns whose order follows their wiring.
wired :: Array Column
wired = [ RigOut, Interface, Instrument ]

-- | A node's rank for the layout's `nodeSort`: its place in `Order` in a
-- | wired column, the fixed list elsewhere.
rankIn :: Order -> Node -> Tuple Column (Tuple Int String)
rankIn ord nd = case Map.lookup nd.id ord of
  Just i | nd.column `elem` wired -> Tuple nd.column (Tuple i nd.name)
  _ -> nodeRank nd

orderOf :: Flow -> Order
orderOf f = snd (foldl fewer (Tuple (crossings f start) start) candidates)
  where
  start = Map.fromFoldable (concatMap (\c -> mapWithIndex (\i nd -> Tuple nd.id i) (sortWith nodeRank (nodesIn c))) columns)
  columns = nub (map _.column f.nodes)
  present = Array.sort (filter (_ `elem` columns) wired)
  nodesIn c = filter (\nd -> nd.column == c) f.nodes
  size = Map.fromFoldable (map (\c -> Tuple c (length (nodesIn c))) columns)
  columnOf = Map.fromFoldable (map (\nd -> Tuple nd.id nd.column) f.nodes)
  live = filter (not <<< _.bone) f.links

  -- where a node stands in its column, as a fraction of the column, so a
  -- column of two and a column of six are weighed alike
  pos ord id = case Map.lookup id ord, Map.lookup id columnOf >>= \c -> Map.lookup c size of
    Just i, Just k -> (toNumber i + 0.5) / toNumber k
    _, _ -> 0.5

  -- one column, put in the order of its neighbours on one side
  sweep incoming ord c =
    let
      nbrs id
        | incoming = map _.from (filter (\l -> l.to == id) live)
        | otherwise = map _.to (filter (\l -> l.from == id) live)
      bary nd = case nbrs nd.id of
        [] -> pos ord nd.id
        xs -> foldl (+) 0.0 (map (pos ord) xs) / toNumber (length xs)
      sorted = sortWith (\nd -> Tuple (bary nd) (fromMaybe 0 (Map.lookup nd.id ord))) (nodesIn c)
    in
      foldl (\m (Tuple i nd) -> Map.insert nd.id i m) ord (mapWithIndex Tuple sorted)

  forward ord = foldl (sweep true) ord present
  back ord = foldl (sweep false) ord (Array.reverse present)
  candidates =
    let
      a = forward start
      b = back a
      c = forward b
    in
      [ a, b, c ]
  -- keep the first of the fewest
  fewer best ord =
    let k = crossings f ord
    in if k < fst best then Tuple k ord else best

-- | How many pairs of lines cross, counting lines that span the same two
-- | columns (lines of different spans are left out: a fair count of those
-- | needs the lanes' waypoints, and the order seldom hangs on them).
crossings :: Flow -> Order -> Int
crossings f ord = length (filter crossed pairs)
  where
  columnOf = Map.fromFoldable (map (\nd -> Tuple nd.id nd.column) f.nodes)
  live = filter (\l -> not l.bone && Map.member l.from columnOf && Map.member l.to columnOf) f.links
  indexed = mapWithIndex Tuple live
  pairs = concatMap (\(Tuple i a) -> map (Tuple a <<< snd) (filter (\(Tuple j _) -> j > i) indexed)) indexed
  at id = fromMaybe 0 (Map.lookup id ord)
  span (l :: Link) = Tuple (Map.lookup l.from columnOf) (Map.lookup l.to columnOf)
  crossed (Tuple a b) =
    span a == span b && a.from /= b.from && a.to /= b.to
      && (at a.from - at b.from) * (at a.to - at b.to) < 0
