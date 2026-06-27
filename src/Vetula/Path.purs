-- | Vetula.Path — pathfinding on a lattice's Hasse graph. The lattice's edges
-- | (every pair of family chords one note apart) form an undirected graph in
-- | which every step is a single-voice move; a *path segment* is a walk on it
-- | between two chosen chords. For now we take the shortest walk (fewest moves)
-- | by breadth-first search; the "more or less circumambulatory" variations
-- | (k-shortest / wandering) will layer on top of this.
module Vetula.Path
  ( Graph
  , adjacency
  , shortestPath
  ) where

import Prelude

import Data.Array (elem, filter, snoc, uncons)
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)

type Graph = Map Int (Array Int)

-- | Build an undirected adjacency map from an edge list (the lattice's
-- | `neighborLinks`). Duplicate edges collapse.
adjacency :: Array { source :: Int, target :: Int } -> Graph
adjacency = foldl (\g e -> add e.source e.target (add e.target e.source g)) Map.empty
  where
  add k v g = Map.insert k (case Map.lookup k g of
                              Just ns -> if elem v ns then ns else snoc ns v
                              Nothing -> [ v ]) g

-- | The shortest walk from `start` to `goal` (inclusive of both), or `Nothing`
-- | if they're in different components (e.g. different exploded families, which
-- | share no edges). Breadth-first, so it's fewest single-note moves.
shortestPath :: Graph -> Int -> Int -> Maybe (Array Int)
shortestPath g start goal
  | start == goal = Just [ start ]
  | otherwise = bfs [ start ] (Map.singleton start start)
  where
  bfs queue parents = case uncons queue of
    Nothing -> Nothing
    Just { head: cur, tail: rest } ->
      let
        fresh = filter (\n -> not (Map.member n parents)) (fromMaybe [] (Map.lookup cur g))
        parents' = foldl (\p n -> Map.insert n cur p) parents fresh
      in
        if elem goal fresh then Just (reconstruct parents' goal)
        else bfs (rest <> fresh) parents'

  reconstruct parents node
    | node == start = [ start ]
    | otherwise = case Map.lookup node parents of
        Just p -> snoc (reconstruct parents p) node
        Nothing -> [ node ]
