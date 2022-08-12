-- | Some graphs from mathematics.
module Bittide.Topology.Graph ( diamond, tree, star, kn, cn ) where

import Prelude

import Data.Array qualified as A
import Data.Function (on)
import Data.Graph (Graph, graphFromEdges)
import Data.List (groupBy, sort)
import Data.Maybe (mapMaybe)
import Data.Tuple (swap)

-- | Diamond graph
diamond :: Graph
diamond = A.listArray (0, 3) [[1,3], [0,2,3], [2,3], [0,1,2]]

-- | Tree of depth @d@ with @c@ children
tree :: Int -> Int -> Graph
tree d c = treeGraph
 where
  -- | At depth @d_i@, child node @i@ is connected to the @(i-1) `div` c + 1@st
  -- node at depth @d_i - 1@
  pairs = [ (d_i, i, (i-1) `div` c + 1) | d_i <- [0..d], i <- [1..(c^d_i)] ]
  mkEdges (0, _, _)           = Nothing
  mkEdges (lvl, node, p_node) = Just ((lvl, node), (lvl-1, p_node))
  directedEdges = mapMaybe mkEdges pairs
  edges = directedEdges ++ fmap swap directedEdges
  adjList = g <$> groupBy ((==) `on` fst) (sort edges)
  g ps@((x,_):_) = (x, snd <$> ps)
  (treeGraph, _, _) = graphFromEdges ((\(key, keys) -> (undefined, key, keys)) <$> adjList)

-- | [Star graph](https://mathworld.wolfram.com/StarGraph.html)
star :: Int -> Graph
star = tree 1

-- | [Cyclic graph](https://mathworld.wolfram.com/CycleGraph.html) with @n@
-- vertices.
cn :: Int -> Graph
cn n = A.array bounds (fmap (\i -> (i, neighbors i)) [0..(n-1)])
 where
  bounds = (0, n-1)
  neighbors i = [(i-1) `mod` n, (i+1) `mod` n]

-- | [Complete graph](https://mathworld.wolfram.com/CompleteGraph.html) with @n@
-- vertices.
kn :: Int -> Graph
kn n = A.array bounds (fmap (\i -> (i, others i)) [0..(n-1)])
 where
  bounds = (0, n-1)
  others i = [ j | j <- [0..(n-1)], j /= i ]