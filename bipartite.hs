module Bipartite where

import Data.List (find)
import Data.Maybe (fromJust)
import Control.Applicative ((<$>))

data NodeX = X Int deriving (Show, Eq)
data NodeY = Y Int deriving (Show, Eq)

type Node = Either NodeX NodeY

type Edge = (NodeX, NodeY)

data Graph = Graph [NodeX] [NodeY] [Edge] deriving (Show, Eq)

data Matching = Matching [Edge]

-- Higher-order function implementing set irrelevance: allow application of
-- functions on nodes regardless of which set the node belongs to.
graph :: Functor f =>
     ((Edge -> Bool) -> [Edge] -> f Edge)
     -> Node
     -> [Edge]
     -> f Node
graph f (Left  x) es = (Right . snd) <$> f (\(u, _) -> u == x) es
graph f (Right y) es = (Left  . fst) <$> f (\(_, v) -> v == y) es

neighbours :: Graph -> Node -> [Node]
neighbours (Graph _ _ es) n = graph filter n es

matched :: Matching -> Node -> Maybe Node
matched (Matching es) n = graph find n es

-- difference
a |-- b = filter (\x -> not $ x `elem` b) a

-- intersection
a ^ b = filter (\x -> x `elem` b) a

-- symetric difference
a <+> b = (a |-- b) ++ (b |-- a)

(<-->) :: Node -> Node -> Maybe Edge
(Left  x) <--> (Right y) = Just (x, y)
(Right y) <--> (Left  x) = Just (x, y)
_ <--> _           = Nothing

isSaturatedIn :: Node -> Matching -> Bool
isSaturatedIn (Left  x) (Matching es) = x `elem` (fst . unzip $ es)
isSaturatedIn (Right y) (Matching es) = y `elem` (snd . unzip $ es)

isFreeIn :: Node -> Matching -> Bool
isFreeIn n m = not $ n `isSaturatedIn` m

addIndependentEdge :: Edge -> Matching -> Matching
addIndependentEdge e@(u, v) m@(Matching es)
    | (Left u) `isSaturatedIn` m || (Right v) `isSaturatedIn` m = m
    | otherwise = Matching (e:es)

-- A maximal matching is a matching M of a graph G with the property that if
-- any edge not in M is added to M, it is no longer a matching
maximalMatching :: Graph -> Matching
maximalMatching g = maximalMatching' g (Matching [])
    where maximalMatching' (Graph _ _ es) m = foldr addIndependentEdge m es

--augmentingPath :: Graph -> Node -> [Edge]
--FIND-AUGMENTING-PATH(G=(V1∪V2,E),M)
{-

    V′1 = a set of free vertices in V1
    V′2 = a set of free vertices in V2
    Construct the directed graph GM = (V1 ∪ V2, EM)
        EM is a set of directed edges such that it includes all arcs from V1 to
            V2, and all matching arcs from V2 to V1
        i.e.
        EM = {(v1, v2) : v1, v2 ∈ E ∖ M, v1 ∈ V1, v2 ∈ V2} ∪
             {(v2, v1) : v1, v2 ∈ M, v1 ∈ V1, v2 ∈ V2}
    Find a simple path p from V′_1 to V′_2 in GM

Note that the above graph G_M is similar to the residual network in network
flow. Apparently, p starts from a free vertex in V′_1 and ends at another free
vertex in V′_2, thus it is an augmenting path.
-}

freeNodes f m es = filter (\n -> (f n) `isFreeIn` m) es

{-
augmentingPath (Graph xs ys es) m =
    where gm = Graph freeXs freeYs alternatingEdges
          freeXs = freeNodes Left  m es
          freeYs = freeNodes Right m es
          alternatingEdges =
-}
