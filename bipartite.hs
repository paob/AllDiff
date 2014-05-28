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
a <+> b = (a |-- b) ++ (a |-- b)

(<-->) :: Node -> Node -> Maybe Edge
(Left  x) <--> (Right y) = Just (x, y)
(Right y) <--> (Left  x) = Just (x, y)
_ <--> _           = Nothing

isSaturatedIn :: Node -> Matching -> Bool
isSaturatedIn (Left  x) (Matching es) = x `elem` (fst . unzip $ es)
isSaturatedIn (Right y) (Matching es) = y `elem` (snd . unzip $ es)

addIndependentEdge :: Edge -> Matching -> Matching
addIndependentEdge e@(u,v) m@(Matching es) 
    | (Left u) `isSaturatedIn` m || (Right v) `isSaturatedIn` m = m
    | otherwise = Matching (e:es)

-- A maximal matching is a matching M of a graph G with the property that if
-- any edge not in M is added to M, it is no longer a matching
maximalMatching :: Graph -> Matching
maximalMatching g = maximalMatching' g (Matching [])
    where maximalMatching' (Graph _ _ es) m = foldr addIndependentEdge m es
