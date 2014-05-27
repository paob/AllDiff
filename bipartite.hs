module Bipartite where

import Data.List (find)
import Data.Maybe (fromJust)
import Control.Applicative ((<$>))

data NodeX = X Int deriving (Show, Eq)
data NodeY = Y Int deriving (Show, Eq)

data Node = Nx NodeX | Ny NodeY deriving (Show, Eq)

type Edge = (NodeX, NodeY)

data Graph = Graph [NodeX] [NodeY] [Edge] deriving (Show, Eq)

data Matching = Matching [Edge]

graph :: Functor f =>
     ((Edge -> Bool) -> [Edge] -> f Edge)
     -> Node 
     -> [Edge]
     -> f Node
graph f (Nx x) es = (Ny . snd) <$> f (\(u, _) -> u == x) es
graph f (Ny y) es = (Nx . fst) <$> f (\(_, v) -> v == y) es

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
(Nx x) <--> (Ny y) = Just (x, y)
(Ny y) <--> (Nx x) = Just (x, y)
_ <--> _           = Nothing

isSaturatedIn :: Node -> Matching -> Bool
isSaturatedIn (Nx x) (Matching es) = x `elem` (fst . unzip $ es)
isSaturatedIn (Ny y) (Matching es) = y `elem` (snd . unzip $ es)

findAvailableEdge :: Graph -> Node -> Matching -> Matching
findAvailableEdge g v m@(Matching es) = case w of
    Just n  -> let Just e = v <--> n in Matching $ e:es
    Nothing -> m
    where w = find (\n -> not $ n `isSaturatedIn` m) (neighbours g v)

matching :: Graph -> Matching
matching g = matching' g (Matching [])
    where matching' g@(Graph xs _ _) m = foldr (findAvailableEdge g) m (Nx <$> xs)

augmentingPath :: Graph -> Matching -> Node -> [Edge]
augmentingPath g m root = augmentingPath' g m root []
    where augmentingPath' g m root visited = case (neighbours g root) |-- visited of
            []   -> []
            v:vs ->
                if (v `isSaturatedIn` m) then
                    case matched m v of
                        Just n  -> e1 : e2 : (augmentingPath' g m n (v:visited))
                                   where Just e1 = n <--> v
                                         Just e2 = v <--> root
                        Nothing -> augmentingPath' g m root (v:visited)
                else [e] where Just e = v <--> root
