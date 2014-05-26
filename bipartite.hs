module Bipartite where

import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad

data NodeX = X Int deriving (Show, Eq)
data NodeY = Y Int deriving (Show, Eq)

data Node = Nx NodeX | Ny NodeY deriving (Show, Eq)

type Edge = (NodeX, NodeY)

data Graph = Graph [NodeX] [NodeY] [Edge] deriving (Show, Eq)

data Matching = Matching [Edge]

(><) :: Functor f =>
     [Edge]
     -> ((Edge -> Bool) -> [Edge] -> f Edge)
     -> Node
     -> f Node
(><) es f (Nx x) = (Ny . snd) <$> f (\(u, _) -> u == x) es
(><) es f (Ny y) = (Nx . fst) <$> f (\(_, v) -> v == y) es

neighbours :: Graph -> Node -> [Node]
neighbours (Graph _ _ es) n = (><) es filter n

matched :: Matching -> Node -> Maybe Node
matched (Matching es) n = (><) es find n

-- difference
a |-- b = filter (\x -> not $ x `elem` b) a

-- intersection
a ^ b = filter (\x -> x `elem` b) a

-- symetric difference
a <+> b = (a |-- b) ++ (a |-- b)

makeEdge :: Node -> Node -> Maybe Edge
makeEdge (Nx x) (Ny y) = Just (x, y)
makeEdge (Ny y) (Nx x) = Just (x, y)
makeEdge _ _           = Nothing

inMatching :: Node -> Matching -> Bool
inMatching (Nx x) (Matching es) = x `elem` (fst . unzip $ es)
inMatching (Ny y) (Matching es) = y `elem` (snd . unzip $ es)

findAvailableEdge :: Graph -> Node -> Matching -> Matching
findAvailableEdge g v m@(Matching es) = case w of
    Just n  -> Matching $ (fromJust $ makeEdge v n) : es
    Nothing -> m
    where w = find (\n -> not $ n `inMatching` m) (neighbours g v)

matching :: Graph -> Matching
matching g = matching' g (Matching [])
    where matching' g@(Graph xs _ _) m = foldr (findAvailableEdge g) m (Nx <$> xs)
