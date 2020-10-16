-- | Questions 80 to 89: Graphs
-- | https://wiki.haskell.org/99_questions/80_to_89
module Graphs where

import Data.List

-- | Graphs
data Graph a = Graph [a] [(a, a)] deriving (Show, Eq)

data Adjac a = Adjac [(a, [a])] deriving (Show, Eq)

-- |
-- @
--      b -- c     d
--      \    /
--       \  /         g --- h
--        f --- k
-- @
graph1 = Graph ['b', 'c', 'd', 'f', 'g', 'h', 'k'] [('b', 'c'), ('b', 'f'), ('c', 'f'), ('f', 'k'), ('g', 'h')]

adjac1 = Adjac [('b', "cf"), ('c', "bf"), ('d', ""), ('f', "bck"), ('g', "h"), ('h', "g"), ('k', "f")]

-- |
-- Problem 80
--
-- (***) Conversions
--
-- Write predicates to convert between the different graph representations.
-- With these predicates, all representations are equivalent;
-- i.e. for the following problems you can always pick freely the most convenient form.
-- The reason this problem is rated (***) is not because it's particularly difficult,
-- but because it's a lot of work to deal with all the special cases.
--
-- Example in Haskell:
--
-- >>> graphToAdj Graph ['b','c','d','f','g','h','k'] [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')]
-- Adj [('b', "cf"), ('c', "bf"), ('d', ""), ('f', "bck"), ('g', "h"), ('h', "g"), ('k', "f")]
graphToAdj :: Eq a => Graph a -> Adjac a
graphToAdj (Graph nodes edges) = Adjac (map (\n -> (n, (getEdges n))) nodes)
  where
    searchEdges n = filter (\e -> fst e == n || snd e == n) edges
    bothDirects es = foldr (\a b -> (fst a) : (snd a) : b) [] es
    format es n = filter (/= n) es
    getEdges n = format (bothDirects (searchEdges n)) n

-- |
-- Problem 81
--
-- (**) Path from one node to another one
--
-- Write a function that, given two nodes a and b in a graph, returns all the acyclic paths from a to b.
-- Example in Haskell:
--
-- @
--         /---------\    5 --- 6
--  1 --- 2 --- 3 --- 4
--   \_________/
-- @
-- >>> paths 1 4 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
-- [[1,2,3,4],[1,3,4]]
--
-- >>> paths 2 6 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
-- []
paths :: Eq a => Int -> Int -> Graph a -> [[a]]
paths t f (Graph ns es) = undefined --foldl (b a -> b) [[]] es
  where
    go = undefined
    searchEdges n edges = filter (\e -> fst e == n) edges
    process edges = nub edges

-- * Main> let t = 1

-- * Main> let f = 4

-- * Main> let es = [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]

-- * Main> let searchEdges n edges = filter (\e -> fst e == n) edges

-- * Main> searchEdges t es

-- [(1,2),(1,3)]

-- * Main> (searchEdges 2 es) ++ (searchEdges 2 es)

-- [(2,3),(2,3)]

-- * Main> nub ((searchEdges 2 es) ++ (searchEdges 2 es))

-- [(2,3)]

-- * Main> nub ((searchEdges 2 es) ++ (searchEdges 3 es))

-- [(2,3),(3,4)]

-- * Main> nub ((searchEdges 3 es) ++ (searchEdges 4 es))

-- [(3,4),(4,2)]

-- * Main> searchEdges 4 es

-- [(4,2)]

-- * Main> process (searchEdges t es)

-- [(1,2),(1,3)]

-- * Main> map (\e -> process (searchEdges (snd e) es)) [(1,2),(1,3)]

-- [[(2,3)],[(3,4)]]
-- map (\e -> process (searchEdges (snd e) es)) [(2,3)]
-- [[(3,4)]]

-- * Main> let t = 2

-- * Main> let f = 6

-- * Main> process (searchEdges t es)

-- [(2,3)]

-- * Main> map (\e -> process (searchEdges (snd e) es)) [(2,3)]

-- [[(3,4)]]

-- * Main> map (\e -> process (searchEdges (snd e) es)) [(3, 4)]

-- [[(4,2)]]

-- * Main> map (\e -> process (searchEdges (snd e) es)) [(4, 2)]

-- [[(2,3)]]
