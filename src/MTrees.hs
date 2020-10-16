-- | Questions 70B to 73: Multiway trees
module MTrees where

-- |
-- Problem 70B
--
-- (*) Check whether a given term represents a multiway tree.
--
-- In Prolog or Lisp, one writes a predicate to check this.
--
-- Example in Prolog:
--
-- ?- istree(t(a,[t(f,[t(g,[])]),t(c,[]),t(b,[t(d,[]),t(e,[])])])).
-- Yes
--
-- In Haskell, we define multiway trees as a datatype, as in the module Data.Tree:
data MTree a = Node a [MTree a] deriving (Eq, Show)

-- Some example trees:

mtree1 = Node 'a' []

mtree2 = Node 'a' [Node 'b' []]

mtree3 = Node 'a' [Node 'b' [Node 'c' []]]

mtree4 = Node 'b' [Node 'd' [], Node 'e' []]

mtree5 =
  Node
    'a'
    [ Node 'f' [Node 'g' []],
      Node 'c' [],
      Node 'b' [Node 'd' [], Node 'e' []]
    ]

-- The last is the tree illustrated above.

-- As in problem 54A, all members of this type are multiway trees;
-- there is no use for a predicate to test them.
------------------

-- |
-- Problem 70C
--
-- (*) Count the nodes of a multiway tree.
--
-- Example in Haskell:
--
-- >>> nnodes tree2
-- 2
nnodes :: MTree a -> Int
nnodes (Node _ []) = 1
nnodes (Node _ (x : [])) = 1 + (nnodes x)
nnodes (Node _ (x : xs)) = 1 + (nnodes x) + sum (map nnodes xs)

-- |
-- Problem 70
--
-- (**) Tree construction from a node string.
--
-- We suppose that the nodes of a multiway tree contain single characters.
-- In the depth-first order sequence of its nodes, a special character ^ has been inserted whenever,
-- during the tree traversal, the move is a backtrack to the previous level.
-- By this rule, the tree below (tree5) is represented as: afg^^c^bd^e^^^
--
-- Define the syntax of the string and write a predicate tree(String,Tree) to construct the Tree when the String is given.
-- Make your predicate work in both directions.
--
-- Example in Haskell:
--
-- >>> stringToTree "afg^^c^bd^e^^^"
-- Node 'a' [Node 'f' [Node 'g' []],Node 'c' [],Node 'b' [Node 'd' [],Node 'e' []]]
stringToTree :: String -> MTree Char
stringToTree (c1 : c2 : cs) = go (c1 : c2 : cs) (Node c1 [])
  where
    go (c1 : c2 : cs) t@(Node n es)
      | c1 == '^' && c2 /= '^' = stringToTree (c2 : cs)
      | c1 == '^' && c2 == '^' = stringToTree (cs)
      | c1 /= '^' && c2 == '^' = Node c1 []
      | c1 /= '^' && c2 /= '^' = Node c1 [stringToTree (c2 : cs)]

-- >>> treeToString (Node 'a' [Node 'f' [Node 'g' []],Node 'c' [],Node 'b' [Node 'd' [],Node 'e' []]])
-- "afg^^c^bd^e^^^"
treeToString :: MTree Char -> String
treeToString (Node c []) = c : '^' : []
treeToString (Node c (x : [])) = c : (treeToString x) ++ ['^']
treeToString (Node c (x : xs)) = c : (treeToString x) ++ (xs >>= treeToString) ++ ['^']
