-- | Questions 61 to 69: Binary trees, continued
module BTrees2 where

import BTrees1

-- |
-- Problem 61
-- Count the leaves of a binary tree
--
-- A leaf is a node with no successors. Write a predicate count_leaves/2 to count them.
--
-- Example:
--
-- % count_leaves(T,N) :- the binary tree T has N leaves
-- Example in Haskell:
--
-- >>> countLeaves tree4
-- 2
countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ left right) = countLeaves left + countLeaves right

-- |
-- Problem 61A
-- Collect the leaves of a binary tree in a list
--
-- A leaf is a node with no successors. Write a predicate leaves/2 to collect them in a list.
--
-- Example:
--
-- % leaves(T,S) :- S is the list of all leaves of the binary tree T
-- Example in Haskell:
--
-- >>> leaves tree4
-- [4,2]
leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch _ left right) = leaves left ++ leaves right

-- |
-- Problem 62
-- Collect the internal nodes of a binary tree in a list
--
-- An internal node of a binary tree has either one or two non-empty successors.
-- Write a predicate internals/2 to collect them in a list.
--
-- Example:
--
-- % internals(T,S) :- S is the list of internal nodes of the binary tree T.
-- Example in Haskell:
--
-- >>> internals tree4
-- [1,2]
internals :: Tree a -> [a]
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch x left right) = x : (internals left ++ internals right)

-- |
-- Problem 62B
-- Collect the nodes at a given level in a list
--
-- A node of a binary tree is at level N if the path from the root to the node has length N-1.
-- The root node is at level 1.
-- Write a predicate atlevel/3 to collect all nodes at a given level in a list.
--
-- Example:
--
-- % atlevel(T,L,S) :- S is the list of nodes of the binary tree T at level L
-- Example in Haskell:
--
-- >>> atLevel tree4 2
-- [2,2]
atLevel :: Tree a -> Int -> [a]
atLevel t l = go t l 1
  where
    go Empty _ _ = []
    go (Branch x Empty Empty) l curr = [x | l == curr]
    go (Branch x left right) l curr =
      if l == curr
        then [x]
        else go left l (curr + 1) ++ go right l (curr + 1)

-- |
-- Problem 63
-- Construct a complete binary tree
--
-- A complete binary tree with height H is defined as follows:
--
-- The levels 1,2,3,...,H-1 contain the maximum number of nodes (i.e 2**(i-1) at the level i)
-- In level H, which may contain less than the maximum possible number of nodes, all the nodes are "left-adjusted".
-- This means that in a levelorder tree traversal all internal nodes come first,
-- the leaves come second, and empty successors (the nil's which are not really nodes!) come last.
-- Particularly, complete binary trees are used as data structures (or addressing schemes) for heaps.
--
-- We can assign an address number to each node in a complete binary tree by enumerating the nodes in level-order,
-- starting at the root with number 1.
-- For every node X with address A the following property holds:
-- The address of X's left and right successors are 2*A and 2*A+1, respectively, if they exist.
-- This fact can be used to elegantly construct a complete binary tree structure.
--
-- Write a predicate complete_binary_tree/2.
--
-- Example:
--
-- % complete_binary_tree(N,T) :- T is a complete binary tree with N nodes.
-- Example in Haskell:
--
-- >>> completeBinaryTree 4
-- Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty Empty)
--
-- >>> isCompleteBinaryTree $ Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)
-- True
