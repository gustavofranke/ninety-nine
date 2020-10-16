-- | Questions 46 to 50: Logic and codes
-- | Logic and Codes
module Logic where

import Data.List

-- |
-- Problem 46
--
-- (**) Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for logical equivalence)
--  which succeed or fail according to the result of their respective operations;
--   e.g. and(A,B) will succeed, if and only if both A and B succeed.
--
-- A logical expression in two variables can then be written as in the following example: and(or(A,B),nand(A,B)).
--
-- Now, write a predicate table/3 which prints the truth table of a given logical expression in two variables.
--
-- Example:
--
-- (table A B (and A (or A B)))
-- true true true
-- true fail true
-- fail true fail
-- fail fail fail
-- Example in Haskell:
--
-- >>> table (\a b -> (and' a (or' a b)))
-- True True True
-- True False True
-- False True False
-- False False False
and' :: Bool -> Bool -> Bool
and' a b
  | (a, b) == (True, True) = True
  | otherwise = False

or' :: Bool -> Bool -> Bool
or' a b
  | (a, b) == (False, False) = False
  | otherwise = True

not' :: Bool -> Bool
not' a
  | a == False = True
  | a == True = False

nand' :: Bool -> Bool -> Bool
nand' = and' . not'

nor' :: Bool -> Bool -> Bool
nor' = or' . not'

xor' :: Bool -> Bool -> Bool
xor' a b
  | a == b = False
  | otherwise = True

impl' :: Bool -> Bool -> Bool
impl' p = or' (not' p)

equ' :: Bool -> Bool -> Bool
equ' a b
  | a == b = True
  | otherwise = False

table :: (Bool -> Bool -> Bool) -> [[Bool]]
table f = map (\x -> fst x : snd x : uncurry f x : []) inputs -- uncurry f x == f (fst x) (snd x)
  where
    inputs = [(True, True), (True, False), (False, True), (False, False)]

-- > table (\a b -> (and' a (or' a b)))
-- [[True,True,True],[True,False,True],[False,True,False],[False,False,False]]

-- |
-- Problem 47
--
-- (*) Truth tables for logical expressions (2).
--
-- Continue problem P46 by defining and/2, or/2, etc as being operators.
-- This allows to write the logical expression in the more natural way, as in the example: A and (A or not B).
-- Define operator precedence as usual; i.e. as in Java.
--
-- Example:
--
-- * (table A B (A and (A or not B)))
-- true true true
-- true fail true
-- fail true fail
-- fail fail fail
-- Example in Haskell:
--
-- >>> table2 (\a b -> a `and'` (a `or'` not b))
-- True True True
-- True False True
-- False True False
-- False False False
--
-- table (\a b -> a `and'` (a `or'` not b))
-- [[True,True,True],[True,False,True],[False,True,False],[False,False,False]]
-- TODO: I think there's no impl needed here?
-- -------------------------------

-- |
-- Problem 48
--
-- (**) Truth tables for logical expressions (3).
--
-- Generalize problem P47 in such a way that the logical expression may contain any number of logical variables.
-- Define table/2 in a way that table(List,Expr) prints the truth table for the expression Expr,
-- which contains the logical variables enumerated in List.
--
-- Example:
--
-- @
-- * (table (A,B,C) (A and (B or C) equ A and B or A and C))
-- true true true true
-- true true fail true
-- true fail true true
-- true fail fail true
-- fail true true true
-- fail true fail true
-- fail fail true true
-- fail fail fail true
-- Example in Haskell:
--
-- >>> tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)
--  infixl 3 `equ'`
-- True  True  True  True
-- True  True  False True
-- True  False True  True
-- True  False False True
-- False True  True  True
-- False True  False True
-- False False True  True
-- False False False True
--
--  infixl 7 `equ'`
-- True  True  True  True
-- True  True  False True
-- True  False True  True
-- True  False False False
-- False True  True  False
-- False True  False False
-- False False True  False
-- False False False False
-- @
tablen :: Integral b => b -> ([Bool] -> Bool) -> [[Bool]]
tablen n f = map (\i -> i ++ [f i]) inputs
  where
    colums = [1 .. n]
    bools = \x -> replicate ((2 ^ x) `div` 2)
    trues = \x -> bools x True
    falses = \x -> bools x False
    --   inputs = transpose (map (\xs -> take (2 ^ n) (cycle xs)) (map (\x -> trues x ++ falses x) $ reverse colums))
    inputs = transpose (map (\x -> take (2 ^ n) (cycle (trues x ++ falses x))) $ reverse colums)

-- * Main> tablen 3 (\[a,b,c] -> a `and'` (b `or'` c)) -- these set of results seems to be the second bit of the example above

-- [[True,True,True,True],
-- [True,True,False,True],
-- [True,False,True,True],
-- [True,False,False,False],
-- [False,True,True,False],
-- [False,True,False,False],
-- [False,False,True,False],
-- [False,False,False,False]
-- ]

-- * Main> tablen 3 (\[a,b,c] -> a `and'` b `or'` a `and'` c)

-- [[True,True,True,True],
-- [True,True,False,False],
-- [True,False,True,True],
-- [True,False,False,False],
-- [False,True,True,False],
-- [False,True,False,False],
-- [False,False,True,False],
-- [False,False,False,False]
-- ]

-- > tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)
-- [
-- [True,True,True,True],
-- [True,True,False,False],
-- [True,False,True,True],
-- [True,False,False,False],
-- [False,True,True,True],
-- [False,True,False,False],
-- [False,False,True,False],
-- [False,False,False,False]
-- ]
-- TODO: I think the `equ` impl is buggy, as the results have discrepancies
-- what follows is what helped my "reasoning"

-- * Main> map (\x -> replicate ((2 ^ x) `div` 2) True ++ replicate ((2 ^ x) `div` 2) False) $ reverse colums

-- [[True,True,True,True,False,False,False,False],[True,True,False,False],[True,False]]

-- * Main> map (\xs -> take (2 ^ n) (cycle xs)) [[True,True,True,True,False,False,False,False],[True,True,False,False],[True,False]]

-- [[True,True,True,True,False,False,False,False],[True,True,False,False,True,True,False,False],[True,False,True,False,True,False,True,False]]

-- * Main> transpose [[True,True,True,True,False,False,False,False],[True,True,False,False,True,True,False,False],[True,False,True,False,True,False,True,False]]

-- [[True,True,True],[True,True,False],[True,False,True],[True,False,False],[False,True,True],[False,True,False],[False,False,True],[False,False,False]]
-------------------------------

-- |
-- Problem 49
--
-- (**) Gray codes.
--
-- An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,
--
-- n = 1: C(1) = ['0','1'].
-- n = 2: C(2) = ['00','01','11','10'].
-- n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].
-- Find out the construction rules and write a predicate with the following specification:
--
-- % gray(N,C) :- C is the N-bit Gray code
-- Can you apply the method of "result caching" in order to make the predicate more efficient, when it is to be used repeatedly?
--
-- Example in Haskell:
--
-- >>> gray 3
-- ["000","001","011","010","110","111","101","100"]
gray :: Integral b => b -> [[Char]]
gray n = reverse $ transpose (map (\x -> take (2 ^ n) (cycle (trues x ++ falses x))) $ reverse colums)
  where
    colums = [1 .. n]
    bools = \x -> replicate ((2 ^ x) `div` 2)
    trues = (`bools` '1')
    falses = (`bools` '0')

-- |
-- Problem 50
--
-- (***) Huffman codes.
--
-- We suppose a set of symbols with their frequencies, given as a list of fr(S,F) terms.
--
-- Example: [fr(a,45),fr(b,13),fr(c,12),fr(d,16),fr(e,9),fr(f,5)].
--
-- Our objective is to construct a list hc(S,C) terms, where C is the Huffman code word for the symbol S.
-- In our example, the result could be Hs = [hc(a,'0'), hc(b,'101'), hc(c,'100'), hc(d,'111'), hc(e,'1101'), hc(f,'1100')] [hc(a,'01'),...etc.].
-- The task shall be performed by the predicate huffman/2 defined as follows:
--
-- % huffman(Fs,Hs) :- Hs is the Huffman code table for the frequency table Fs
--
-- Example in Haskell:
--
-- >>> huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
-- [('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]
--
--
-- TODO: below is what I have so far
-- *Main GHC.Integer.Logarithms> map ((/ 100) . snd) [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
-- [0.45,0.13,0.12,0.16,9.0e-2,5.0e-2]
-- *Main GHC.Integer.Logarithms> sum $ map ((/ 100) . snd) [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
-- 1.0
