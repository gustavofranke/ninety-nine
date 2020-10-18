-- | Questions 95 to 99: Miscellaneous problems, continued
-- https://wiki.haskell.org/99_questions/95_to_99
module Miscellaneous2 where

import Data.List
import qualified Data.Map as Map
import Data.Maybe

-- |
-- Problem 95
--
-- (**) English number words
--
-- On financial documents, like cheques, numbers must sometimes be written in full words.
--
-- Example: 175 must be written as one-seven-five.
--
-- Write a predicate full-words/1 to print (non-negative) integer numbers in full words.
--
-- Example in Haskell:
--
-- >>> fullWords 175
-- one-seven-five
fullWords :: Int -> [Char]
fullWords i = format
  where
    soluc = key <$> show i
    key k = Map.lookup k nums
    nums =
      Map.fromList
        [ ('0', "zero"),
          ('1', "one"),
          ('2', "two"),
          ('3', "three"),
          ('4', "four"),
          ('5', "five"),
          ('6', "six"),
          ('7', "seven"),
          ('8', "eight"),
          ('9', "nine")
        ]
    format = intercalate "-" $ catMaybes soluc

-- |
-- Problem 96
--
-- (**) Syntax checker
--
-- In a certain programming language (Ada) identifiers are defined by the syntax diagram below.
--
-- https://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/p96.gif
--
-- @
-- identifier
-- ------------> letter ------------------------------->
--                      \------------------letter----/
--                         |  \__ (-) __/ \_digit__/|
--                         |________________________|
-- @
--
-- Transform the syntax diagram into a system of syntax diagrams which do not contain loops;
-- i.e. which are purely recursive.
--
-- Using these modified diagrams, write a predicate identifier/1
-- that can check whether or not a given string is a legal identifier.
--
-- Example in Prolog:
--
-- % identifier(Str) :- Str is a legal identifier
-- Example in Haskell:
--
-- >>> identifier "this-is-a-long-identifier"
-- True
-- >>> identifier "this-ends-in-"
-- False
-- >>> identifier "two--hyphens"
-- False

-- data Component =
--                   Letter Char
--                 | Digit Char
--                 | Hyphen

-- newtype Identifier = Identifier (Letter Nel.:| [Component])
-- next :: Component -> Component
-- next (Letter l) = undefined
-- next (Digit c) = undefined
-- next Hyphen = undefined

identifier :: [Char] -> Bool
identifier str = null errors
  where
    tuples = zip str (tail str)
    errors = invalids `intersect` tuples
    invalids =
      [ ('-', '-'),
        ('n', '-')
      ]

-- |
-- Problem 97
--
-- (**) Sudoku
--
-- https://norvig.com/sudoku.html
--
-- Sudoku puzzles go like this:
--   Problem statement                 Solution
--    .  .  4 | 8  .  . | .  1  7     9  3  4 | 8  2  5 | 6  1  7
--    6  7  . | 9  .  . | .  .  .     6  7  2 | 9  1  4 | 8  5  3
--    5  .  8 | .  3  . | .  .  4     5  1  8 | 6  3  7 | 9  2  4
--    --------+---------+--------     --------+---------+--------
--    3  .  . | 7  4  . | 1  .  .     3  2  5 | 7  4  8 | 1  6  9
--    .  6  9 | .  .  . | 7  8  .     4  6  9 | 1  5  3 | 7  8  2
--    .  .  1 | .  6  9 | .  .  5     7  8  1 | 2  6  9 | 4  3  5
--    --------+---------+--------     --------+---------+--------
--    1  .  . | .  8  . | 3  .  6     1  9  7 | 5  8  2 | 3  4  6
--    .  .  . | .  .  6 | .  9  1     8  5  3 | 4  7  6 | 2  9  1
--    2  4  . | .  .  1 | 5  .  .     2  4  6 | 3  9  1 | 5  7  8
--
-- Every spot in the puzzle belongs to a (horizontal) row and a (vertical) column,
-- as well as to one single 3x3 square (which we call "square" for short).
--
-- At the beginning, some of the spots carry a single-digit number between 1 and 9.
--
-- The problem is to fill the missing spots with digits in such a way that every
-- number between 1 and 9 appears exactly once in each row, in each column, and in each square.
