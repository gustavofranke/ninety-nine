-- | Questions 11 to 20: Lists, continued
-- | https://wiki.haskell.org/99_questions/11_to_20
module Lists2 where

import Lists1

-- |
-- Problem 11
--
-- (*) Modified run-length encoding.
--
-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
--
-- Example:
--
-- * (encode-modified '(a a a a b c c a a d e e e e))
-- ((4 A) B (2 C) (2 A) D (4 E))
--
-- Example in Haskell:
--
-- >>> encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
data EncodeModified a = Multiple Int a | Single a deriving (Eq, Show)

encodeModified :: Eq a => [a] -> [EncodeModified a]
encodeModified x = map func (encode x)
  where
    func t = if fst t == 1 then Single (snd t) else uncurry Multiple t -- uncurry Multiple t == Multiple (fst t) (snd t)

-- |
-- Problem 12
--
-- (**) Decode a run-length encoded list.
--
-- Given a run-length code list generated as specified in problem 11.
-- Construct its uncompressed version.
--
-- Example in Haskell:
--
-- >>> decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"
decodeModified :: Eq a => [EncodeModified a] -> [a]
decodeModified [] = []
decodeModified ((Single x) : xs) = x : decodeModified xs
decodeModified ((Multiple i x) : xs) = replicate i x ++ decodeModified xs

-- |
-- Problem 13
--
-- (**) Run-length encoding of a list (direct solution).
--
-- Implement the so-called run-length encoding data compression method directly.
-- I.e. don't explicitly create the sublists containing the duplicates,
-- as in problem 9, but only count them.
-- As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
--
-- Example:
--
-- * (encode-direct '(a a a a b c c a a d e e e e))
-- ((4 A) B (2 C) (2 A) D (4 E))
-- Example in Haskell:
--
-- >>> encodeDirect "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
-- >>> encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
encodeDirect :: Eq a => [a] -> [EncodeModified a]
encodeDirect = encodeModified

-- |
-- Problem 14
--
-- (*) Duplicate the elements of a list.
--
-- Example:
--
-- * (dupli '(a b c c d))
-- (A A B B C C C C D D)
--
-- Example in Haskell:
--
-- >>> dupli [1, 2, 3]
-- [1,1,2,2,3,3]
dupli :: [a] -> [a]
dupli = foldr (\a b -> a : a : b) []

dupli' :: [a] -> [a]
dupli' xs = reverse $ foldl (\b a -> a : a : b) [] xs

dupli'' :: Enum a => [a] -> [a]
dupli'' xs = xs >>= (\x -> take 2 [x, x ..])

-- |
-- Problem 15
--
-- (**) Replicate the elements of a list a given number of times.
--
-- Example:
--
-- * (repli '(a b c) 3)
-- (A A A B B B C C C)
--
-- Example in Haskell:
--
-- >>> repli "abc" 3
-- "aaabbbccc"
repli :: Enum a => [a] -> Int -> [a]
repli xs i = foldr (\a b -> take i [a, a ..] ++ b) [] xs

repli' :: Enum a => [a] -> Int -> [a]
repli' xs i = xs >>= (\a -> take i [a, a ..])

-- |
-- Problem 16
--
-- (**) Drop every N'th element from a list.
--
-- Example:
--
-- * (drop '(a b c d e f g h i k) 3)
-- (A B D E G H K)
--
-- Example in Haskell:
--
-- >>> dropEvery "abcdefghik" 3
-- "abdeghk"
dropEvery :: [a] -> Int -> [a]
-- dropEvery xs i = map (fst) (filter (\e -> snd e /= i) (zip xs $ cycle [1..i]))
dropEvery xs i = map fst (filter ((/= i) . snd) (zip xs $ cycle [1 .. i]))

-- |
-- Problem 17
--
-- (*) Split a list into two parts; the length of the first part is given.
--
-- Do not use any predefined predicates.
--
-- Example:
--
-- * (split '(a b c d e f g h i k) 3)
-- ( (A B C) (D E F G H I K))
--
-- Example in Haskell:
--
-- >>> split "abcdefghik" 3
-- ("abc", "defghik")
split :: [a] -> Int -> ([a], [a])
split xs i = (take i xs, drop i xs)

-- |
-- Problem 18
--
-- (**) Extract a slice from a list.
--
-- Given two indices, i and k,
-- the slice is the list containing the elements between the i'th and k'th element of
-- the original list (both limits included).
-- Start counting the elements with 1.
--
-- Example:
--
-- * (slice '(a b c d e f g h i k) 3 7)
-- (C D E F G)
--
-- Example in Haskell:
--
-- >>> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
-- "cdefg"
slice :: [a] -> Int -> Int -> [a]
slice xs i k = map fst $ filter (\x -> snd x >= i && snd x <= k) $ zip xs [1 ..]

-- |
-- Problem 19
--
-- (**) Rotate a list N places to the left.
-- Hint: Use the predefined functions length and (++).
--
-- Examples:
-- * (rotate '(a b c d e f g h) 3)
-- (D E F G H A B C)
--
-- * (rotate '(a b c d e f g h) -2)
-- (G H A B C D E F)
--
-- Examples in Haskell:
--
-- >>> rotate ['a','b','c','d','e','f','g','h'] 3
-- "defghabc"
--
-- >>> rotate ['a','b','c','d','e','f','g','h'] (-2)
-- "ghabcdef"
rotate :: [a] -> Int -> [a]
rotate xs i
  | i > 0 = drop i xs ++ take i xs
  | i < 0 = drop (y + i) xs ++ take (y + i) xs
  | i == 0 = xs
  where
    y = length xs

-- |
-- Problem 20
--
-- (*) Remove the K'th element from a list.
--
-- Example in Prolog:
--
-- @
-- ?- remove_at(X,[a,b,c,d],2,R).
-- X = b
-- R = [a,c,d]
-- @
--
-- Example in Lisp:
--
-- @
-- (remove-at '(a b c d) 2)
-- (A C D)
-- @
--
-- (Note that this only returns the residue list,
-- while the Prolog version also returns the deleted element.)
--
-- Example in Haskell:
--
-- >>> removeAt 2 "abcd"
-- ('b',"acd")
removeAt :: Int -> [a] -> (a, [a])
-- removeAt i xs = (head $ map (fst) $ filter (\t -> snd t == i) (zip xs [1..]),
--                         map (fst) $ filter (\t -> snd t /= i) (zip xs [1..]))
removeAt i xs = (head $ func (\t -> snd t == i), func (\t -> snd t /= i))
  where
    func f = map fst $ filter f (zip xs [1 ..])
