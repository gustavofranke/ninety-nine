-- | Questions 1 to 10: Lists
-- | https://wiki.haskell.org/99_questions/1_to_10
module Lists1 where

{-|
 Problem 1

 (*) Find the last element of a list.

 (Note that the Lisp transcription of this problem is incorrect.)

 Example in Haskell:

 >>> myLast [1,2,3,4]
 4

 >>> myLast ['x','y','z']
 'z'
-}
myLast :: [a] -> a
myLast [] = error "error!"
myLast [x] = x
myLast (_:xs) = myLast xs

myLast' :: [a] -> a
myLast' [] = error "error!"
myLast' x = (head . reverse) x

{-|
 Problem 2

 (*) Find the last but one element of a list.
-}
myButLast :: [a] -> a
myButLast []  = error "error!"
myButLast [q] = error "error!"
myButLast x   = (last . init) x

myButLast' :: [a] -> a
myButLast' []  = error "error!"
myButLast' [q] = error "error!"
myButLast' x   = (head. reverse . init) x

{-|
 Problem 3

 (*) Find the K'th element of a list. The first element in the list is number 1.
 
 Example:
 
 * (element-at '(a b c d e) 3)
 c

 Example in Haskell:
 
 >>> elementAt [1,2,3] 2
 2
 >>> elementAt "haskell" 5
 'e'
-}
elementAt :: [a] -> Int -> a
elementAt as i = fst $ last $ zip as [1..i]
-- last $ take 5 "haskell"
-- 'e'

{-|
 Problem 4

 (*) Find the number of elements of a list.
 
 Example in Haskell:
 
 >>> myLength [123, 456, 789]
 3
 >>> myLength "Hello, world!"
 13
-}
myLength :: [a] -> Int
myLength = sum . map (\_ -> 1)

{-|
 Problem 5

 (*) Reverse a list.
 
 Example in Haskell:
 
 >>> myReverse "A man, a plan, a canal, panama!"
 "!amanap ,lanac a ,nalp a ,nam A"
 >>> myReverse [1,2,3,4]
 [4,3,2,1]
-}
myReverse :: [a] -> [a]
myReverse = foldl (\a b -> b:a) []

{-|
 Problem 6

 (*) Find out whether a list is a palindrome.
 A palindrome can be read forward or backward; e.g. (x a m a x).
 
 Example in Haskell:
 
 >>> isPalindrome [1,2,3]
 False
 >>> isPalindrome "madamimadam"
 True
 >>> isPalindrome [1,2,4,8,16,8,4,2,1]
 True
-}
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = if xs == reverse xs then True else False

{-|
 Problem 7
 (**) Flatten a nested list structure.
 
 Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
 
 Example:
 
 * (my-flatten '(a (b (c d) e)))
 (A B C D E)
 
 Example in Haskell:
 
 We have to define a new data type, because lists in Haskell are homogeneous.
 
  data NestedList a = Elem a | List [NestedList a]
 >>> flatten (Elem 5)
 [5]
 >>> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
 [1,2,3,4,5]
 >>> flatten (List [])
 []
-}
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (List [])     = []
flatten (Elem l) = [l]
flatten (List (l:ls)) = flatten l ++ flatten (List ls)

{-|
 Problem 8
 (**) Eliminate consecutive duplicates of list elements.
 
 If a list contains repeated elements they should be replaced with a single copy of the element.
The order of the elements should not be changed.
 
 Example:
 
 * (compress '(a a a a b c c a a d e e e e))
 (A B C A D E)
 
 Example in Haskell:
 
 >>> compress "aaaabccaadeeee"
 "abcade"
-}
compress :: Eq a => [a] -> [a]
compress x = reverse $ foldl (\a b -> if (head a) == b then a else b:a) [head x] x

{-|
 Problem 9

 (**) Pack consecutive duplicates of list elements into sublists.
 If a list contains repeated elements they should be placed in separate sublists.

 Example:

 * (pack '(a a a a b c c a a d e e e e))
 ((A A A A) (B) (C C) (A A) (D) (E E E E))
 
 Example in Haskell:

 >>> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
 ["aaaa","b","cc","aa","d","eeee"]
-}
pack :: Eq a => [a] -> [[a]]
pack [] = []
-- pack x = takeWhile (\e -> e == (head x)) x : (pack $ dropWhile (\e -> e == (head x)) x)
pack x = takeWhile cond x : (pack $ dropWhile cond x)
    -- where cond = (\e -> e == (head x))
    where cond = (== (head x))

{-|
 Problem 10

 (*) Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

 Example:

 * (encode '(a a a a b c c a a d e e e e))
 ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
 
 Example in Haskell:

 >>> encode "aaaabccaadeeee"
 [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
-}
encode :: Eq a => [a] -> [(Int, a)]
encode x = map (\p -> ((length p), (head p))) (pack x)