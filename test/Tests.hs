module Main where

import Lists1
import Lists2
import Lists3
import Arithmetic

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Problem 1" $ do

    it "can myLast [1,2,3,4]" $ do
      myLast [1,2,3,4] `shouldBe` (4 :: Float)    

    it "can myLast ['x','y','z']" $ do
      myLast ['x','y','z'] `shouldBe` 'z'     
  
  describe "Problem 3" $ do

    it "can elementAt [1,2,3] 2" $ do
      elementAt [1,2,3] 2 `shouldBe` (2 :: Integer)

    it "can elementAt \"haskell\" 5" $ do
      elementAt "haskell" 5 `shouldBe` 'e'        

  -- describe "Problem 4" $ do

  --   it "can myLength [123, 456, 789]" $ do
  --     myLength [123, 456, 789] `shouldBe` 3

  --   it "can myLength \"Hello, world!\"" $ do
  --     myLength "Hello, world!" `shouldBe` 13

  describe "Problem 7" $ do

    it "can flatten (Elem 5)" $ do
      flatten (Elem 5) `shouldBe` ([5] :: [Int])

    it "can flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])" $ do
      flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) `shouldBe` ([1,2,3,4,5] :: [Int])    

    -- it "can flatten (List [])" $ do
    --   flatten (List []) `shouldBe` [] 

  describe "Problem 8" $ do

    it "can compress \"aaaabccaadeeee\"" $ do
      compress "aaaabccaadeeee" `shouldBe` "abcade"

  describe "Problem 9" $ do

    it "can pack chars into strings" $ do
      pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'] `shouldBe` ["aaaa","b","cc","aa","d","eeee"]

  describe "Problem 10" $ do

    it "can encode strings" $ do
      encode "aaaabccaadeeee" `shouldBe` [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
------------------------------------------
  -- describe "Problem 11" $ do

  --   it "can compress \"aaaabccaadeeee\"" $ do
  --     encodeModified "aaaabccaadeeee" `shouldBe` [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']

  describe "Problem 14" $ do

    it "can dupli [1, 2, 3]" $ do
      dupli [1, 2, 3] `shouldBe` ([1,1,2,2,3,3] :: [Int])
 
  describe "Problem 15" $ do

    it "can repli \"abc\" 3" $ do
      repli "abc" 3 `shouldBe` "aaabbbccc"

  describe "Problem 16" $ do

    it "can dropEvery \"abcdefghik\" 3" $ do
      dropEvery "abcdefghik" 3 `shouldBe` "abdeghk"

  -- describe "Problem 17" $ do

  --   it "can split \"abcdefghik\" 3" $ do
  --     dropEvery "abcdefghik" 3 `shouldBe` ("abc", "defghik")

  describe "Problem 18" $ do

    it "can slice ['a','b','c','d','e','f','g','h','i','k'] 3 7" $ do
      slice ['a','b','c','d','e','f','g','h','i','k'] 3 7 `shouldBe` "cdefg"

  describe "Problem 19" $ do

    it "can rotate ['a','b','c','d','e','f','g','h'] 3" $ do
      rotate ['a','b','c','d','e','f','g','h'] 3 `shouldBe` "defghabc"

    it "can rotate ['a','b','c','d','e','f','g','h'] (-2)" $ do
      rotate ['a','b','c','d','e','f','g','h'] (-2) `shouldBe` "ghabcdef"

  describe "Problem 20" $ do

    it "can removeAt 2 \"abcd\"" $ do
      removeAt 2 "abcd" `shouldBe` ('b',"acd")
-----------------------------

  describe "Problem 21" $ do

    it "can insertAt 'X' \"abcd\" 2" $ do
      insertAt 'X' "abcd" 2 `shouldBe` "aXbcd"

  describe "Problem 22" $ do

    it "can range 4 9" $ do
      range 4 9 `shouldBe` [4,5,6,7,8,9]

  -- describe "Problem 23" $ do

  --   it "can rndSelect \"abcdefgh\" 3" $ do
  --     rndSelect "abcdefgh" 3 `shouldBe` "eda"

  -- describe "Problem 24" $ do

  --   it "can diffSelect 6 49" $ do
  --     diffSelect 6 49 `shouldBe` [23,1,17,33,21,37]

  -- describe "Problem 25" $ do

  --   it "can rndPermu \"abcdef\"" $ do
  --     rndPermu "abcdef" `shouldBe` "badcef"

  -- describe "Problem 28" $ do

  --   it "can lfsort" $ do
  --     lsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"] `shouldBe` ["ijkl","o","abc","fgh","de","de","mn"]
-----------------------

  describe "Problem 31" $ do

    it "can isPrime" $ do
      isPrime 7    `shouldBe` True
      isPrime 3049 `shouldBe` True
      isPrime 3137 `shouldBe` True
      isPrime 3461 `shouldBe` True
      filter (isPrime) [-100..199] `shouldBe` [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,169,173,179,181,191,193,197,199]

  describe "Problem 32" $ do

    it "can myGCD" $ do
      myGCD 36 63     `shouldBe` 9
      -- myGCD (-3) (-6) `shouldBe` 3
      myGCD (-3) 6    `shouldBe` 3
  