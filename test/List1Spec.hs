module List1Spec where

import Lists1
import Test.Hspec

spec :: Spec
spec = do
  describe "Problem 1" $ do

    it "can myLast [1,2,3,4]" $ do
      myLast [1,2,3,4] `shouldBe` (4 :: Float)    

    it "can myLast ['x','y','z']" $ do
      myLast ['x','y','z'] `shouldBe` 'z'     

  describe "Problem 2" $ do

    it "can myButLast [1,2,3,4]" $ do
      myButLast [1,2,3,4] `shouldBe` (3 :: Int)    

    it "can myLmyButLastast ['x','y','z']" $ do
      myButLast ['x','y','z'] `shouldBe` 'y'   
  describe "Problem 3" $ do

    it "can elementAt [1,2,3] 2" $ do
      elementAt [1,2,3] 2 `shouldBe` (2 :: Integer)

    it "can elementAt \"haskell\" 5" $ do
      elementAt "haskell" 5 `shouldBe` 'e'        

  describe "Problem 4" $ do

    it "can myLength [123, 456, 789]" $ do
      myLength [123 :: Int, 456, 789] `shouldBe` 3

    it "can myLength \"Hello, world!\"" $ do
      myLength "Hello, world!" `shouldBe` 13

  describe "Problem 5" $ do

    it "can myReverse" $ do
      myReverse "A man, a plan, a canal, panama!" `shouldBe` "!amanap ,lanac a ,nalp a ,nam A"
      myReverse [1,2,3,4] `shouldBe` ([4,3,2,1]::[Int])

  describe "Problem 6" $ do

    it "can isPalindrome" $ do
      isPalindrome [1::Int,2,3] `shouldBe` False
      isPalindrome "madamimadam" `shouldBe` True
      isPalindrome [1::Int,2,4,8,16,8,4,2,1] `shouldBe` True

  describe "Problem 7" $ do

    it "can flatten (Elem 5)" $ do
      flatten (Elem 5) `shouldBe` ([5] :: [Int])

    it "can flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])" $ do
      flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) `shouldBe` ([1,2,3,4,5] :: [Int])    

    it "can flatten (List [])" $ do
      flatten (List []) `shouldBe` ([] :: [Int])

  describe "Problem 8" $ do

    it "can compress \"aaaabccaadeeee\"" $ do
      compress "aaaabccaadeeee" `shouldBe` "abcade"

  describe "Problem 9" $ do

    it "can pack chars into strings" $ do
      pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'] `shouldBe` ["aaaa","b","cc","aa","d","eeee"]

  describe "Problem 10" $ do

    it "can encode strings" $ do
      encode "aaaabccaadeeee" `shouldBe` [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
