module List2Spec where

import Lists2
import Test.Hspec

spec :: Spec
spec = do
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
