module List3Spec where

import Lists3
import Test.Hspec

spec :: Spec
spec = do
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
