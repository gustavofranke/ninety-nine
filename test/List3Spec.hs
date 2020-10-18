module List3Spec where

import qualified Data.Set as Set
import Lists3
import Test.Hspec

spec :: Spec
spec = do
  describe "Problem 21" $ do
    it "can insertAt 'X' \"abcd\" 2" $ do
      insertAt 'X' "abcd" 2 `shouldBe` "aXbcd"

  describe "Problem 22" $ do
    it "can range 4 9" $ do
      range 4 9 `shouldBe` [4, 5, 6, 7, 8, 9]

  describe "Problem 23" $ do
    it "can rndSelect \"abcdefgh\" 3" $ do
      let list = "abcdefgh"
      let obtained = Set.fromList $ rndSelect list 3
      let expected = Set.fromList list `Set.intersection` obtained
      obtained `shouldBe` expected

  describe "Problem 24" $ do
    it "can diffSelect 6 49" $ do
      let first = 6
      let second = 49
      length (diffSelect first second) `shouldBe` first
  -- max (diffSelect first second) `shouldSatisfy` (second <=)
  -- min (diffSelect first second) `shouldSatisfy` (>= 1)

  describe "Problem 25" $ do
    it "can rndPermu \"abcdef\"" $ do
      Set.fromList (rndPermu "abcdef") `shouldBe` Set.fromList "abcdef"

  describe "Problem 28" $ do
    it "can lsort" $ do
        lsort ["abc","de","fgh","de","ijkl","mn","o"] `shouldBe` ["o","de","de","mn","abc","fgh","ijkl"]
        lsort' ["abc","de","fgh","de","ijkl","mn","o"] `shouldBe` ["o","de","de","mn","abc","fgh","ijkl"]
--   it "can lfsort" $ do
--     lsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"] `shouldBe` ["ijkl","o","abc","fgh","de","de","mn"]
