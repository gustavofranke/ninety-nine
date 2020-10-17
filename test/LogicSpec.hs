module LogicSpec where

import qualified Data.Set as Set
import Logic
import Test.Hspec

spec :: Spec
spec = do
  describe "Problem 46" $ do
    it "can table" $ do
      table (\a b -> and' a (or' a b)) `shouldBe` [[True, True, True], [True, False, True], [False, True, False], [False, False, False]]
      table (\a b -> a `and'` (a `or'` not b)) `shouldBe` [[True, True, True], [True, False, True], [False, True, False], [False, False, False]]

  describe "Problem 49" $ do
    it "can gray 3" $ do
      Set.fromList (gray (3 :: Int)) `shouldBe` Set.fromList ["000", "001", "011", "010", "110", "111", "101", "100"]
