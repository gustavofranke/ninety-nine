module BTrees1Spec where

import BTrees1
import Test.Hspec

spec :: Spec
spec = do
  describe "Problem 56" $ do
    it "can symmetric" $ do
      symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty) `shouldBe` False
      symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)) `shouldBe` True

  describe "Problem 57" $ do
    it "can symmetric" $ do
      (symmetric . construct $ [5, 3, 18, 1, 4, 12, 21]) `shouldBe` True
      (symmetric . construct $ [3, 2, 5, 7, 1]) `shouldBe` True
