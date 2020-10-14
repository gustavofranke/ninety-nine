module BTrees2Spec where

import BTrees1
import BTrees2
import Test.Hspec

spec :: Spec
spec = do
  describe "Problem 61" $ do

    it "can countLeaves" $ do
      countLeaves tree4 `shouldBe` 2

  describe "Problem 61A" $ do

    it "can leaves" $ do
      leaves tree4 `shouldBe` [4,2]

  describe "Problem 62" $ do

    it "can internals" $ do
      internals tree4 `shouldBe` [1,2]

  describe "Problem 62B" $ do

    it "can atLevel" $ do
      atLevel tree4 2 `shouldBe` [2,2]