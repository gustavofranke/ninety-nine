module Miscellaneous2Spec where

import Miscellaneous2
import Test.Hspec

spec :: Spec
spec = do
  describe "Problem 95" $ do
    it "can fullWords" $ do
      fullWords 175 `shouldBe` "one-seven-five"

  describe "Problem 96" $ do
    it "can identifier" $ do
      identifier "this-is-a-long-identifier" `shouldBe` True
      identifier "this-ends-in-" `shouldBe` False
      identifier "two--hyphens" `shouldBe` False
