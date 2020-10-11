module ArithmeticSpec where

import Arithmetic
import Test.Hspec

spec :: Spec
spec = do
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