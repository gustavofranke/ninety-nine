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

  describe "Problem 33" $ do

    it "can coprime" $ do
      coprime 35 64 `shouldBe` True

  describe "Problem 34" $ do

    it "can totient 10" $ do
      totient 10 `shouldBe` 4
      totient 1009 `shouldBe` 1008

  describe "Problem 35" $ do

    it "can primeFactors 315" $ do
      primeFactors 315 `shouldBe` [3, 3, 5, 7]

  describe "Problem 36" $ do

    it "can primeFactorsMult 315" $ do
      primeFactorsMult 315 `shouldBe` [(3,2),(5,1),(7,1)]

  describe "Problem 37" $ do

    it "can phi 10090" $ do
      phi 10090 `shouldBe` 1008

  describe "Problem 39" $ do

    it "can primesR 10 20" $ do
      primesR 10 20 `shouldBe` [11,13,17,19]

  describe "Problem 40" $ do

    it "can goldbach 28" $ do
      goldbach 28 `shouldBe` (5, 23)

--   describe "Problem 41" $ do

--     it "can goldbachList" $ do
--       goldbachList 9 20 `shouldBe` [(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)]
