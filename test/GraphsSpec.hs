module GraphsSpec where

import Graphs
import Test.Hspec

spec :: Spec
spec = do
  describe "Problem 80" $ do

    it "can graphToAdj" $ do
      graphToAdj graph1 `shouldBe` adjac1