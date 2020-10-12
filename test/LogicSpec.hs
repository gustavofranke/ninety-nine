module LogicSpec where

import Logic
import Test.Hspec

spec :: Spec
spec = do
  describe "Problem 46" $ do

    it "can table" $ do
      table (\a b -> (and' a (or' a b))) `shouldBe` [[True,True,True],[True,False,True],[False,True,False],[False,False,False]]
      table (\a b -> a `and'` (a `or'` not b)) `shouldBe` [[True,True,True],[True,False,True],[False,True,False],[False,False,False]]