module MTreesSpec where

import MTrees
import Test.Hspec

spec :: Spec
spec = do
  describe "Problem 70C" $ do

    it "can nnodes" $ do
      nnodes mtree2 `shouldBe` 2

  describe "Problem 70" $ do
    -- it "can stringToTree" $ do
    --   stringToTree "afg^^c^bd^e^^^" `shouldBe` Node 'a' [Node 'f' [Node 'g' []],Node 'c' [],Node 'b' [Node 'd' [],Node 'e' []]]
    it "can treeToString" $ do
      treeToString (Node 'a' [Node 'f' [Node 'g' []],Node 'c' [],Node 'b' [Node 'd' [],Node 'e' []]]) `shouldBe` "afg^^c^bd^e^^^"