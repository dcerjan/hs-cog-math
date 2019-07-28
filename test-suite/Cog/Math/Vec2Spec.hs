module Cog.Math.Vec2Spec (
  spec
) where

import Test.Tasty.Hspec

import Cog.Math.Vec2

spec :: Spec
spec = parallel $ do
  describe "Vec2" $ do
    describe "add" $ do
      it "can add two Vec2 together" $ do
        Vec2 1 2 `add` Vec2 3 4 `shouldBe` Vec2 4 6

    describe "sub" $ do
      it "can subtract two Vec2" $ do
        Vec2 1 2 `sub` Vec2 3 4 `shouldBe` Vec2 (-2) (-2)