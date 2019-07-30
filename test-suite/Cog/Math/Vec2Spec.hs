{-# LANGUAGE MagicHash, BangPatterns #-}

module Cog.Math.Vec2Spec (
  spec
) where

import GHC.Exts
import Test.Tasty.Hspec

import Cog.Math.Vec2

spec :: Spec
spec = describe "Vec2" $ do
  parallel $ do
    describe "add" $ do
      it "can add two Vec2 together" $ do
        boxVec2 ((mkVec2 1 2) `add` (mkVec2 3 4)) `shouldBe` boxVec2 (mkVec2 4 6)

    describe "sub" $ do
      it "can subtract two Vec2" $ do
        boxVec2 ((mkVec2 1 2) `sub` (mkVec2 3 4)) `shouldBe` boxVec2 (mkVec2 (-2) (-2))

    describe "dot" $ do
      it "can dot two Vec2 together" $ do
        let !v = (mkVec2 1 2) `dot` (mkVec2 3 4)
        (F# v) `shouldBe` 11
