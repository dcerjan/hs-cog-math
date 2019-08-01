{-# LANGUAGE MagicHash, BangPatterns #-}

module Cog.Math.Vec2Spec (
  spec
) where

import GHC.Exts
import Test.Tasty.Hspec
import Prelude hiding (reverse)

import Cog.Math.Vec2

spec :: Spec
spec = describe "Vec2" $ do
  parallel $ do
    describe "equals" $ do
      it "should return true if either a and be are the same object or have the same values" $ do
        let a = newVec2 1 2
        let b = newVec2 1 2
        let c = newVec2 3 4
        a `equals` a `shouldBe` True
        a `equals` b `shouldBe` True
        a `equals` c `shouldBe` False

    describe "add" $ do
      it "should sum two vectors" $ do
        let !v = (newVec2 1 2) `add` (newVec2 (-2) 3)
        (v `equals` newVec2 (-1) 5) `shouldBe` True

    describe "sub" $ do
      it "should subtract two vectors" $ do
        let !v = (newVec2 1 2) `sub` (newVec2 (-2) 3)
        (v `equals` newVec2 3 (-1)) `shouldBe` True

    describe "scale" $ do
      it "should scale vector with a factor" $ do
        let !v = (newVec2 1 2) `scale` 2.0#
        (v `equals` newVec2 2 4) `shouldBe` True

    describe "reverse" $ do
      it "should calcualte a vector with all its components reversed" $ do
        let !a = reverse (newVec2 2 (-3))
        let !expected = newVec2 (-2) 3
        (boxVec2 a) `shouldBe` (boxVec2 expected)

    describe "dot" $ do
      it "should calculate a dot product of two vectors" $ do
        let !d = (newVec2 1 2) `dot` (newVec2 2 1)
        (F# d) `shouldBe` 4

    describe "cross" $ do
      it "should calculate a cross product of a vector A and the normal vector to the plane A is on" $ do
        let !v = cross (newVec2 1 2)
        let !crossed = newVec2 2 (-1)
        (boxVec2 v) `shouldBe` (boxVec2 crossed)

    describe "lenSq" $ do
      it "should calculate a squared length of a vector" $ do
        let !l = lenSq (newVec2 3 4)
        (F# l) `shouldBe` 25

    describe "len" $ do
      it "should calculate a squared length of a vector" $ do
        let !l = len (newVec2 3 4)
        (F# l) `shouldBe` 5

    describe "normalize" $ do
      it "should calculate a normalized unit vector of a given vector" $ do
        let !n = normalize (newVec2 4 0)
        let !expected = newVec2 1 0
        (boxVec2 n) `shouldBe` (boxVec2 expected)

    describe "distSq" $ do
      it "should calculate a squared length of a vector" $ do
        let !d = distSq (newVec2 4 5) (newVec2 1 1)
        (F# d) `shouldBe` 25

    describe "dist" $ do
      it "should calculate a squared length of a vector" $ do
        let !d = dist (newVec2 4 5) (newVec2 1 1)
        (F# d) `shouldBe` 5

    describe "lerp" $ do
      it "should calculate a linear-interpolated vector between to vectors" $ do
        let !p = lerp (newVec2 1 1) (newVec2 3 3) 0.5#
        (boxVec2 p) `shouldBe` (boxVec2 (newVec2 2 2))
      it "should not extrapolate for t not in [0, 1]" $ do
        let !src = newVec2 0 0
        let !dst = newVec2 1 0
        (boxVec2 (lerp src dst -0.5#)) `shouldBe` boxVec2 (newVec2 0 0)
        (boxVec2 (lerp src dst 1.5#)) `shouldBe` boxVec2 (newVec2 1 0)

    describe "nlerp" $ do
      it "should calculate a normalized-linear-interpolated vector between to vectors" $ do
        let !p = nlerp (newVec2 1 0) (newVec2 0 1) 0.5#
        (boxVec2 p) `shouldBe` boxVec2(newVec2 0.7071067811865475 0.7071067811865475)
      it "should not extrapolate for t not in [0, 1]" $ do
        let !src = newVec2 2 0
        let !dst = newVec2 0 2
        (boxVec2 (nlerp src dst -0.5#)) `shouldBe` boxVec2 (newVec2 1 0)
        (boxVec2 (nlerp src dst 1.5#)) `shouldBe` boxVec2 (newVec2 0 1)

    describe "slerp" $ do
      it "should calculate a spherical-linear-interpolated vector between to vectors" $ do
        let !p = slerp (newVec2 1 0) (newVec2 0 1) 0.5#
        (boxVec2 p) `shouldBe` boxVec2(newVec2 0.7071067811865475 0.7071067811865475)
      it "should not extrapolate for t not in [0, 1]" $ do
        let !src = newVec2 2 0
        let !dst = newVec2 0 2
        (boxVec2 (slerp src dst -0.5#)) `shouldBe` boxVec2 (newVec2 2 0)
        (boxVec2 (slerp src dst 1.5#)) `shouldBe` boxVec2 (newVec2 0 2)

    describe "angle" $ do
      it "should calculate an angle of a vector to x-axis" $ do
        let !a = angle (newVec2 1 1)
        (F# a) `shouldBe` 0.7853981633974484

    describe "angleTo" $ do
      it "should calculate an angle between two vectors" $ do
        let !a = angleTo (newVec2 1 0) (newVec2 0 1)
        (F# a) `shouldBe` 1.5707963267948966

    describe "project" $ do
      it "should project A onto B" $ do
        let !p = project (newVec2 1 1) (newVec2 2 1)
        (boxVec2 p) `shouldBe` boxVec2 (newVec2 1.2 0.6)
