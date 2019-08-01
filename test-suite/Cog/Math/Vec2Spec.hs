{-# LANGUAGE MagicHash, BangPatterns #-}

module Cog.Math.Vec2Spec (
  spec
) where

import GHC.Exts
import Test.Tasty.Hspec
import Prelude hiding (reverse)

import qualified Cog.Math.Vec2 as Vec2

spec :: Spec
spec = describe "Vec2" $ do
  parallel $ do
    describe "eq" $ do
      it "should return true if either a and be are the same object or have the same values" $ do
        let a = Vec2.new 1 2
        let b = Vec2.new 1 2
        let c = Vec2.new 3 4
        a `Vec2.eq` a `shouldBe` True
        a `Vec2.eq` b `shouldBe` True
        a `Vec2.eq` c `shouldBe` False

    describe "add" $ do
      it "should sum two vectors" $ do
        let !v = (Vec2.new 1 2) `Vec2.add` (Vec2.new (-2) 3)
        (v `Vec2.eq` Vec2.new (-1) 5) `shouldBe` True

    describe "sub" $ do
      it "should subtract two vectors" $ do
        let !v = (Vec2.new 1 2) `Vec2.sub` (Vec2.new (-2) 3)
        (v `Vec2.eq` Vec2.new 3 (-1)) `shouldBe` True

    describe "scale" $ do
      it "should scale vector with a factor" $ do
        let !v = (Vec2.new 1 2) `Vec2.scale` 2.0#
        (v `Vec2.eq` Vec2.new 2 4) `shouldBe` True

    describe "reverse" $ do
      it "should calcualte a vector with all its components reversed" $ do
        let !a = Vec2.reverse (Vec2.new 2 (-3))
        let !expected = Vec2.new (-2) 3
        (Vec2.box a) `shouldBe` (Vec2.box expected)

    describe "dot" $ do
      it "should calculate a dot product of two vectors" $ do
        let !d = (Vec2.new 1 2) `Vec2.dot` (Vec2.new 2 1)
        (F# d) `shouldBe` 4

    describe "cross" $ do
      it "should calculate a cross product of a vector A and the normal vector to the plane A is on" $ do
        let !v = Vec2.cross (Vec2.new 1 2)
        let !crossed = Vec2.new 2 (-1)
        (Vec2.box v) `shouldBe` (Vec2.box crossed)

    describe "lenSq" $ do
      it "should calculate a squared length of a vector" $ do
        let !l = Vec2.lenSq (Vec2.new 3 4)
        (F# l) `shouldBe` 25

    describe "len" $ do
      it "should calculate a squared length of a vector" $ do
        let !l = Vec2.len (Vec2.new 3 4)
        (F# l) `shouldBe` 5

    describe "normalize" $ do
      it "should calculate a normalized unit vector of a given vector" $ do
        let !n = Vec2.normalize (Vec2.new 4 0)
        let !expected = Vec2.new 1 0
        (Vec2.box n) `shouldBe` (Vec2.box expected)

    describe "distSq" $ do
      it "should calculate a squared length of a vector" $ do
        let !d = Vec2.distSq (Vec2.new 4 5) (Vec2.new 1 1)
        (F# d) `shouldBe` 25

    describe "dist" $ do
      it "should calculate a squared length of a vector" $ do
        let !d = Vec2.dist (Vec2.new 4 5) (Vec2.new 1 1)
        (F# d) `shouldBe` 5

    describe "lerp" $ do
      it "should calculate a linear-interpolated vector between to vectors" $ do
        let !p = Vec2.lerp (Vec2.new 1 1) (Vec2.new 3 3) 0.5#
        (Vec2.box p) `shouldBe` (Vec2.box (Vec2.new 2 2))
      it "should not extrapolate for t not in [0, 1]" $ do
        let !src = Vec2.new 0 0
        let !dst = Vec2.new 1 0
        (Vec2.box (Vec2.lerp src dst -0.5#)) `shouldBe` Vec2.box (Vec2.new 0 0)
        (Vec2.box (Vec2.lerp src dst 1.5#)) `shouldBe` Vec2.box (Vec2.new 1 0)

    describe "nlerp" $ do
      it "should calculate a normalized-linear-interpolated vector between to vectors" $ do
        let !p = Vec2.nlerp (Vec2.new 1 0) (Vec2.new 0 1) 0.5#
        (Vec2.box p) `shouldBe` Vec2.box(Vec2.new 0.7071067811865475 0.7071067811865475)
      it "should not extrapolate for t not in [0, 1]" $ do
        let !src = Vec2.new 2 0
        let !dst = Vec2.new 0 2
        (Vec2.box (Vec2.nlerp src dst -0.5#)) `shouldBe` Vec2.box (Vec2.new 1 0)
        (Vec2.box (Vec2.nlerp src dst 1.5#)) `shouldBe` Vec2.box (Vec2.new 0 1)

    describe "slerp" $ do
      it "should calculate a spherical-linear-interpolated vector between to vectors" $ do
        let !p = Vec2.slerp (Vec2.new 1 0) (Vec2.new 0 1) 0.5#
        (Vec2.box p) `shouldBe` Vec2.box(Vec2.new 0.7071067811865475 0.7071067811865475)
      it "should not extrapolate for t not in [0, 1]" $ do
        let !src = Vec2.new 2 0
        let !dst = Vec2.new 0 2
        (Vec2.box (Vec2.slerp src dst -0.5#)) `shouldBe` Vec2.box (Vec2.new 2 0)
        (Vec2.box (Vec2.slerp src dst 1.5#)) `shouldBe` Vec2.box (Vec2.new 0 2)

    describe "angle" $ do
      it "should calculate an angle of a vector to x-axis" $ do
        let !a = Vec2.angle (Vec2.new 1 1)
        (F# a) `shouldBe` 0.7853981633974484

    describe "angleTo" $ do
      it "should calculate an angle between two vectors" $ do
        let !a = Vec2.angleTo (Vec2.new 1 0) (Vec2.new 0 1)
        (F# a) `shouldBe` 1.5707963267948966

    describe "project" $ do
      it "should project A onto B" $ do
        let !p = Vec2.project (Vec2.new 1 1) (Vec2.new 2 1)
        (Vec2.box p) `shouldBe` Vec2.box (Vec2.new 1.2 0.6)
