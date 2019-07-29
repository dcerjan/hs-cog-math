{-# LANGUAGE MagicHash #-}

module Cog.Math.Vec2
  ( module Cog.Math.Vec2
  , module Cog.Math.Vec
) where

import GHC.Prim (Float#)
import Prelude hiding (reverse)
import Cog.Math.Vec


data Vec2 = Vec2 Float# Float# deriving (Show, Eq)

cross :: Vec2 -> Vec2
cross (Vec2 x y) = Vec2 (-y) x

instance Vec Vec2 where
  add (Vec2 ax ay) (Vec2 bx by) = Vec2 (ax +## bx) (ay +## by)
  sub (Vec2 ax ay) (Vec2 bx by) = Vec2 (ax - bx) (ay - by)
  scale (Vec2 x y) s = Vec2 (x * s) (y * s)
  dot (Vec2 ax ay) (Vec2 bx by) = ax * bx +## ay * by
  reverse (Vec2 x y) = Vec2 (-x) (-y)
  lenSq (Vec2 x y) = x ** 2 +## y ** 2
  distSq (Vec2 ax ay) (Vec2 bx by) = dx ** 2 +## dy ** 2
    where
      dx = bx - ax
      dy = by - ay
  angle v@(Vec2 x _) = acos $ x / l
    where l = len v
  angleTo a b = acos $ d / l
    where
      d = a `dot` b
      l = (len a) * (len b)
  lerp src@(Vec2 srcX srcY) dst@(Vec2 dstX dstY) t
    | t <= 0 = src
    | t >= 1 = dst
    | otherwise = Vec2 x y
      where
        x = srcX +## (dstX - srcX) * t
        y = srcY +## (dstY - srcY) * t
  slerp src dst t
    | t <= 0 = src
    | t >= 1 = dst
    | otherwise = pSrc `add` pDst
      where
        omega = src `angleTo` dst
        sinOmega = sin omega
        tOmega = t * omega
        fSrc = (sin (omega - tOmega)) / sinOmega
        fDst = (sin tOmega) / sinOmega
        pSrc = src `scale` fSrc
        pDst = dst `scale` fDst
