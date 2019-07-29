{-# LANGUAGE MagicHash, BangPatterns #-}

module Cog.Math.Vec2
  ( module Cog.Math.Vec2
  , module Cog.Math.Vec
) where

import GHC.Prim (Float#, plusFloat#, minusFloat#, negateFloat#, divideFloat#, timesFloat#, powerFloat#, sinFloat#, acosFloat#, leFloat#, geFloat#, (==#), Int#)
import Prelude hiding (reverse)
import Cog.Math.Vec


data Vec2 = Vec2 Float# Float# deriving (Show, Eq)

cross :: Vec2 -> Vec2
cross (Vec2 x y) = Vec2 (negateFloat# y) x

int2bool :: Int# -> Bool
int2bool 0# = False
int2bool _ = True

instance Vec Vec2 where
  add !(Vec2 ax ay) !(Vec2 bx by) = Vec2 (ax `plusFloat#` bx) (ay `plusFloat#` by)
  sub !(Vec2 ax ay) !(Vec2 bx by) = Vec2 (ax `minusFloat#` bx) (ay `minusFloat#` by)
  scale !(Vec2 x y) !s = Vec2 (x `timesFloat#` s) (y `timesFloat#` s)
  dot !(Vec2 ax ay) !(Vec2 bx by) = ax `timesFloat#` bx `plusFloat#` ay `timesFloat#` by
  reverse !(Vec2 x y) = Vec2 (negateFloat# x) (negateFloat# y)
  lenSq !(Vec2 x y) = x `powerFloat#` 2.0# `plusFloat#` y `powerFloat#` 2.0#
  distSq !(Vec2 ax ay) !(Vec2 bx by) = dx `powerFloat#` 2.0# `plusFloat#` dy `powerFloat#` 2.0#
    where
      dx = bx `minusFloat#` ax
      dy = by `minusFloat#` ay
  angle !v@(Vec2 x _) = acosFloat# (x `divideFloat#` l)
    where !l = len v
  angleTo !a !b = acosFloat# (d `divideFloat#` l)
    where
      !d = a `dot` b
      !l = (len a) `timesFloat#` (len b)
  lerp !src@(Vec2 srcX srcY) !dst@(Vec2 dstX dstY) !t
    | int2bool ((t `leFloat#` 0.0#) ==# 1#) = src
    | int2bool ((t `geFloat#` 1.0#) ==# 1#) = dst
    | otherwise = Vec2 x y
      where
        !x = srcX `plusFloat#` (dstX `minusFloat#` srcX) `timesFloat#` t
        !y = srcY `plusFloat#` (dstY `minusFloat#` srcY) `timesFloat#` t
  slerp !src !dst !t
    | int2bool ((t `leFloat#` 0.0#) ==# 1#) = src
    | int2bool ((t `geFloat#` 1.0#) ==# 1#) = dst
    | otherwise = pSrc `add` pDst
      where
        !omega = src `angleTo` dst
        !sinOmega = sinFloat# omega
        !tOmega = t `timesFloat#` omega
        !fSrc = (sinFloat# (omega `minusFloat#` tOmega)) `divideFloat#` sinOmega
        !fDst = (sinFloat# tOmega) `divideFloat#` sinOmega
        !pSrc = src `scale` fSrc
        !pDst = dst `scale` fDst
