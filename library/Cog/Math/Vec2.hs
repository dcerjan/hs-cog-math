{-# LANGUAGE MagicHash, BangPatterns, UnboxedTuples #-}

module Cog.Math.Vec2
  ( Vec2
  , mkVec2
  , boxVec2
  , showVec2
  , eqVec2
  , add
  , sub
  , scale
  , dot
  , reverse
  , lenSq
  , len
  , distSq
  , dist
  , normalize
  , angle
  , angleTo
  , lerp
  , nlerp
  , slerp
  , project
  , cross
  ) where

import GHC.Exts
import Prelude hiding (reverse)

int2bool :: Int# -> Bool
int2bool 0# = False
int2bool _ = True

type Vec2 = (# Float#, Float# #)

mkVec2 :: Float -> Float -> Vec2
mkVec2 !(F# x) !(F# y) = (# x, y #)

boxVec2 :: Vec2 -> (Float, Float)
boxVec2 !(# x, y #) = (F# x, F# y)

showVec2 :: Vec2 -> String
showVec2 !(# x, y #) = "Vec2(" ++ sx ++ "f, " ++ sy ++ "f)"
  where
    !sx = show $ F# x
    !sy = show $ F# y

eqVec2 :: Vec2 -> Vec2 -> Bool
eqVec2 !(# ax, ay #) !(# bx, by #) = x && y
  where
    !x = int2bool ((ax `eqFloat#` bx) ==# 1#)
    !y = int2bool ((ay `eqFloat#` by) ==# 1#)

add :: Vec2 -> Vec2 -> Vec2
add !(# ax, ay #) !(# bx, by #) = (# ax `plusFloat#` bx, ay `plusFloat#` by #)

sub :: Vec2 -> Vec2 -> Vec2
sub !(# ax, ay #) !(# bx, by #) = (# ax `minusFloat#` bx, ay `minusFloat#` by #)

scale :: Vec2 -> Float# -> Vec2
scale !(# x, y #) !s = (# x `timesFloat#` s, y `timesFloat#` s #)

dot :: Vec2 -> Vec2 -> Float#
dot !(# ax, ay #) !(# bx, by #) = (ax `timesFloat#` bx) `plusFloat#` (ay `timesFloat#` by)

reverse :: Vec2 -> Vec2
reverse !(# x, y #) = (# negateFloat# x, negateFloat# y #)

lenSq :: Vec2 -> Float#
lenSq !(# x, y #) = (x `powerFloat#` 2.0#) `plusFloat#` (y `powerFloat#` 2.0#)

len :: Vec2 -> Float#
len !a = sqrtFloat# (lenSq a)

distSq :: Vec2 -> Vec2 -> Float#
distSq !(# ax, ay #) !(# bx, by #) = (dx `powerFloat#` 2.0#) `plusFloat#` (dy `powerFloat#` 2.0#)
  where
    !dx = bx `minusFloat#` ax
    !dy = by `minusFloat#` ay

dist :: Vec2 -> Vec2 -> Float#
dist !a !b = sqrtFloat# (distSq a b)

normalize :: Vec2 -> Vec2
normalize v = v `scale` (1.0# `divideFloat#` len v)

angle :: Vec2 -> Float#
angle !v@(# x, _ #) = acosFloat# (x `divideFloat#` l)
  where !l = len v

angleTo :: Vec2 -> Vec2 -> Float#
angleTo !a !b = acosFloat# (d `divideFloat#` l)
  where
    !d = a `dot` b
    !l = (len a) `timesFloat#` (len b)

lerp :: Vec2 -> Vec2 -> Float# -> Vec2
lerp !src@(# srcX, srcY #) !dst@(# dstX, dstY #) !t
  | int2bool ((t `leFloat#` 0.0#) ==# 1#) = src
  | int2bool ((t `geFloat#` 1.0#) ==# 1#) = dst
  | otherwise = (# x, y #)
    where
      !x = srcX `plusFloat#` (dstX `minusFloat#` srcX) `timesFloat#` t
      !y = srcY `plusFloat#` (dstY `minusFloat#` srcY) `timesFloat#` t

nlerp :: Vec2 -> Vec2 -> Float# -> Vec2
nlerp !src !dst !t = normalize(lerp src dst t)

slerp :: Vec2 -> Vec2 -> Float# -> Vec2
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

project :: Vec2 -> Vec2 -> Vec2
project !a !b = b `scale` q
  where !q = (a `dot` b) `divideFloat#` len b

cross :: Vec2 -> Vec2
cross !(# x, y #) = (# negateFloat# y, x #)
