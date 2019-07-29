{-# LANGUAGE MagicHash, BangPatterns #-}

module Cog.Math.Vec where

import GHC.Prim (Float#, sqrtFloat#, divideFloat#)

class Vec a where
  add :: a -> a -> a
  sub :: a -> a -> a
  scale :: a -> Float# -> a
  dot :: a -> a -> Float#
  reverse :: a -> a
  lenSq :: a -> Float#
  len :: a -> Float#
  len !a = sqrtFloat# (lenSq a)
  distSq :: a -> a -> Float#
  dist :: a -> a -> Float#
  dist !a !b = sqrtFloat# (distSq a b)
  normalize :: a -> a
  normalize v = v `scale` (1.0# `divideFloat#` len v)
  angle :: a -> Float#
  angleTo :: a -> a -> Float#
  lerp :: a -> a -> Float# -> a
  nlerp :: a -> a -> Float# -> a
  nlerp src dst t = normalize $ lerp src dst t
  slerp :: a -> a -> Float# -> a
  project :: a -> a -> a
  project a b = b `scale` q
    where q = (a `dot` b) `divideFloat#` len b
