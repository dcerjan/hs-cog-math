module Cog.Math.Vec2 (
  Vec2 (..),
  add,
  sub,
  scale,
  dot,
  cross,
  reverse,
  lenSq,
  len,
  distSq,
  dist,
  normalize,
  angle,
  angleTo,
  lerp,
  nlerp,
  slerp,
  project
) where

import Prelude hiding (reverse)


data Vec2 = Vec2 Float Float deriving (Show, Eq)

add :: Vec2 -> Vec2 -> Vec2
add (Vec2 ax ay) (Vec2 bx by) = Vec2 (ax + bx) (ay + by)

sub :: Vec2 -> Vec2 -> Vec2
sub (Vec2 ax ay) (Vec2 bx by) = Vec2 (ax - bx) (ay - by)

scale :: Vec2 -> Float -> Vec2
scale (Vec2 x y) s = Vec2 (x * s) (y * s)

dot :: Vec2 -> Vec2 -> Float
dot (Vec2 ax ay) (Vec2 bx by) = ax * bx + ay * by

cross :: Vec2 -> Vec2
cross (Vec2 x y) = Vec2 (-y) x

reverse :: Vec2 -> Vec2
reverse (Vec2 x y) = Vec2 (-x) (-y)

lenSq :: Vec2 -> Float
lenSq (Vec2 x y) = x ** 2 + y ** 2

len :: Vec2 -> Float
len = sqrt . lenSq

distSq :: Vec2 -> Vec2 -> Float
distSq (Vec2 ax ay) (Vec2 bx by) = dx ** 2 + dy ** 2
  where
    dx = bx - ax
    dy = by - ay

dist :: Vec2 -> Vec2 -> Float
dist a b = sqrt $ distSq a b

normalize :: Vec2 -> Vec2
normalize v@(Vec2 x y) = Vec2 (x/l) (y/l)
  where l = len v

angle :: Vec2 -> Float
angle v@(Vec2 x _) = acos $ x / l
  where l = len v

angleTo :: Vec2 -> Vec2 -> Float
angleTo a b = acos $ d / l
  where
    d = a `dot` b
    l = (len a) * (len b)

lerp :: Vec2 -> Vec2 -> Float -> Vec2
lerp src@(Vec2 srcX srcY) dst@(Vec2 dstX dstY) t
  | t <= 0 = src
  | t >= 1 = dst
  | otherwise = Vec2 x y
    where
      x = srcX + (dstX - srcX) * t
      y = srcY + (dstY - srcY) * t

nlerp :: Vec2 -> Vec2 -> Float -> Vec2
nlerp src dst t = normalize $ lerp src dst t

slerp :: Vec2 -> Vec2 -> Float -> Vec2
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

project :: Vec2 -> Vec2 -> Vec2
project a b = b `scale` q
  where q = (a `dot` b) / len b
