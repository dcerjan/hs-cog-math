{-# LANGUAGE BangPatterns #-}

-- You can benchmark your code quickly and effectively with Criterion. See its
-- website for help: <http://www.serpentine.com/criterion/>.
import Criterion.Main

import qualified Cog.Math.Vec2 as Vec2

data Vec2Boxed = Vec2Boxed Float Float deriving (Show, Eq)

class Vec a where
  add :: a -> a -> a
  scale :: a -> Float -> a

instance Vec Vec2Boxed where
  add (Vec2Boxed ax ay) (Vec2Boxed bx by) = Vec2Boxed (ax + bx) (ay + by)
  scale (Vec2Boxed x y) s = Vec2Boxed (x * s) (y * s)

main :: IO ()
main = defaultMain [ bgroup "boxed" [ bench "1" $ whnf (\f -> add (Vec2Boxed 1 1) (Vec2Boxed 2 f)) 1
                                    ]
                   , bgroup "unboxed" [ bench "1" $ whnf (\f -> Vec2.box (Vec2.add (Vec2.new 1 1) (Vec2.new 2 f))) 1
                                      ]
                   ]
