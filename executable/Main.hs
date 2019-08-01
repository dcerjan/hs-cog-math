{-# LANGUAGE BangPatterns #-}

-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import qualified Cog.Math.Vec2 as Vec2
import qualified Cog.Math.Utils as U

main :: IO ()
main = do
  let !a = Vec2.new 1 2
  let !b = Vec2.new 3 4
  putStrLn $ Vec2.show (a `Vec2.add` b)
  putStrLn $ U.showFloat (a `Vec2.dot` b)
  putStrLn $ U.showFloat (Vec2.len (a `Vec2.add` b))
