{-# LANGUAGE BangPatterns #-}

-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import Cog.Math.Vec2 (newVec2, add, dot, len, showVec2)
import Cog.Math.Utils (showFloat)

main :: IO ()
main = do
  let !a = newVec2 1 2
  let !b = newVec2 3 4
  putStrLn $ showVec2 (a `add` b)
  putStrLn $ showFloat (a `dot` b)
  putStrLn $ showFloat (len (a `add` b))
