-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import Cog.Math.Vec2 (Vec2 (..), add, dot)

main :: IO ()
main = do
  let a = Vec2 1 2
  let b = Vec2 3 4
  putStrLn $ show $ a `add` b
  putStrLn $ show $ ((a `dot` b) :: Float)
