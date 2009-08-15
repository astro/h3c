import Matrix
import Control.Monad
import Debug.Trace

main = do putStrLn $ concat $ map show $
                   runTransition 30000 $
                   do now <- getTime
                      forM allLEDs $ \led -> light led white
                      let (x, y) = pong_movements 0 1 1 1 !! (truncate $ now / 250)
                      putPixel x y black

pong_movements x y dx dy
    | x' < 0 = pong_movements x y 1 dy
    | y' < 0 = pong_movements x y dx 1
    | x' > 5 = pong_movements x y (-1) dy
    | y' > 2 = pong_movements x y dx (-1)
    | otherwise = (x, y):(pong_movements x' y' dx dy)
    where x' = x + dx
          y' = y + dy

