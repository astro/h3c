import Matrix
import Control.Monad
import Debug.Trace

c time x y = case ((time + x) `mod` 6) `div` 2 of
               0 -> green
               1 -> yellow
               2 -> red

main = putStrLn $ concat $ map show $
       runTransition 60000 $
       do t <- truncate `liftM` (/ 80.0) `liftM` getTime
          forM_ [0..5] $ \x -> do
            putPixel x 0 (c1 !! (x + t))
            putPixel (5 - x) 1 (c2 !! (x + t))
            putPixel (5 - x) 2 (c2 !! (x + t))

c1 = cycle [yellow, yellow, yellow, yellow, yellow,
            green, green, green,
            red, red,
            white]
c2 = cycle [blue, blue, blue, blue, blue,
            red, red, red, red, red, red, red, red,
            white, white, white, yellow, white, white,
            red, red, red, red, red, red, red, red]
