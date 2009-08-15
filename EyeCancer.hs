import Matrix
import Control.Monad
import Debug.Trace

c time x y = case ((time + x) `mod` 6) `div` 2 of
               0 -> green
               1 -> yellow
               2 -> red

main = putStrLn $ concat $ map show $
       runTransition 60000 $
       do now <- (`mod` 200) `liftM` truncate `liftM` getTime
          let (c1, c2) | now < 100 = (yellow, blue)
                       | otherwise = (green, red)
          forM [0..5] $ \x ->
              forM [0..2] $ \y ->
              putPixel x y $ if (x + y) `mod` 2 == 0
                             then c1 else c2
