import Matrix
import Control.Monad
import Debug.Trace

c time x y = case ((time + x) `mod` 6) `div` 2 of
               0 -> green
               1 -> yellow
               2 -> red

main = putStrLn $ concat $ map show $
       runTransition 60000 $
       do now <- getTime
          let scaledTime = truncate $ now / 200
          forM [0..2] $ \y ->
              forM [0..5] $ \x ->
              putPixel x y $ c scaledTime x y
