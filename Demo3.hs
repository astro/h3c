import Matrix
import Control.Monad
import Debug.Trace

white_to_red = do forM allLEDs $ \led -> light led white
                  now <- getTime
                  forM [0..2] $ \y ->
                      forM [0..5] $ \x ->
                      putPixel x (truncate $ fromIntegral y + (fromIntegral x / 2.0) + (now * 3.0) - 4.0) red

red_to_yellow = do now <- getTime
                   forM [0..2] $ \y ->
                       forM [0..5] $ \x ->
                       putPixel x y (if y < (truncate $ 3.0 - (now * 3.0))
                                     then yellow
                                     else red)

tear = do tear1 C 0.01
          tear1 A (-0.02)
          tear1 I 0.02
          tear1 J 0.04
          tear1 N (-0.05)
          tear1 M 0.05
    where tear1 led inc = do colorAt <- getColorAt led
                             case colorAt of
                               Nothing -> return ()
                               Just color -> let color' = colorAdd color (Matrix.color inc 0 (-inc))
                                             in light led color'

main = do putStrLn $ concat $ map show $
                   runTransition 10000 $
                   do zoomTime 0 1000 white_to_red
                      zoomTime 1000 1500 red_to_yellow
                      during 1500 10000 tear
