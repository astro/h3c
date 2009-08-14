import Matrix
import Control.Monad

main = putStrLn $ concat $ map show $
       runTransition 2000 $
       zoomTime 0 2000 $
                do now <- getTime
                   forM allLEDs $ \led -> light led (color 1 now (1 - now))
                   light A (color now 0 0)
                   light B (color 0 now 0)
                   light C (color 0 0 now)
