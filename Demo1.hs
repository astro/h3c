import Matrix
import Control.Monad

main = putStrLn $ concat $ map show $
       runTransition 5000 $
       do forM allLEDs $ \led -> light led white
          during 300 2500 $ do light A red
          during 1900 3000 $ do light B blue
          during 2200 2400 $ do light A green
