module Interpreter (Interpreter, new, update) where

import System.Time
import Script
import Command
import Color

data Interpreter = Interpreter { iScript :: [Command],
                                 iRemaining :: [Command],
                                 iLastUpdate :: Integer }

new :: FilePath -> IO Interpreter
new path = do commands <- parse path
              return Interpreter { iScript = commands,
                                   iRemaining = [],
                                   iLastUpdate = 0 }

update :: Interpreter -> IO Interpreter
update i = do now <- getTimeStep
              case iRemaining i of
                [] ->
                    return i { iRemaining = iScript i,
                               iLastUpdate = now }
                _ -> return i
    where step now i
              | iLastUpdate i >= now = i
              | otherwise = let i' = i { iLastUpdate = iLastUpdate i + 1,
                                         iRemaining = tail $ iRemaining i }
                            in case head $ iRemaining i of
                                 W duration
                                     | duration <= 10 ->
                                         return i'


getTimeStep :: IO Integer
getTimeStep = do TOD s ps <- getClockTime
                 return $ s + (ps `div` 1000 `div` 1000 `div` 10)
