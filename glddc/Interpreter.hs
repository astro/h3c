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
              return $
                     case iRemaining i of
                       [] ->
                           i { iRemaining = iScript i,
                               iLastUpdate = now }
                       _ ->
                           let catchUp i'
                                   | iLastUpdate i' < now = catchUp $ step i'
                                   | otherwise = i'
                           in catchUp i

step :: Interpreter -> Interpreter
step i = let i' = i { iLastUpdate = iLastUpdate i + 10,
                      iRemaining = tail $ iRemaining i }
         in case head $ iRemaining i of
              W duration
                  | duration <= 10 -> i'
                  | otherwise -> i' { iRemaining = (W $ duration - 10):(iRemaining i') }
              C led color ->
                  -- TODO
                  i'


-- |Get time in milliseconds
getTimeStep :: IO Integer
getTimeStep = do TOD s ps <- getClockTime
                 return $ s + (ps `div` 1000 `div` 1000)
