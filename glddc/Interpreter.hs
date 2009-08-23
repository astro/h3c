module Interpreter (Interpreter, new, update, colorFor) where

import System.Time
import Script
import Command
import Color
import qualified LEDs

data Interpreter = Interpreter { iScript :: [Command],
                                 iRemaining :: [Command],
                                 iLastUpdate :: Integer,
                                 iLEDs :: LEDs.LEDs }

new :: FilePath -> IO Interpreter
new path = do commands <- parse path
              return Interpreter { iScript = commands,
                                   iRemaining = [],
                                   iLastUpdate = 0,
                                   iLEDs = LEDs.new }

update :: Interpreter -> IO Interpreter
update i = do now <- getTimeStep
              putStrLn $ "from " ++ (show $ iLastUpdate i) ++ " to " ++ (show now) ++ " diff: " ++ (show $ now - iLastUpdate i)
              putStrLn $ (show $ length $ iRemaining i) ++ " remaining"
              return $
                     case iRemaining i of
                       [] ->
                           i { iRemaining = iScript i,
                               iLastUpdate = now }
                       _ ->
                           let catchUp i'
                                   | iRemaining i' == [] = i'
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
                  i' { iLEDs = LEDs.light (iLEDs i') led color }

colorFor :: LEDs.LEDID -> Interpreter -> Color
colorFor led i = LEDs.getColor led $ iLEDs i


-- |Get time in milliseconds
getTimeStep :: IO Integer
getTimeStep = do TOD s ps <- getClockTime
                 return $ (s * 1000) + (ps `div` 1000 `div` 1000 `div` 1000)
