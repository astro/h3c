module Transition (Transition, runTransition, printCommands,
                   at, during,
                   light, LED(..), LEDID(..),
                   black, white, red, green, blue, color) where

import Text.Printf
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Data.Monoid
import Data.List (intercalate)
import Debug.Trace


type Time = Double

newtype Color = Color (Double, Double, Double)
    deriving (Eq)
instance Show Color where
    show (Color (r, g, b)) = "#" ++ (to_hex r) ++ (to_hex g) ++ (to_hex b)
        where to_hex :: Double -> String
              to_hex = printf "%02X" . to_val
              to_val :: Double -> Int
              to_val c | c > 1.0 = 255
                       | c < 0.0 = 0
                       | otherwise = truncate $ c * 255.0
black = Color (0, 0, 0)
white = Color (1, 1, 1)
red = Color (1, 0, 0)
green = Color (0, 1, 0)
blue = Color (0, 0, 1)
color r g b = Color (r, g, b)


data LEDID = ALL | A | B | C | D | E | F | G | H | I | J | K | L | M | N | O
             deriving (Show, Ord, Eq)
data LED = Both LEDID
         | Front LEDID
         | Back LEDID
           deriving (Ord, Eq)
occludes_ l1 l2 = ((show l1) ++ " occludes " ++ (show l2) ++ " = " ++ (show $ l1 `occludes` l2)) `trace` l1 `occludes` l2
occludes :: LED -> LED -> Bool
(Both ALL) `occludes` _ = True
(Front ALL) `occludes` (Front _) = True
(Back ALL) `occludes` (Back _) = True
(Both id1) `occludes` (Both id2) | id1 == id2 = True
(Both id1) `occludes` (Front id2) | id1 == id2 = True
(Both id1) `occludes` (Back id2) | id1 == id2 = True
l1 `occludes` l2 | l1 == l2 = True
_ `occludes` _ = False

instance Show LED where
    show (Both ledid) = show ledid
    show (Front ledid) = "F" ++ (show ledid)
    show (Back ledid) = "B" ++ (show ledid)

type LEDState = (LED, Color)

s1 `ledappend` [] = s1
s1 `ledappend` (s:s2) = (replaceOrAppend s1 s) `ledappend` s2
    where replaceOrAppend s1 s | (reverse s1) `hasColor` s = s1
                               | otherwise = s1 ++ [s]
          hasColor [] _
              = False
          hasColor ((id1, color1):s) (id2, color2)
              = if id2 `occludes_` id1
                then color1 == color2
                else s `hasColor` (id2, color2)


data Command = CmdC LED Color
             | CmdW Time
               deriving (Eq)
instance Show Command where
    show (CmdC led color) = "C " ++ (show led) ++ " " ++ (show color)
    show (CmdW delay) = "W " ++ (show $ truncate delay)

printCommands :: [Command] -> IO ()
printCommands = putStrLn . intercalate "\n" . map show

type Transition a = ReaderT Time (State [LEDState]) a
runTransition :: Time -> Transition a -> [Command]
runTransition duration transition = compressWaits $ run 0 []
    where run :: Time -> [LEDState] -> [Command]
          run time states
              | time >= duration = []
              | otherwise = let newStates = iterate time
                                allStates = states ++ newStates
                                allNewStates = rmOcclusions allStates
                                commands = map stateToCommand allNewStates
                                commands' | commands == [] = [CmdW 10]
                                          | otherwise = commands
                                time' = time + 10 * (fromIntegral $ length commands')
                            in ("time=" ++ (show time) ++
                                "\nnewStates=" ++ (show newStates) ++
                                "\nallStates=" ++ (show allStates) ++
                                "\nallNewStates=" ++ (show allNewStates)) `trace`
                               commands' ++ (run time' allStates)

          iterate :: Time -> [LEDState]
          iterate time = execState (runReaderT transition time) []

          -- |Remove any LEDStates that are later overdrawn
          rmOcclusions :: [LEDState] -> [LEDState]
          rmOcclusions [] = []
          rmOcclusions (ledstate@(led, _color):ledstates)
                       | any (`occludes_` led) $
                         map fst ledstates = rmOcclusions ledstates
                       | otherwise = ledstate:rmOcclusions ledstates

          stateToCommand (led, color) = CmdC led color

          compressWaits :: [Command] -> [Command]
          compressWaits ((CmdW t1):(CmdW t2):rest) = compressWaits $ (CmdW $ t1 + t2):rest
          compressWaits (c:cs) = c:(compressWaits cs)
          compressWaits [] = []

at :: Time -> Transition () -> Transition ()
at time transition = do now <- ask
                        when (now > time) transition
during :: Time -> Time -> Transition () -> Transition ()
during time1 time2 transition = do now <- ask
                                   --(printf "%.2f =< %.2f < %.2f -> %s" time1 now time2 $ if ((time1 <= now) && (now < time2)) then "t" else "f") `trace`
                                   when ((time1 <= now) && (now < time2)) transition
light :: LED -> Color -> Transition ()
light led color
      = do states <- get
           put $ states ++ [(led, color)]
