module Matrix (
               Time, Color(..), black, white, red, green, yellow, blue, color, colorAdd, mixColor,
               Transition, LED(..), getTime, getColorAt, allLEDs, runTransition, during, at, zoomTime,
               light, putPixel, allLEDs
              ) where

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
colorAdd (Color (r, g, b)) (Color (r', g', b'))
    = Color ((r + r'), (g + g'), (b + b'))

black = Color (0, 0, 0)
white = Color (1, 1, 1)
red = Color (1, 0, 0)
green = Color (0, 1, 0)
blue = Color (0, 0, 1)
yellow = Color (1, 1, 0)
color r g b = Color (r, g, b)
mixColor :: Double -> Color -> Color -> Color
mixColor alpha (Color (r, g, b)) (Color (r', g', b'))
          = Color (r * alpha + r' * alpha', g * alpha + g' * alpha', b * alpha + b' * alpha')
    where alpha' = 1.0 - alpha

data LED = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O
         deriving (Show, Eq)
type LEDState = (LED, Color)

allLEDs :: [LED]
allLEDs = [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]

colorInStates :: [LEDState] -> LED -> Maybe Color
colorInStates [] _led = Nothing
colorInStates ((led',color):states) led
    | led == led' = Just color
    | otherwise = colorInStates states led

data Command = CmdC LED Color
             | CmdW Time
               deriving (Eq)

instance Show Command where
    show (CmdC led color) = "C " ++ (show led) ++ " " ++ (show color) ++ "\n"
    show (CmdW duration) = "W " ++ (show $ truncate duration) ++ "\n"

type Transition a = ReaderT Time (State [LEDState]) a

getTime :: Transition Time
getTime = do now <- ask
             return now

getColorAt :: LED -> Transition (Maybe Color)
getColorAt led = do states <- get
                    return $ colorInStates states led

runTransition :: Time -> Transition a -> [Command]
runTransition duration transition = compressWaits $ run 0 []
    where run :: Time -> [LEDState] -> [Command]
          run time states
              | time >= duration = []
              | otherwise = let newStates = rmOcclusions $ iterate time states
                                newStates' = states `statesDifferences` newStates
                                nextStates = rmOcclusions $ newStates' ++ states
                                commands = map stateToCommand newStates'
                                commands' | commands == [] = [CmdW 10]
                                          | otherwise = commands
                                time' = time + 10 * (fromIntegral $ length commands')
                            in commands' ++ (run time' nextStates)

          iterate :: Time -> [LEDState] -> [LEDState]
          iterate time = execState (runReaderT transition time)

          statesDifferences original target
              = filter (\(target_led, target_color) ->
                            case colorInStates original target_led of
                              Nothing -> True
                              Just original_color -> target_color /= original_color
                       ) target

          -- |Remove any LEDState that is later overdrawn
          rmOcclusions :: [LEDState] -> [LEDState]
          rmOcclusions [] = []
          rmOcclusions (ledstate@(led, _color):states)
              = ledstate:(filter (not . (led ==) . fst) $
                          rmOcclusions states)

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
                                   when ((time1 <= now) && (now < time2)) transition

zoomTime :: Time -> Time -> Transition a -> Transition ()
zoomTime time1 time2 transition
         = during time1 time2 $
           do now <- getTime
              let zoomedTime = (now - time1) / (time2 - time1)
              states <- get
              let states' :: [LEDState]
                  states' = execState (runReaderT transition zoomedTime) states
              put states'

light :: LED -> Color -> Transition ()
light led color
      = do states <- get
           case states `colorInStates` led of
             Just color' | color == color' -> return ()
             _ -> put $ (led, color):states

putPixel :: Int -> Int -> Color -> Transition ()
putPixel 0 0 = light D
putPixel 1 0 = light E
putPixel 0 1 = light C
putPixel 0 2 = light B
putPixel 1 2 = light A
putPixel 2 0 = light I
putPixel 3 0 = light J
putPixel 2 1 = light H
putPixel 2 2 = light G
putPixel 3 2 = light F
putPixel 4 0 = light N
putPixel 5 0 = light O
putPixel 4 1 = light M
putPixel 4 2 = light L
putPixel 5 2 = light K
putPixel _ _ = \_ -> return ()

