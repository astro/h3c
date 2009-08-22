module LEDs where

import Data.Map (Map)
import qualified Data.Map as Map
import Color

data LEDState = Lit Color

type LEDs = Map String LEDState

new :: LEDs
new = empty

light :: LEDs -> String -> Color -> LEDs
light leds ledid@[fb, _] color
    | fb == 'F' || fb == 'B'
    = insert ledid (Lit color) leds
light leds ledid color
    = foldl (\leds' ledid' ->
                 light leds' ledid' color
            ) leds $ normalizeLEDs ledid

normalizeLEDs :: String -> [String]
normalizeLEDs [ledid] = [['F', ledid], ['B', ledid]]
normalizeLEDs 