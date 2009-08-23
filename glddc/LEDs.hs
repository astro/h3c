module LEDs where

import Data.Map (Map)
import qualified Data.Map as Map
import Color

data LEDState = Lit Color

type LEDID = String
type LEDs = Map LEDID LEDState

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

normalizeLEDs :: LEDID -> [LEDID]
normalizeLEDs ledid@[fb, _] | fb == 'F' || fb == 'B' = [ledid]
normalizeLEDs [ledid] = [['F', ledid], ['B', ledid]]
normalizeLEDs "ALL" = concat $ map normalizeLEDs $ map (:[]) ['A'..'O']
normalizeLEDs (fb:"ALL") | fb == 'F' || fb == 'B' = concat $ map normalizeLEDs $ map (fb::[]) ['A'..'O']
