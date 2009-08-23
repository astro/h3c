module LEDs (LEDID, LEDs, new, light, getColor) where

import Prelude hiding (lookup)
import Data.Map hiding (map, filter)
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
-- a letter A-O
normalizeLEDs [ledid] | ledid `elem` ['A'..'O'] = [['F', ledid], ['B', ledid]]
-- only Front or Back
normalizeLEDs (fb:ledid)
    | length ledid > 0 && (fb == 'F' || fb == 'B')
        = filter ((== fb) . head) $ normalizeLEDs ledid
normalizeLEDs ledid = concat $ map normalizeLEDs $ map (:[]) leds
    where leds | ledid == "ALL" = ['A'..'O']
               | ledid == "1" = ['A'..'E']
               | ledid == "2" = ['F'..'J']
               | ledid == "3" = ['K'..'O']

getColor :: LEDID -> LEDs -> Color
getColor ledid leds = case lookup ledid leds of
                        Nothing -> black
                        Just (Lit color) -> color
