module LEDs (LEDID, LEDs, new, light, fade, getColor) where

import Prelude hiding (lookup)
import Data.Map hiding (map, filter)
import Color

data LEDState = Lit Color
              | Fading Color Integer Color Integer
              deriving (Show, Eq)

type LEDID = String
type LEDs = Map LEDID LEDState

new :: LEDs
new = empty

light :: LEDs -> String -> Color -> LEDs
light leds ledid color
    = setStates leds ledid $
      const $ Lit color

fade leds ledid endcolor starttime endtime
    = setStates leds ledid $ \ledid' ->
      let startcolor = getColor ledid' leds starttime
      in Fading startcolor starttime endcolor endtime

setStates :: LEDs -> String -> (LEDID -> LEDState) -> LEDs
setStates leds ledid@[fb, _] ledstateHandler
    | fb == 'F' || fb == 'B'
    = let oldColor = getColor ledid leds
          ledstate = ledstateHandler ledid
      in insert ledid ledstate leds
setStates leds ledid ledstateHandler
    = foldl (\leds' ledid' ->
                 setStates leds' ledid' ledstateHandler
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

getColor :: LEDID -> LEDs -> Integer -> Color
getColor ledid leds time
    = case lookup ledid leds of
        Nothing -> black
        Just (Lit color) -> color
        Just (Fading startcolor starttime endcolor endtime) ->
            let alpha | time >= starttime && time <= endtime
                          = fromIntegral (time - starttime) / fromIntegral (endtime - starttime)
                      | time > endtime
                          = 1
            in mix alpha endcolor startcolor
