module Color (Color, clamp, parseColor) where

import Text.Printf
import Data.Char (ord)
import Debug.Trace

data Color = Color Double Double Double
    deriving (Eq)

clamp :: Color -> Color
clamp (Color r g b)
    = Color (clamp' r) (clamp' g) (clamp' b)
    where clamp' a | a < 0 = 0
                   | a > 1 = 1
                   | otherwise = a

instance Show Color where
    show color
        = let (Color r g b) = clamp color
          in "#" ++ (toHex r) ++ (toHex g) ++ (toHex b)
        where toHex :: Double -> String
              toHex = printf "%02X" . toInt
              toInt :: Double -> Int
              toInt = truncate . (* 255.0)

{-
instance Read Color where
    readList ('#':r1:r2:g1:g2:b1:b2:rest)
        = let r = hexToVal r1 r2
              g = hexToVal g1 g2
              b = hexToVal b1 b2
          in (show rest) `trace`
                    [([Color r g b], rest)]
        where hexToVal :: Char -> Char -> Double
              hexToVal h1 h2
                  = fromIntegral ((fromHex h1) * 0x10 + (fromHex h2)) / 255.0
              fromHex :: Char -> Int
              fromHex h
                  | h >= '0' && h <= '9' = ord h - ord '0'
                  | h >= 'a' && h <= 'f' = ord h - ord 'a' + 0x10
                  | h >= 'A' && h <= 'F' = ord h - ord 'A' + 0x10
-}
parseColor ('#':r1:r2:g1:g2:b1:b2:rest)
    = let r = hexToVal r1 r2
          g = hexToVal g1 g2
          b = hexToVal b1 b2
      in Color r g b
    where hexToVal :: Char -> Char -> Double
          hexToVal h1 h2
              = fromIntegral ((fromHex h1) * 0x10 + (fromHex h2)) / 255.0
          fromHex :: Char -> Int
          fromHex h | h >= '0' && h <= '9' = ord h - ord '0'
                    | h >= 'a' && h <= 'f' = ord h - ord 'a' + 0x10
                    | h >= 'A' && h <= 'F' = ord h - ord 'A' + 0x10
