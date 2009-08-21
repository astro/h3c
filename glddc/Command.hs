module Command (Command(..)) where

import Color

data Command = C String Color
             | W Integer
               deriving (Show, Eq)