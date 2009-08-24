module Command (Command(..)) where

import Color

data Command = C String Color
             | W Integer
             | F String Color Integer
               deriving (Show, Eq)