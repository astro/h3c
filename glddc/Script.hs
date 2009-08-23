module Script (parse) where

import Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as Parsec
import Control.Monad
import Color
import Command

parse :: FilePath -> IO [Command]
parse path = do r <- parseFromFile script path
                case r of
                  Right commands -> return commands
                  Left e -> error $ show e

script :: GenParser Char st [Command]
script = do commands <- (map (\(Just command) -> command) .
                         filter (maybe False $ \_ -> True)) `liftM`
                        many line
            eof
            return commands

line = do many whitespace
          r <- (Just `liftM` color
                <|>
                Just `liftM` wait
                <|>
                do comment
                   return Nothing
                <|>
                return Nothing
               )
          many whitespace
          eol
          return r

color = do char 'C'
           many whitespace
           led <- many1 $ oneOf "0123456789ABCDEFGHIJKLMNOPQR"
           many whitespace
           c <- colorSpec
           return $ C led c
    where colorSpec :: GenParser Char st Color
          colorSpec = parseColor `liftM`
                      many1 (oneOf "#0123456789abcdefABCDEF")

wait = do char 'W'
          many whitespace
          duration <- read `liftM` many digit
          return $ W duration

comment = do (char '#'
              <|>
              char '/')
             manyTill anyChar eol


whitespace = oneOf " \t\r"

eol = char '\n'
