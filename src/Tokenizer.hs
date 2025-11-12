module Tokenizer
  ( tokenize
  ) where

import Data.Char (isDigit, isSpace)

-- Strict digit-dot-digit numbers; friendly errors via Either
tokenize :: String -> Either String [String]
tokenize [] = Right []
tokenize (c:cs)
  | isSpace c         = tokenize cs
  | c `elem` "+-*/()" = fmap ([c]:) (tokenize cs)
  | isDigit c || c == '.' =
      let (num, rest)   = span (\x -> isDigit x || x == '.') (c:cs)
          dots          = length (filter (== '.') num)
          startsWithDot = not (null num) && head num == '.'
          endsWithDot   = not (null num) && last num == '.'
      in
        if dots == 0
           then fmap (num:) (tokenize rest)
           else if dots == 1 && not startsWithDot && not endsWithDot
                  then fmap (num:) (tokenize rest)
                  else Left ("Invalid number: " ++ num)
  | otherwise         = Left ("Invalid character: " ++ [c])
