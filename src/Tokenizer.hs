module Tokenizer
  ( tokenize
  ) where

import Data.Char (isDigit, isSpace)

-- DESIGN IMPROVEMENT 2: Pure Function & Function Composition
-- Pure function 1: Count dots 
countDots :: String -> Int
countDots = length . filter (== '.')

-- Pure function 2: Check dot position 
hasInvalidDotPosition :: String -> Bool
hasInvalidDotPosition num = 
  not (null num) && (head num == '.' || last num == '.')

-- Pure function 3: Validate number (compuses pure function 1 & 2)
isValidNumber :: String -> Bool
isValidNumber num =
  let dots = countDots num
  in dots == 0 || (dots == 1 && not (hasInvalidDotPosition num))

tokenize :: String -> Either String [String]
tokenize [] = Right []
tokenize (c:cs)
  | isSpace c         = tokenize cs
  | c `elem` "+-*/()" = fmap ([c]:) (tokenize cs)
  | isDigit c || c == '.' =
      let (num, rest) = span (\x -> isDigit x || x == '.') (c:cs)
      in if isValidNumber num
         then fmap (num:) (tokenize rest)
         else Left ("Invalid number: " ++ num)
  | otherwise         = Left ("Invalid character: " ++ [c])
