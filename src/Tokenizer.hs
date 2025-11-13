module Tokenizer
  ( tokenize
  ) where

import Data.Char (isDigit, isSpace)

-- IMPROVEMENT 2: Function Composition - Extract validation logic
-- Pure helper functions that can be composed and tested independently
-- Demonstrates separation of concerns (FP principle)
countDots :: String -> Int
countDots = length . filter (== '.')

hasInvalidDotPosition :: String -> Bool
hasInvalidDotPosition num = 
  not (null num) && (head num == '.' || last num == '.')

-- IMPROVEMENT 2: Pure function for number validation
-- Follows FP principle: one function, one responsibility
isValidNumber :: String -> Bool
isValidNumber num =
  let dots = countDots num
  in dots == 0 || (dots == 1 && not (hasInvalidDotPosition num))

-- IMPROVEMENT 3: Recursion with helper functions
-- Main tokenizer now delegates to pure helper functions
-- Maintains exact same functionality with better structure
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
