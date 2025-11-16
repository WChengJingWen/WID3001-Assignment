module Tokenizer
  ( tokenize
  ) where

import Data.Char (isDigit, isSpace, isAlpha)  -- isAlpha is imported for function names

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

-- New list of recognized functions
functions :: [String]
functions = ["sin","abs", "sqrt"]

tokenize :: String -> Either String [String]
tokenize [] = Right []
tokenize (c:cs)
  | isSpace c         = tokenize cs
  | c `elem` "+-*/^()" = fmap ([c]:) (tokenize cs)  -- Added exponentiation ^
  | isDigit c || c == '.' =
      let (num, rest) = span (\x -> isDigit x || x == '.') (c:cs)
      in if isValidNumber num
         then fmap (num:) (tokenize rest)
         else Left ("Invalid number: " ++ num)
  | isAlpha c = -- New: Handle function names
      let (name, rest) = span isAlpha (c:cs)
      in if name `elem` functions
        then fmap (name:) (tokenize rest) -- Recognized function
        else Left ("Unknown token or functions:" ++ name)
  | otherwise         = Left ("Invalid character: " ++ [c])
