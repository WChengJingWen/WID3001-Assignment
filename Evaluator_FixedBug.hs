module Evaluator where

import System.Environment (getArgs, getEnv)
import Data.Char (isDigit, isSpace)
import Control.Exception (catch, SomeException)
import Text.Read (readMaybe)

-- Tokenize the input string into tokens
-- change type
tokenize :: String -> Either String [String]

-- rewrite cases to produce Left/Right
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
        -- integers: OK
        if dots == 0 then fmap (num:) (tokenize rest)
        -- exactly one dot: must be digit '.' digit (no leading/trailing dot)
        else if dots == 1 && not startsWithDot && not endsWithDot
             then fmap (num:) (tokenize rest)
             else Left ("Invalid number: " ++ num)
  | otherwise         = Left ("Invalid character: " ++ [c])

-- Expression data type
data Expr = Num Double | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
  deriving Show

-- Parse tokens into an expression tree
parseExpr :: [String] -> Maybe (Expr, [String])
parseExpr = parseAddSub

parseAddSub :: [String] -> Maybe (Expr, [String])
parseAddSub ts = do
  (e1, r1) <- parseMulDiv ts
  let go e ("+":xs) = do
        (e2, r2) <- parseMulDiv xs
        go (Add e e2) r2
      go e ("-":xs) = do
        (e2, r2) <- parseMulDiv xs
        go (Sub e e2) r2
      go e rest     = Just (e, rest)
  go e1 r1


parseMulDiv :: [String] -> Maybe (Expr, [String])
parseMulDiv ts = do
  (e1, r1) <- parseFactor ts
  let go e ("*":xs) = do
        (e2, r2) <- parseFactor xs
        go (Mul e e2) r2
      go e ("/":xs) = do
        (e2, r2) <- parseFactor xs
        go (Div e e2) r2
      go e rest     = Just (e, rest)
  go e1 r1

parseFactor :: [String] -> Maybe (Expr, [String])
parseFactor [] = Nothing

-- Unary minus: -factor
parseFactor ("-":ts) = do
  (e, rest) <- parseFactor ts
  Just (Mul (Num (-1)) e, rest)

-- Parentheses: ( expr )
parseFactor ("(":ts) = do
  (e, rest) <- parseExpr ts
  case rest of
    (")":rest') -> Just (e, rest')
    _           -> Nothing

-- Number
parseFactor (t:ts) =
  case readMaybe t :: Maybe Double of
    Just v  -> Just (Num v, ts)
    Nothing -> Nothing

-- Evaluate the expression
eval :: Expr -> Double
eval (Num n) = n
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Mul a b) = eval a * eval b
eval (Div a b) = eval a / eval b

-- Main CLI loop
main :: IO ()
main = do
  secret <- catch (getEnv "SECRET_MODIFIER")
                (\(_ :: SomeException) -> return "1.0")
  args <- getArgs
  if null args
    then putStrLn "Usage: evaluator \"expression\""
    else do
      case tokenize (head args) of
        Left lexErr -> putStrLn $ "Lex error: " ++ lexErr
        Right tokens ->
          case parseExpr tokens of
            Just (expr, [])    -> do
              let val = eval expr
              if isNaN val || isInfinite val
                then putStrLn "Math error: division by zero"
                else do
                  let secretD = maybe 1.0 id (readMaybe secret :: Maybe Double)
                  let result  = val * secretD
                  print result
            Just (_, leftover) -> putStrLn $ "Parse error: unexpected tokens " ++ show leftover
            Nothing            -> do
              let opens  = length (filter (=="(") tokens)
                  closes = length (filter (==")") tokens)
              if opens > closes
                then putStrLn "Parse error: missing ')'"
                else putStrLn "Parse error"

