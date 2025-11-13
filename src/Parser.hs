module Parser
  ( parseExpr
  ) where

import Text.Read (readMaybe)
import AST (Expr(..))

-- Public API
parseExpr :: [String] -> Maybe (Expr, [String])
parseExpr = parseAddSub

-- Add/Sub (left associative)
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

-- Mul/Div (left associative)
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

-- Factor: unary -, parentheses, number
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
