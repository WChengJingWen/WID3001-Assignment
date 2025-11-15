module Parser
  ( parseExpr
  ) where

import Text.Read (readMaybe)
import AST (Expr(..))

-- Public API
parseExpr :: [String] -> Maybe (Expr, [String])
parseExpr = parseAddSub

-- DESIGN IMPROVEMENT 1: Higher-Order Functions
-- Remove redundant code with a single reusable function
parseBinOp :: (Expr -> Expr -> Expr)           -- constructor
           -> [(String, Expr -> Expr -> Expr)]  -- operators
           -> ([String] -> Maybe (Expr, [String])) -- next parser
           -> [String] 
           -> Maybe (Expr, [String])
parseBinOp _ ops nextLevel ts = do
  (e1, r1) <- nextLevel ts
  let go e (op:xs) 
        | Just cons <- lookup op ops = do
            (e2, r2) <- nextLevel xs
            go (cons e e2) r2
      go e rest = Just (e, rest)
  go e1 r1

-- Add/Sub (left associative) - now uses HOF pattern
parseAddSub :: [String] -> Maybe (Expr, [String])
parseAddSub = parseBinOp Add [("+", Add), ("-", Sub)] parseMulDiv

-- Mul/Div (left associative) - now uses HOF pattern
parseMulDiv :: [String] -> Maybe (Expr, [String])
parseMulDiv = parseBinOp Mul [("*", Mul), ("/", Div)] parseFactor

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