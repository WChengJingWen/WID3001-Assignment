module EvaluatorCore
  ( eval
  ) where

import AST (Expr(..))

-- DESIGN IMPROVEMENT 1: Higher-Order Functions
-- Operations extracted as first-class functions following HOF principles
applyOp :: (Double -> Double -> Double) -> Expr -> Expr -> Double
applyOp op a b = op (eval a) (eval b)

-- IMPROVEMENT 2: Pure recursive functions with pattern matching
-- Each case is a pure function mapping inputs to outputs
eval :: Expr -> Double
eval (Num n)   = n
eval (Add a b) = applyOp (+) a b
eval (Sub a b) = applyOp (-) a b
eval (Mul a b) = applyOp (*) a b
eval (Div a b) = applyOp (/) a b
