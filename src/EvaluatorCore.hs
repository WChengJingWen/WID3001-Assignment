module EvaluatorCore
  ( eval
  ) where

import AST (Expr(..))

-- DESIGN IMPROVEMENT 1: Higher-Order Functions
-- Operations extracted as first-class functions following HOF principles
applyOp :: (Double -> Double -> Double) -> Expr -> Expr -> Double
applyOp op a b = op (eval a) (eval b)

-- Helper for unary functions
applyFn :: (Double -> Double) -> Expr -> Double
applyFn fn e = fn (eval e)

eval :: Expr -> Double
eval (Num n)   = n
eval (Add a b) = applyOp (+) a b
eval (Sub a b) = applyOp (-) a b
eval (Mul a b) = applyOp (*) a b
eval (Div a b) = applyOp (/) a b
eval (Pow a b) = applyOp (**) a b  -- New: using (**) for floating point exponentiation

-- Unary Functions
eval (Sin e) = applyFn sin e
eval (Abs e) = applyFn abs e
eval (Sqrt e) = applyFn sqrt e
