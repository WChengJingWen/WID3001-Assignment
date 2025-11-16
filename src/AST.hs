module AST
  ( Expr(..)
  ) where

-- Expression data type
data Expr
  = Num Double
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr  -- New: Exponential (a ^ b)
  -- Unary Functions
  | Sin Expr       -- New: Sin Functions
  | Abs Expr       -- New: Absolute value
  | Sqrt Expr      -- New: Square root
  deriving Show
