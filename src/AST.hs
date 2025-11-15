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
  deriving Show
