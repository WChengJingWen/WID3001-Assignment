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


-- New: Reusable function for RIGHT-ASSOCIATIVE binary operators (e.g., ^)
parseRightAssoc :: [(String, Expr -> Expr -> Expr)]  -- operators
               -> ([String] -> Maybe (Expr, [String])) -- current level parser (for right recursion)
               -> ([String] -> Maybe (Expr, [String])) -- next level parser (for left operand)
               -> [String] 
               -> Maybe (Expr, [String])
parseRightAssoc ops currentLevel nextLevel ts = do
    (e1, r1) <- nextLevel ts
    -- Check if r1 (the rest of the tokens) starts with an operator.
    case r1 of
        (op:xs) | Just cons <- lookup op ops -> 
            -- Use case to explicitly handle the Maybe from the recursive call
            case currentLevel xs of 
                Just (e2, r2) -> Just (cons e1 e2, r2)
                Nothing       -> Nothing
        -- If no operator, or lookup fails, return the initial expression and remaining tokens
        rest -> Just (e1, rest)

-- Add/Sub (left associative) - now uses HOF pattern
parseAddSub :: [String] -> Maybe (Expr, [String])
parseAddSub = parseBinOp Add [("+", Add), ("-", Sub)] parseMulDiv

-- Mul/Div (left associative) - now uses HOF pattern
parseMulDiv :: [String] -> Maybe (Expr, [String])
parseMulDiv = parseBinOp Mul [("*", Mul), ("/", Div)] parseUnary

-- Unary minus has higher precedence than power (^)
parseUnary :: [String] -> Maybe (Expr, [String])
parseUnary ("-":ts) = do
    (e, rest) <- parseUnary ts   -- chain unary operators
    Just (Mul (Num (-1)) e, rest)
parseUnary ts = parsePower ts

-- Power (^) - Right Associative
parsePower :: [String] -> Maybe (Expr, [String])
-- Note: Must pass 'parsePower' as the current level parser for right-associativity
parsePower = parseRightAssoc [("^", Pow)] parsePower parseFunction

-- Function Calls (sin, abs, sqrt)
parseFunction :: [String] -> Maybe (Expr, [String])
parseFunction [] = Nothing
-- NEW TIGHT UNARY: This handles the unary minus for the base of a power, 
-- inside parentheses, or inside a function (e.g., 2^-3 or sin(-1)).
parseFunction ("-":ts) = do
    -- Recursively call parseFunction to allow tight chains (e.g., sin(--3))
    (e, rest) <- parseFunction ts 
    Just (Mul (Num (-1)) e, rest)
parseFunction (t:ts)
  | t == "sin" = parseFunctionCall Sin ts
  | t == "abs" = parseFunctionCall Abs ts
  | t == "sqrt" = parseFunctionCall Sqrt ts
  | otherwise = parseFactor (t:ts) -- Fall through to parseFactor if not a function

-- Helper function to parse 'func(expr)'
parseFunctionCall :: (Expr -> Expr) -> [String] -> Maybe (Expr, [String])
parseFunctionCall cons ("(":ts) = do
  (e, rest) <- parseExpr ts
  case rest of
    (")":rest') -> Just (cons e, rest')
    _           -> Nothing
parseFunctionCall _ _ = Nothing


-- Factor: unary -, parentheses, number
parseFactor :: [String] -> Maybe (Expr, [String])
parseFactor [] = Nothing

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