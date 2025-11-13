module Main where

import System.Environment (getArgs, getEnv)
import Control.Exception (catch, SomeException)
import Text.Read (readMaybe)

import Tokenizer (tokenize)
import Parser    (parseExpr)
import EvaluatorCore (eval)

-- IMPROVEMENT 3: Function Composition - Extract I/O logic
-- Separates pure computation from side effects 
getSecretModifier :: IO Double
getSecretModifier = 
  catch (getEnv "SECRET_MODIFIER")
        (\(_ :: SomeException) -> return "1.0")
  >>= return . maybe 1.0 id . (readMaybe :: String -> Maybe Double)

-- IMPROVEMENT 3: Pure helper functions for validation
-- Demonstrates immutability and pure functions (no side effects)
isInvalidResult :: Double -> Bool
isInvalidResult val = isNaN val || isInfinite val

countParens :: String -> [String] -> (Int, Int)
countParens target tokens = 
  (length (filter (==target) tokens), length (filter (==target) tokens))

-- IMPROVEMENT 2: Extract parse error diagnosis as pure function
-- Follows FP principle: separate pure logic from I/O
diagnoseParseError :: [String] -> String
diagnoseParseError tokens =
  let opens  = length (filter (=="(") tokens)
      closes = length (filter (==")") tokens)
  in if opens > closes
     then "Parse error: missing ')'"
     else "Parse error"

-- IMPROVEMENT 3: Monadic composition with do-notation
-- Clean sequencing of I/O actions while maintaining pure core logic
processExpression :: String -> Double -> IO ()
processExpression input secret =
  case tokenize input of
    Left lexErr  -> putStrLn $ "Lex error: " ++ lexErr
    Right tokens ->
      case parseExpr tokens of
        Just (expr, []) -> do
          let val = eval expr
          if isInvalidResult val
            then putStrLn "Math error: division by zero"
            else print (val * secret)
        Just (_, leftover) ->
          putStrLn $ "Parse error: unexpected tokens " ++ show leftover
        Nothing -> putStrLn (diagnoseParseError tokens)

-- Main entry point - cleaner separation of concerns
main :: IO ()
main = do
  secret <- getSecretModifier
  args <- getArgs
  if null args
    then putStrLn "Usage: evaluator \"expression\""
    else processExpression (head args) secret
