module Main where

import System.Environment (getArgs, getEnv)
import Control.Exception (catch, SomeException)
import Text.Read (readMaybe)

import Tokenizer (tokenize)
import Parser    (parseExpr)
import EvaluatorCore (eval)

-- DESIGN IMPROVEMENT 3: Separation of I/O and Pure Logic
-- I/O function 1: Get secret modifier
getSecretModifier :: IO Double
getSecretModifier = 
  catch (getEnv "SECRET_MODIFIER")
        (\(_ :: SomeException) -> return "1.0")
  -- DESIGN IMPROVEMENT 4: Monadic Composition
  -- Uses monadic bind 
  >>= return . maybe 1.0 id . (readMaybe :: String -> Maybe Double)

-- Pure function 1 : Validation logic separated
isInvalidResult :: Double -> Bool
isInvalidResult val = isNaN val || isInfinite val

-- Pure function 2: Error diagnosis
diagnoseParseError :: [String] -> String
diagnoseParseError tokens =
  let opens  = length (filter (=="(") tokens)
      closes = length (filter (==")") tokens)
  in if opens > closes
     then "Parse error: missing ')'"
     else "Parse error"

-- I/O function 2: Process expression
processExpression :: String -> Double -> IO ()
processExpression input secret =
  case tokenize input of
    Left lexErr  -> putStrLn $ "Lex error: " ++ lexErr
    Right tokens ->
      case parseExpr tokens of
        Just (expr, []) -> do
          let val = eval expr
          if isInvalidResult val  -- call pure function 1
            then putStrLn "Math error: division by zero"
            else print (val * secret)
        Just (_, leftover) ->
          putStrLn $ "Parse error: unexpected tokens " ++ show leftover
        Nothing -> putStrLn (diagnoseParseError tokens) -- call pure function 2

-- DESIGN IMPROVEMENT 5: Pattern Matching with Guards
main :: IO ()
main = do
  secret <- getSecretModifier
  args <- getArgs
  if null args
    case args of
      []    -> putStrLn "Usage: evaluator \"expression\""
      (x:_) -> processExpression x secret
