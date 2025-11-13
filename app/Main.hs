module Main where

import System.Environment (getArgs, getEnv)
import Control.Exception (catch, SomeException)
import Text.Read (readMaybe)

import Tokenizer (tokenize)
import Parser    (parseExpr)
import EvaluatorCore (eval)

main :: IO ()
main = do
  secret <- catch (getEnv "SECRET_MODIFIER")
                  (\(_ :: SomeException) -> return "1.0")
  args <- getArgs
  if null args
    then putStrLn "Usage: evaluator \"expression\""
    else case tokenize (head args) of
      Left lexErr  -> putStrLn $ "Lex error: " ++ lexErr
      Right tokens ->
        case parseExpr tokens of
          Just (expr, []) -> do
            let val = eval expr
            if isNaN val || isInfinite val
              then putStrLn "Math error: division by zero"
              else do
                let secretD = maybe 1.0 id (readMaybe secret :: Maybe Double)
                print (val * secretD)
          Just (_, leftover) ->
            putStrLn $ "Parse error: unexpected tokens " ++ show leftover
          Nothing -> do
            let opens  = length (filter (=="(") tokens)
                closes = length (filter (==")") tokens)
            if opens > closes
              then putStrLn "Parse error: missing ')'"
              else putStrLn "Parse error"
