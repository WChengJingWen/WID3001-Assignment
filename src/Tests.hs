module Test where

import Test.HUnit  
import AST (Expr(..))
import Tokenizer (tokenize)
import Parser (parseExpr)
import EvaluatorCore (eval)

-- Test 1: Basic tokenization of simple expression
testTokenizeSimple :: Test
testTokenizeSimple = TestCase $ do
  let result = tokenize "2 + 3"
  assertEqual "Should tokenize '2 + 3'" 
              (Right ["2", "+", "3"]) 
              result

-- Test 2: Tokenize with functions (new feature)
testTokenizeFunctions :: Test
testTokenizeFunctions = TestCase $ do
  let result = tokenize "sin(3.14)"
  assertEqual "Should tokenize 'sin(3.14)'" 
              (Right ["sin", "(", "3.14", ")"]) 
              result

-- Test 3: Tokenize power operator (new feature)
testTokenizePower :: Test
testTokenizePower = TestCase $ do
  let result = tokenize "2^3"
  assertEqual "Should tokenize '2^3'" 
              (Right ["2", "^", "3"]) 
              result

-- Test 4: EDGE CASE - Invalid number with multiple dots (fixed bug)
testTokenizeMultipleDots :: Test
testTokenizeMultipleDots = TestCase $ do
  let result = tokenize "3.1.4"
  assertEqual "Should reject multiple dots" 
              (Left "Invalid number: 3.1.4")  
              result


-- Test 5: EDGE CASE - Invalid number starting with dot (fixed bug)
testTokenizeLeadingDot :: Test
testTokenizeLeadingDot = TestCase $ do
  let result = tokenize ".5 + 2"
  assertEqual "Should reject leading dot" 
              (Left "Invalid number: .5" )
              result  


-- Test 6: EDGE CASE - Invalid number ending with dot (fixed bug)
testTokenizeTrailingDot :: Test
testTokenizeTrailingDot = TestCase $ do
  let result = tokenize "5. + 2"
  assertEqual "Should reject trailing dot" 
              (Left "Invalid number: 5.") 
              result

-- Test 7: Valid decimal number
testTokenizeValidDecimal :: Test
testTokenizeValidDecimal = TestCase $ do
  let result = tokenize "3.14"
  assertEqual "Should accept valid decimal '3.14'" 
              (Right ["3.14"]) 
              result

-- Test 8: Unknown function should fail
testTokenizeUnknownFunction :: Test
testTokenizeUnknownFunction = TestCase $ do
  let result = tokenize "cos(1)"
  assertEqual "Should reject unknown function" 
              (Left "Unknown token or functions:cos")
              result  

-- Test 9: Parse simple addition
testParseAddition :: Test
testParseAddition = TestCase $ do
  let tokens = ["2", "+", "3"]
      result = parseExpr tokens
  case result of
    Just (Add (Num 2.0) (Num 3.0), []) -> return ()
    _ -> assertFailure $ "Failed to parse addition: " ++ show result

-- Test 10: Parse power expression (new feature, right-associative)
testParsePower :: Test
testParsePower = TestCase $ do
  let tokens = ["2", "^", "3", "^", "2"] 
      result = parseExpr tokens
  case result of
    Just (Pow (Num 2.0) (Pow (Num 3.0) (Num 2.0)), []) -> return ()
    _ -> assertFailure $ "Failed to parse power (right-assoc): " ++ show result

-- Test 11: Parse function call (new feature)
testParseFunction :: Test
testParseFunction = TestCase $ do
  let tokens = ["sin", "(", "0", ")"]
      result = parseExpr tokens
  case result of
    Just (Sin (Num 0.0), []) -> return ()
    _ -> assertFailure $ "Failed to parse sin function: " ++ show result

-- Test 12: Parse unary minus
testParseUnaryMinus :: Test
testParseUnaryMinus = TestCase $ do
  let tokens = ["-", "5"]
      result = parseExpr tokens
  case result of
    Just (Mul (Num (-1.0)) (Num 5.0), []) -> return ()
    _ -> assertFailure $ "Failed to parse unary minus: " ++ show result

-- Test 13: Parse double unary minus (edge case)
testParseDoubleUnaryMinus :: Test
testParseDoubleUnaryMinus = TestCase $ do
  let tokens = ["-", "-", "3"]
      result = parseExpr tokens
  case result of
    Just (Mul (Num (-1.0)) (Mul (Num (-1.0)) (Num 3.0)), []) -> return ()
    _ -> assertFailure $ "Failed to parse double unary minus: " ++ show result

-- Test 14: EDGE CASE - Missing closing parenthesis (fixed bug detection)
testParseMissingParen :: Test
testParseMissingParen = TestCase $ do
  let tokens = ["(", "2", "+", "3"]
      result = parseExpr tokens
  case result of
    Nothing -> return ()  
    Just (_, leftover) | not (null leftover) -> return ()  
    _ -> assertFailure "Should fail on missing parenthesis"

-- Test 15: Parse complex nested expression
testParseNestedExpression :: Test
testParseNestedExpression = TestCase $ do
  let tokens = ["(", "2", "+", "3", ")", "*", "4"]
      result = parseExpr tokens
  case result of
    Just (Mul (Add (Num 2.0) (Num 3.0)) (Num 4.0), []) -> return ()
    _ -> assertFailure $ "Failed to parse nested expression: " ++ show result

-- Test 16: Evaluate basic arithmetic
testEvalBasic :: Test
testEvalBasic = TestCase $ do
  let expr = Add (Num 2.0) (Num 3.0)
      result = eval expr
  assertEqual "2 + 3 should equal 5" 5.0 result

-- Test 17: Evaluate power operation (new feature)
testEvalPower :: Test
testEvalPower = TestCase $ do
  let expr = Pow (Num 2.0) (Num 3.0)
      result = eval expr
  assertEqual "2 ^ 3 should equal 8" 8.0 result

-- Test 18: Evaluate sin function (new feature)
testEvalSin :: Test
testEvalSin = TestCase $ do
  let expr = Sin (Num 0.0)
      result = eval expr
  assertEqual "sin(0) should equal 0" 0.0 result

-- Test 19: Evaluate abs function (new feature)
testEvalAbs :: Test
testEvalAbs = TestCase $ do
  let expr = Abs (Num (-5.0))
      result = eval expr
  assertEqual "abs(-5) should equal 5" 5.0 result

-- Test 20: Evaluate sqrt function (new feature)
testEvalSqrt :: Test
testEvalSqrt = TestCase $ do
  let expr = Sqrt (Num 9.0)
      result = eval expr
  assertEqual "sqrt(9) should equal 3" 3.0 result

-- Test 21: EDGE CASE - Division by zero produces Infinity
testEvalDivisionByZero :: Test
testEvalDivisionByZero = TestCase $ do
  let expr = Div (Num 1.0) (Num 0.0)
      result = eval expr
  assertBool "Division by zero should produce Infinity" (isInfinite result)

-- Test 22: Evaluate right-associative power
testEvalRightAssocPower :: Test
testEvalRightAssocPower = TestCase $ do
  let expr = Pow (Num 2.0) (Pow (Num 3.0) (Num 2.0))  
      result = eval expr
  assertEqual "2^3^2 should equal 512 (right-assoc)" 512.0 result

-- Test 23: Evaluate complex expression
testEvalComplexExpression :: Test
testEvalComplexExpression = TestCase $ do
  let expr = Sub (Mul (Add (Num 2.0) (Num 3.0)) (Num 4.0)) (Num 5.0)
      result = eval expr
  assertEqual "Complex expression should evaluate correctly" 15.0 result

-- Test 24: Evaluate unary minus
testEvalUnaryMinus :: Test
testEvalUnaryMinus = TestCase $ do
  let expr = Mul (Num (-1.0)) (Num 5.0)
      result = eval expr
  assertEqual "-5 should equal -5" (-5.0) result

-- Test 25: Full pipeline - tokenize, parse, evaluate
testFullPipeline :: Test
testFullPipeline = TestCase $ do
  let input = "2 + 3 * 4"
  case tokenize input of
    Left err -> assertFailure $ "Tokenization failed: " ++ err
    Right tokens -> case parseExpr tokens of
      Nothing -> assertFailure "Parsing failed"
      Just (expr, []) -> do
        let result = eval expr
        assertEqual "2 + 3 * 4 should equal 14" 14.0 result
      Just (_, leftover) -> assertFailure $ "Unexpected tokens: " ++ show leftover

-- Test 26: Full pipeline with new features
testFullPipelineWithFeatures :: Test
testFullPipelineWithFeatures = TestCase $ do
  let input = "abs(-5) + 2^3"  
  case tokenize input of
    Left err -> assertFailure $ "Tokenization failed: " ++ err
    Right tokens -> case parseExpr tokens of
      Nothing -> assertFailure "Parsing failed"
      Just (expr, []) -> do
        let result = eval expr
        assertEqual "abs(-5) + 2^3 should equal 13" 13.0 result
      Just (_, leftover) -> assertFailure $ "Unexpected tokens: " ++ show leftover

-- Test 27: Full pipeline - edge case with parentheses
testFullPipelineParentheses :: Test
testFullPipelineParentheses = TestCase $ do
  let input = "(2 + 3) * (4 - 1)"  
  case tokenize input of
    Left err -> assertFailure $ "Tokenization failed: " ++ err
    Right tokens -> case parseExpr tokens of
      Nothing -> assertFailure "Parsing failed"
      Just (expr, []) -> do
        let result = eval expr
        assertEqual "(2+3)*(4-1) should equal 15" 15.0 result
      Just (_, leftover) -> assertFailure $ "Unexpected tokens: " ++ show leftover

-- Test 28: Full pipeline - sqrt with power
testFullPipelineSqrtPower :: Test
testFullPipelineSqrtPower = TestCase $ do
  let input = "sqrt(16) ^ 2" 
  case tokenize input of
    Left err -> assertFailure $ "Tokenization failed: " ++ err
    Right tokens -> case parseExpr tokens of
      Nothing -> assertFailure "Parsing failed"
      Just (expr, []) -> do
        let result = eval expr
        assertEqual "sqrt(16)^2 should equal 16" 16.0 result
      Just (_, leftover) -> assertFailure $ "Unexpected tokens: " ++ show leftover

tests :: Test
tests = TestList
  [ TestLabel "Test 1: Tokenize Simple" testTokenizeSimple
  , TestLabel "Test 2: Tokenize Functions" testTokenizeFunctions
  , TestLabel "Test 3: Tokenize Power" testTokenizePower
  , TestLabel "Test 4: Tokenize Multiple Dots (Bug Fix)" testTokenizeMultipleDots
  , TestLabel "Test 5: Tokenize Leading Dot (Bug Fix)" testTokenizeLeadingDot
  , TestLabel "Test 6: Tokenize Trailing Dot (Bug Fix)" testTokenizeTrailingDot
  , TestLabel "Test 7: Tokenize Valid Decimal" testTokenizeValidDecimal
  , TestLabel "Test 8: Tokenize Unknown Function" testTokenizeUnknownFunction
  , TestLabel "Test 9: Parse Addition" testParseAddition
  , TestLabel "Test 10: Parse Power (Right-Assoc)" testParsePower
  , TestLabel "Test 11: Parse Function" testParseFunction
  , TestLabel "Test 12: Parse Unary Minus" testParseUnaryMinus
  , TestLabel "Test 13: Parse Double Unary Minus" testParseDoubleUnaryMinus
  , TestLabel "Test 14: Parse Missing Paren (Bug Detection)" testParseMissingParen
  , TestLabel "Test 15: Parse Nested Expression" testParseNestedExpression
  , TestLabel "Test 16: Eval Basic" testEvalBasic
  , TestLabel "Test 17: Eval Power (New Feature)" testEvalPower
  , TestLabel "Test 18: Eval Sin (New Feature)" testEvalSin
  , TestLabel "Test 19: Eval Abs (New Feature)" testEvalAbs
  , TestLabel "Test 20: Eval Sqrt (New Feature)" testEvalSqrt
  , TestLabel "Test 21: Eval Division by Zero (Bug Fix)" testEvalDivisionByZero
  , TestLabel "Test 22: Eval Right-Assoc Power" testEvalRightAssocPower
  , TestLabel "Test 23: Eval Complex Expression" testEvalComplexExpression
  , TestLabel "Test 24: Eval Unary Minus" testEvalUnaryMinus
  , TestLabel "Test 25: Full Pipeline Basic" testFullPipeline
  , TestLabel "Test 26: Full Pipeline With Features" testFullPipelineWithFeatures
  , TestLabel "Test 27: Full Pipeline Parentheses" testFullPipelineParentheses
  , TestLabel "Test 28: Full Pipeline Sqrt Power" testFullPipelineSqrtPower
  ]

main :: IO ()
main = do
  putStrLn "Running Expression Evaluator Test Suite..."
  putStrLn "=========================================="
  counts <- runTestTT tests
  putStrLn "\n=========================================="
  putStrLn "Test Summary:"
  putStrLn $ "  Cases:    " ++ show (cases counts)
  putStrLn $ "  Tried:    " ++ show (tried counts)
  putStrLn $ "  Errors:   " ++ show (errors counts)
  putStrLn $ "  Failures: " ++ show (failures counts)
  putStrLn "=========================================="