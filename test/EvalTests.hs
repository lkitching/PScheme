module EvalTests where

import Test.Tasty
import Test.Tasty.HUnit

import PScheme.Reader
import PScheme.Eval

expectRead :: String -> IO Value
expectRead f = case (readStringOne f) of
  Left err -> (assertFailure $ "Failed to read: " ++ show err) >> pure Nil
  Right v -> pure v

expectEval :: String -> IO (Either EvalError Value)
expectEval f = do
  v <- expectRead f
  env <- defaultEnv
  runEval env (eval v)

assertEval :: String -> Value -> Assertion
assertEval input expected = do
  r <- expectEval input
  case r of
    Left err -> assertFailure $ "Eval error: " ++ show err
    Right result -> expected @=? result

assertEvalError :: String -> EvalError -> Assertion
assertEvalError input expectedErr = do
  r <- expectEval input
  case r of
    Left err -> expectedErr @=? err
    Right result -> assertFailure $ "Expected error during evaluation."

evalTests :: TestTree
evalTests = testGroup "Eval tests" [unitTests]

plusTests :: TestTree
plusTests = testGroup "+" [
  testCase "No args" $ assertEval "(+)" (Number 0),
  testCase "One arg" $ assertEval "(+ 3)" (Number 3),
  testCase "Multiple args" $ assertEval "(+ 1 2 3)" (Number 6)]

subtractTests :: TestTree
subtractTests = testGroup "-" [
  testCase "No args" $ assertEval "(-)" (Number 0),
  testCase "One arg" $ assertEval "(- 4)" (Number (-4)),
  testCase "Multiple args" $ assertEval "(- 5 3 1)" (Number 1)]

multTests :: TestTree
multTests = testGroup "*" [
  testCase "No args" $ assertEval "(*)" (Number 1),
  testCase "One arg" $ assertEval "(* 3)" (Number 3),
  testCase "Multiple args" $ assertEval "(* 2 5 -3 4)" (Number (-120))]

ifTests :: TestTree
ifTests = testGroup "if" [
  testCase "No args" $ assertEval "(if)" Nil,
  testCase "Condition only" $ assertEval "(if 3)" Nil,
  testCase "True case when true" $ assertEval "(if 3 \"true\")" (Str "true"),
  testCase "True case when false" $ assertEval "(if 0 \"true\")" Nil,
  testCase "Both cases when true" $ assertEval "(if \"t\" 2 4)" (Number 2),
  testCase "Both cases when false" $ assertEval "(if 0 2 4)" (Number 4)]

letTests :: TestTree
letTests = testGroup "let" [
  testCase "Single binding" $ assertEval "(let ((a 1)) a)" (Number 1),
  testCase "Dependent binding" $ assertEvalError "(let ((a 1) (b (+ a 1))) b)" (UnboundSymbol "a")]

letStarTests :: TestTree
letStarTests = testGroup "let*" [
  testCase "Dependent bindings" $ assertEval "(let* ((a 1) (b (+ a 1))) b)" (Number 2)]

letrecTests :: TestTree
letrecTests = testGroup "letrec" [
  testCase "Recursive binding" $ assertEval "((letrec ((f (lambda (n) (if n (* n (f (- n 1))) 1)))) f) 5)" (Number 120)]

lambdaTests :: TestTree
lambdaTests = testGroup "lambda" [
  testCase "No capture" $ assertEval "((lambda (x) (+ x 1)) 3)" (Number 4),
  testCase "Capture binding" $ assertEval "((let ((n 3)) (lambda (x) (+ x n))) 2)" (Number 5),
  testCase "Param names shadow bindings" $ assertEval "((let ((n 3)) (lambda (n) (* 2 n))) 1)" (Number 2)]

quoteTests :: TestTree
quoteTests = testGroup "quote" [
  testCase "Quote symbol" $ assertEval "'a" (Symbol "a")]

unquoteTests :: TestTree
unquoteTests = testGroup "unquote" [
  testCase "Unquote symbol" $ assertEval "(let ((a 1)) '(a ,a))" (listToCons [(Symbol "a"), (Number 1)]),
  testCase "outside quote" $ assertEvalError ",a" (UnboundSymbol "unquote")]

unitTests :: TestTree
unitTests = testGroup "Eval unit tests" [
  testGroup "default forms" [plusTests,
                             subtractTests,
                             multTests,
                             ifTests,
                             letTests,
                             letStarTests,
                             letrecTests,
                             lambdaTests,
                             quoteTests,
                             unquoteTests]]
