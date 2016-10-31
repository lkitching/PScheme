module EvalTests where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad (foldM)

import PScheme.Reader
import PScheme.Eval

evalAll :: String -> IO (Either EvalError Value)
evalAll f = case readAll f of
  Left readErr -> pure $ Left $ ReadError readErr
  Right vs -> do
    env <- defaultEnv
    runEval env $ foldM (\_ v -> eval v) Nil vs

assertEval :: String -> Value -> Assertion
assertEval input expected = do
  r <- evalAll input
  case r of
    Left err -> assertFailure $ "Eval error: " ++ show err
    Right result -> expected @=? result

assertEvalError :: String -> EvalError -> Assertion
assertEvalError input expectedErr = do
  r <- evalAll input
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

listTests :: TestTree
listTests = testGroup "list" [
  testCase "No args" $ assertEval "(list)" Nil,
  testCase "One arg" $ assertEval "(list 1)" (Cons (Number 1) Nil),
  testCase "Multiple args" $ assertEval "(list 2 \"str\" 'a)" (listToCons [(Number 2), (Str "str"), (Symbol "a")])]

carTests :: TestTree
carTests = testGroup "car" [
  testCase "Empty list" $ assertEval "(car '())" Nil,
  testCase "Nonempty list" $ assertEval "(car (list 1 2 3))" (Number 1)]

cdrTests :: TestTree
cdrTests = testGroup "cdr" [
  testCase "Empty list" $ assertEval "(cdr '())" Nil,
  testCase "Nonempty list" $ assertEval "(cdr (list 1 2 3))" (listToCons [(Number 2), (Number 3)])]

consTests :: TestTree
consTests = testGroup "cons" [
  testCase "cons" $ assertEval "(cons 1 '(2 3))" (listToCons $ map Number [1, 2, 3])]

evalTests' :: TestTree
evalTests' = testGroup "eval" [
  testCase "eval" $ assertEval "(eval '(* 2 4))" (Number 8),
  testCase "No closure" $ assertEvalError "(eval (let ((n 2)) '(+ 1 n)))" (UnboundSymbol "n")]

setTests :: TestTree
setTests = testGroup "set!" [
  testCase "set!" $ assertEval "(let ((a 1)) (begin (set! a 2) a))" (Number 2),
  testCase "unbound" $ assertEvalError "(set! a 3)" (UnboundRef "a")]

defineTests :: TestTree
defineTests = testGroup "define" [
  testCase "sequential" $ assertEval "(define a 4) a" (Number 4),
  testCase "recursive binding" $ assertEval (concat [
                                                "(define factorial",
                                                "  (lambda (x) (if x (* x (factorial (- x 1))) 1)))",
                                                "(factorial 4)"])
                                            (Number 24)]

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
                             unquoteTests,
                             listTests,
                             carTests,
                             cdrTests,
                             evalTests',
                             setTests,
                             defineTests]]
