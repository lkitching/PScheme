module Main where

import PScheme.Reader
import PScheme.Eval
import Control.Monad (forever)
import System.IO (hFlush, stdout)

evalExpr :: Expr -> String
evalExpr = show

formatError :: ReadError -> String
formatError Incomplete = "Incomplete expression"
formatError (ExpectedChar c) = "Expected '" ++ (show c) ++ "\""
formatError (BadNumber s) = "Invalid number: " ++ s

showEval :: Env -> Expr -> String
showEval env expr = either show show (eval env expr)

readEval :: Env -> String -> String
readEval env s = either formatError (showEval env) (snd $ runRead s readExpr)

repl :: IO ()
repl = do
  putStr "pscheme> "
  hFlush stdout
  l <- getLine
  putStrLn $ readEval defaultEnv l

main :: IO ()
main = forever repl
