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

readEval :: Env -> String -> IO String
readEval env s = case (snd $ runRead s readExpr) of
  Left e -> return $ formatError e
  Right expr -> do
    result <- runEval env (evalM expr)
    return $ either show show result

repl :: IO ()
repl = do
  putStr "pscheme> "
  hFlush stdout
  l <- getLine
  r <- readEval defaultEnv l
  putStrLn r

main :: IO ()
main = forever repl
