module Main where

import PScheme.Reader
import PScheme.Eval
import PScheme.Env
import Control.Monad (forever)
import System.IO (hFlush, stdout)

evalExpr :: Expr -> String
evalExpr = show

formatError :: ReadError -> String
formatError Incomplete = "Incomplete expression"
formatError (ExpectedChar c) = "Expected '" ++ (show c) ++ "\""
formatError (BadNumber s) = "Invalid number: " ++ s

readEval :: Env PValue -> String -> IO String
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
