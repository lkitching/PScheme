module Main where

import PScheme.Reader
import Control.Monad (forever)
import System.IO (hFlush, stdout)

evalExpr :: Expr -> String
evalExpr = show

formatError :: ReadError -> String
formatError Incomplete = "Incomplete expression"
formatError (ExpectedChar c) = "Expected '" ++ (show c) ++ "\""
formatError (BadNumber s) = "Invalid number: " ++ s

readEval :: String -> String
readEval s = either formatError evalExpr (snd $ runRead s readExpr)

repl :: IO ()
repl = do
  putStr "pscheme> "
  hFlush stdout
  l <- getLine
  putStrLn $ readEval l

main :: IO ()
main = forever repl
