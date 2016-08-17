module Main where

import PScheme.Reader
import PScheme.Eval
import PScheme.Env
import Control.Monad (forever)
import System.IO (hFlush, stdout)

formatError :: ReadError -> String
formatError Incomplete = "Incomplete expression"
formatError (ExpectedChar c) = "Expected '" ++ (show c) ++ "\""
formatError (BadNumber s) = "Invalid number: " ++ s

readEval :: Env Value -> String -> IO String
readEval env s = case (snd $ runRead s readExpr) of
  Left e -> return $ formatError e
  Right expr -> do
    result <- runEval env (eval expr)
    return $ either show show result

repl :: IO ()
repl = do
  putStr "pscheme> "
  hFlush stdout
  l <- getLine
  env <- defaultEnv
  r <- readEval env l
  putStrLn r

readNext :: ParseOutcome -> IO (Either ReadError Value)
readNext state = do
  putStr "pscheme> "
  hFlush stdout
  l <- getLine
  case (inputLine state l) of
    (Left err) -> pure $ Left err
    (Right o) ->
      case (parsedValue o) of
        Just v -> pure $ Right v
        Nothing -> readNext o

readNextOne :: IO (Either ReadError Value)
readNextOne = readNext initOutcome

inputLine :: ParseOutcome -> String -> ParseResult
inputLine state str =
  case (readTokens str) of
    Left err -> error "Invalid tokens!"
    Right ts -> parseNext state ts

evalOne :: IO ()
evalOne = do
  f <- readNextOne
  putStrLn $ either show show f

main :: IO ()
main = forever repl
