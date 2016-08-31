module Main where

import PScheme.Repl
import PScheme.Reader
import PScheme.Eval (exceptT, defaultEnv, runEval, eval)
import System.IO
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Control.Monad (forM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO, MonadIO)

parseForms :: [[Token]] -> Either ReadError [Value]
parseForms [] = pure []
parseForms (ts:tss) = do
  state <- parse ts
  parseLines tss state

parseLines :: [[Token]] -> ParseOutcome -> Either ReadError [Value]
parseLines [] state = case (parsedValue state) of
  Nothing -> pure []
  Just v -> pure [v]
parseLines (ts:tss) state = case (parsedValue state) of
  Nothing -> (parseNext state ts) >>= (parseLines tss)
  Just v -> do
    state' <- parseNext state ts
    vs <- parseLines tss state'
    pure (v:vs)

tokeniseAll :: String -> Either ReadError [[Token]]
tokeniseAll s = tokeniseLines (lines s) where
  tokeniseLines [] = pure []
  tokeniseLines (l:ls) = do
    ts <- readTokens l
    tss <- tokeniseLines ls
    pure $ ts:tss

readHandle :: Handle -> ExceptT ReadError IO [Value]
readHandle h = do
  contents <- liftIO $ hGetContents h
  lineTokens <- exceptT $ tokeniseAll contents
  exceptT $ parseForms lineTokens
  
evalFile :: FilePath -> IO ()
evalFile filePath = withFile filePath ReadMode $ \h -> do
  result <- runExceptT $ readHandle h
  case result of
    Left err -> print err
    Right forms -> do
      env <- defaultEnv
      valsOrError <- runEval env (traverse eval forms)
      case valsOrError of
        Left err -> print err
        Right vals ->
          forM_ vals print

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl
    [filePath] -> evalFile filePath
    _ -> do
      hPutStrLn stderr "Usage: pscheme [file]"
      exitFailure
