module PScheme.Repl where

import PScheme.Reader
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import System.IO (hFlush, stdout)

data Prompt = InitPrompt | ContPrompt

instance Show Prompt where
  show InitPrompt = "pscheme> "
  show ContPrompt = "       | "

exceptT :: Monad m => Either e a -> ExceptT e m a
exceptT (Left e) = throwE e
exceptT (Right v) = return v

tokenise :: Monad m => String -> ExceptT ReadError m [Token]
tokenise = exceptT . readTokens

parseTokens :: Monad m => (Maybe ParseOutcome) -> [Token] -> ExceptT ReadError m ParseOutcome
parseTokens Nothing tokens = exceptT $ parse tokens
parseTokens (Just po) tokens = exceptT $ parseNext po tokens

statePrompt :: (Maybe a) -> Prompt
statePrompt Nothing = InitPrompt
statePrompt (Just _) = ContPrompt

promptLine :: Show a => a -> IO String
promptLine p = putStr (show p) >> hFlush stdout >> getLine

readNext :: (Maybe ParseOutcome) -> ExceptT ReadError IO Value
readNext state = do
  line <- liftIO $ promptLine $ statePrompt state
  tokens <- tokenise line
  parseOutcome <- parseTokens state tokens
  case (parsedValue parseOutcome) of
    Nothing -> readNext (Just parseOutcome)
    Just v -> pure v

readOne :: ExceptT ReadError IO Value
readOne = readNext Nothing

evalOne :: IO ()
evalOne = do
  r <- runExceptT readOne
  putStrLn $ either show show r
