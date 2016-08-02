module PScheme.Reader (
  runRead,
  readExpr,
  Value(..),
  ReadError(..),
  Eval,
  EvalError(..),
  EvalResult,
  consToList,
  values
  ) where

import Data.Char (isSpace, isDigit)
import Control.Monad
import Control.Applicative
import Data.Maybe (listToMaybe)
import Data.List (intercalate)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Except (ExceptT)
import PScheme.Env (Env)

data EvalError =
    UnboundSymbol String
  | OperatorRequired
  | TypeError String Value
  | FormError String [Value]
  | ArityError Int Int
  | DerefUndefinedError
  | ListError String

instance Show EvalError where
  show (UnboundSymbol sym) = "Unbound symbol: " ++ sym
  show OperatorRequired = "Operator required"
  show (TypeError ty value) = "Unexpected type for " ++ (show value) ++ ": required " ++ ty
  show (FormError msg form) = "Invalid form: " ++ msg
  show (ArityError expected actual) = "Wrong number of arguments (" ++ (show actual) ++ "). Expected: " ++ (show expected)
  show DerefUndefinedError = "Cannot evaluate undefined"
  show (ListError msg) = "List error: " ++ msg

data Value =
    Number Integer
  | Str String
  | Symbol String
  | Nil
  | Cons Value Value
  | Undefined
  | Fn ([Value] -> EvalResult)
  | Closure (Env Value) [String] Value
  | Special ([Value] -> Eval Value)

values :: Value -> [Value]
values Nil = []
values (Cons hd tl) = hd:(values tl)
values e = [e]

consToList :: Value -> Value -> [Value]
consToList hd tl = hd:(values tl)

instance Show Value where
  show (Number i) = show i
  show (Str s) = "\"" ++ s ++ "\""
  show (Symbol s) = s
  show Nil = "()"
  show (Cons hd tl) = "(" ++ (intercalate " " (map show (hd:(values tl)))) ++ ")"
  show Undefined = "<undefined>"
  show (Fn _) = "<function>"
  show (Closure _ _ _) = "<closure>"
  show (Special _) = "<special>"

type EvalResult = Either EvalError Value
type Eval a = ReaderT (Env Value) (ExceptT EvalError IO) a

data ReadError =
    Incomplete
  | ExpectedChar Char
  | BadNumber String deriving (Show)

data Reader a = R (String -> (String, Either ReadError a))

runRead :: String -> Reader a -> (String, Either ReadError a)
runRead s (R rf) = rf s

instance Functor Reader where
  fmap f (R r) = R $ (fmap . fmap . fmap) f r

instance Monad Reader where
  return x = R $ \s -> (s, pure x)
  r >>= bf = R $ \s ->
    let (s', result) = runRead s r
    in case result of
      (Left e) -> (s', Left e)
      (Right res) -> runRead s' (bf res)

instance Applicative Reader where
  pure = return
  (<*>) = ap

failRead :: ReadError -> Reader ()
failRead err = R $ \s -> (s, Left err)

expectChar :: Char -> Reader ()
expectChar c = R $ \s -> case s of
  (a:cs) | c == a -> (cs, Right ())
  _ -> (s, Left $ ExpectedChar c)

peek :: Reader (Maybe Char)
peek = R $ \s -> (s, Right $ listToMaybe s)

peekOne :: Reader Char
peekOne = R $ \s -> case s of
  [] -> (s, Left Incomplete)
  c:_ -> (s, Right c)

consumeNext :: Reader ()
consumeNext = R $ \s -> case s of
  [] -> (s, Left Incomplete)
  c:cs -> (cs, Right ())

readWhile :: (Char -> Bool) -> Reader String
readWhile p = R $ \s -> let (str, rest) = span p s in (rest, Right str)

readUntil :: (Char -> Bool) -> Reader String
readUntil p = readWhile (not . p)

whitespace :: Reader ()
whitespace = R $ \s -> let r = dropWhile isSpace s in (r, Right ())

readStringContents :: Reader String
readStringContents = do
  s <- readWhile (/= '"')
  expectChar '"'
  return s

readStringExpr :: Reader Value
readStringExpr = fmap Str readStringContents

readListExpr :: Reader Value
readListExpr = do
  whitespace
  c <- peekOne
  case c of
    ')' -> consumeNext >> pure Nil
    '.' -> do
      consumeNext
      last <- readExpr
      whitespace
      expectChar ')'
      pure last
    _ -> do
      first <- readExpr
      rest <- readListExpr
      pure $ Cons first rest

isDelimiter :: Char -> Bool
isDelimiter c = isSpace c || c == '(' || c == ')' || c == '"'

tryReadNumber :: String -> Either ReadError Integer
tryReadNumber s = case (reads s) of
  [(i, "")] -> Right i
  _ -> Left (BadNumber s)
  
readNumber :: Reader Integer
readNumber = R $ \s -> let (i, rest) = span (not . isDelimiter) s in (rest, tryReadNumber i)

readNumberOrSym :: Char -> (Integer -> Integer) -> Reader Value
readNumberOrSym prefix modifier = do
  mNext <- peek
  case mNext of
    Nothing -> return (Symbol [prefix])
    Just c | isDelimiter c -> return (Symbol [prefix])
    _ -> fmap (Number . modifier) readNumber
  
readExpr :: Reader Value
readExpr = do
  whitespace
  c <- peekOne
  case c of
    '"' -> consumeNext >> readStringExpr
    '(' -> consumeNext >> readListExpr
    '+' -> consumeNext >> (readNumberOrSym '+' id)
    '-' -> consumeNext >> (readNumberOrSym '-' negate)
    n | isDigit n -> fmap Number readNumber
    _ -> fmap Symbol (readUntil isDelimiter)
