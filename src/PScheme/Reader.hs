module PScheme.Reader (
  runRead,
  readExpr,
  Expr(..),
  ReadError(..),
  ) where

import Data.Char (isSpace, isDigit)
import Control.Monad
import Control.Applicative
import Data.Maybe (listToMaybe)

data Expr =
    Symbol String
  | Number Integer
  | Str String
  | List [Expr] deriving (Show)

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

expectChar :: Char -> Reader ()
expectChar c = R $ \s -> case s of
  (a:cs) | c == a -> (cs, Right ())
  _ -> (s, Left $ ExpectedChar c)

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

readStringExpr :: Reader Expr
readStringExpr = fmap Str readStringContents

tryReadListExpr :: Reader (Maybe Expr)
tryReadListExpr = do
  whitespace
  c <- peekOne
  case c of
    ')' -> consumeNext >> return Nothing
    _ -> fmap Just readExpr

readListExprs :: Reader [Expr]
readListExprs = do
  m <- tryReadListExpr
  case m of
    Nothing -> return []
    Just e -> readListExprs >>= \l -> return (e:l)

readListExpr :: Reader Expr
readListExpr = fmap List readListExprs

isDelimiter :: Char -> Bool
isDelimiter c = isSpace c || c == '(' || c == ')' || c == '"'

tryReadNumber :: String -> Either ReadError Integer
tryReadNumber s = case (reads s) of
  [(i, "")] -> Right i
  _ -> Left (BadNumber s)
  
readNumber :: Reader Integer
readNumber = R $ \s -> let (i, rest) = span (not . isDelimiter) s in (rest, tryReadNumber i)

readExpr :: Reader Expr
readExpr = do
  whitespace
  c <- peekOne
  case c of
    '"' -> consumeNext >> readStringExpr
    '(' -> consumeNext >> readListExpr
    '+' -> consumeNext >> fmap Number readNumber
    '-' -> consumeNext >> fmap (Number . negate) readNumber
    n | isDigit n -> fmap Number readNumber
    _ -> fmap Symbol (readUntil isDelimiter)
  

