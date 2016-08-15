module PScheme.Reader (
  runRead,
  readExpr,
  Value(..),
  ReadError(..),
  Eval,
  EvalError(..),
  EvalResult,
  consToList,
  values,
  readToken,
  readTokens
  ) where

import Data.Char (isSpace, isDigit, isNumber, isLetter)
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
  show (Cons v Nil) = "(" ++ (show v) ++ ")"
  show (Cons hd tl@(Cons _ _)) =  "(" ++ (intercalate " " (map show (hd:(values tl)))) ++ ")"
  show (Cons x y) = "(" ++ (show x) ++ " . " ++ (show y) ++ ")"
  show Undefined = "<undefined>"
  show (Fn _) = "<function>"
  show (Closure _ _ _) = "<closure>"
  show (Special _) = "<special>"

type EvalResult = Either EvalError Value
type Eval a = ReaderT (Env Value) (ExceptT EvalError IO) a

data ReadError =
    Incomplete
  | ExpectedChar Char
  | InvalidEscape Char
  | InvalidChar Char
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

failRead :: ReadError -> Reader a
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

data NumSign = Positive | Negative deriving (Show)
data Token =
    TNumber (Maybe NumSign) String
  | TStr String
  | TSym String
  | OpenParen
  | CloseParen deriving (Show)

newtype CharBuf = CharBuf String

emptyBuf :: CharBuf
emptyBuf = CharBuf []

isEmptyBuf :: CharBuf -> Bool
isEmptyBuf (CharBuf cs) = null cs

singleBuf :: Char -> CharBuf
singleBuf c = CharBuf [c]

appendChar :: Char -> CharBuf -> CharBuf
appendChar c (CharBuf buf) = CharBuf $ c:buf

data EscapeState = ExpectingEscape | NoEscape
data NumState = Plus | Minus | WithSign NumSign CharBuf | NoSign Char CharBuf

instance Show NumState where
  show Plus = "+"
  show Minus = "-"
  show (WithSign Positive buf) = '+':(show buf)
  show (WithSign Negative buf) = '-':(show buf)
  show (NoSign fc buf) = fc:(show buf)

instance Show CharBuf where
  show (CharBuf buf) = reverse buf

data PartialToken =
    PartialString CharBuf EscapeState
  | PartialNum NumState
  | PartialSym CharBuf
  | POpen
  | PClose
  | None

numStateToken :: NumState -> Token
numStateToken Plus = TSym "+"
numStateToken Minus = TSym "-"
numStateToken (WithSign sign buf) = TNumber (Just sign) (show buf)
numStateToken (NoSign first restBuf) = TNumber Nothing (first:(show restBuf))

appendNumChar :: Char -> NumState -> NumState
appendNumChar c Plus = WithSign Positive (singleBuf c)
appendNumChar c Minus = WithSign Negative (singleBuf c)
appendNumChar c (WithSign sign buf) = WithSign sign (appendChar c buf)
appendNumChar c (NoSign f buf) = NoSign f (appendChar c buf)

isValidFollowingSymbolChar c = isLetter c

readToken' :: PartialToken -> String -> (String, Either ReadError (Maybe Token))
readToken' None [] = ([], Right Nothing)
readToken' None ('(':cs) = (cs, Right (Just OpenParen))
readToken' None (')':cs) = (cs, Right (Just CloseParen))
readToken' None ('"':cs) = readToken' (PartialString emptyBuf NoEscape) cs
readToken' None ('-':cs) = readToken' (PartialNum Minus) cs
readToken' None ('+':cs) = readToken' (PartialNum Plus) cs
readToken' None (c:cs) | isSpace c = readToken' None cs
readToken' None (c:cs) | isNumber c = readToken' (PartialNum (NoSign c emptyBuf)) cs
readToken' None (c:cs) | otherwise = readToken' (PartialSym $ singleBuf c) cs

readToken' (PartialString _ _) [] = ([], Left Incomplete)
readToken' (PartialString buf NoEscape) (c:cs) =
  case c of
    '"' -> (cs, Right $ Just $ TStr $ show buf)
    '\\' -> readToken' (PartialString buf ExpectingEscape) cs
    _ -> readToken' (PartialString (appendChar c buf) NoEscape) cs

readToken' (PartialString buf ExpectingEscape) s@(c:cs) =
  case c of
    '"' -> readToken' (PartialString (appendChar '"' buf) NoEscape) cs
    'n' -> readToken' (PartialString (appendChar '\n' buf) NoEscape) cs
    '\\' -> readToken' (PartialString (appendChar '\\' buf) NoEscape) cs
    _ -> (s, Left $ InvalidEscape c)

readToken' (PartialNum state) [] = ([], Right $ Just $ numStateToken state)
readToken' (PartialNum state) (c:cs) | isNumber c = readToken' (PartialNum (appendNumChar c state)) cs
readToken' (PartialNum state) s@(c:_) | isDelimiter c = (s, Right $ Just $ numStateToken state)
readToken' (PartialNum state) s@(c:_) = (s, Left $ BadNumber $ "Invalid digit '" ++ [c] ++ "' after " ++ (show state))

readToken' (PartialSym buf) [] = ([], Right $ Just $ TSym $ show buf)
readToken' (PartialSym buf) s@(c:_) | isDelimiter c = (s, Right $ Just $ TSym $ show buf)
readToken' (PartialSym buf) (c:cs) | isValidFollowingSymbolChar c = readToken' (PartialSym $ appendChar c buf) cs
readToken' (PartialSym _) s@(c:_) = (s, Left $ InvalidChar c)

readToken :: String -> (String, Either ReadError (Maybe Token))
readToken = readToken' None

readTokens :: String -> Either ReadError [Token]
readTokens s =
  let (rest, res) = readToken s
  in  case res of
    Left err -> Left err
    Right Nothing -> Right []
    Right (Just token) -> fmap (token:) (readTokens rest)

