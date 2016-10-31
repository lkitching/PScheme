module PScheme.Reader (
  Value(..),
  ReadError(..),
  Eval,
  EvalError(..),
  EvalResult,
  consToList,
  values,
  readTokens,
  parseString,
  readAll,
  readStringOne,
  listToCons,
  ParseState(..),
  NumSign(..),
  Token(..),
  ParseOutcome,
  ParseResult,
  parse,
  parseNext,
  parsedValue,
  escapeChars
  ) where

import Data.Char (isSpace, isDigit, isNumber, isLetter)
import Control.Monad
import Control.Applicative
import Data.Maybe (listToMaybe)
import Data.List (intercalate)
import Control.Monad.Trans.State.Lazy (StateT)
import Control.Monad.Trans.Except (ExceptT)
import PScheme.Env (Env)

data EvalError =
    UnboundSymbol String
  | ReadError ReadError
  | UnboundRef String
  | OperatorRequired
  | TypeError String Value
  | FormError String [Value]
  | ArityError Int Int
  | DerefUndefinedError
  | ListError String deriving (Eq)

instance Show EvalError where
  show (UnboundSymbol sym) = "Unbound symbol: " ++ sym
  show (ReadError err) = "Read error: " ++ (show err)
  show (UnboundRef name) = "Unbound variable: " ++ name
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
  | Special (Value -> Eval Value)
  | Macro (Env Value) [String] Value

instance Eq Value where
  (Number i) == (Number j) = i == j
  (Str s1) == (Str s2) = s1 == s2
  (Symbol s1) == (Symbol s2) = s1 == s2
  Nil == Nil = True
  (Cons h1 t1) == (Cons h2 t2) = h1 == h2 && t1 == t2
  Undefined == Undefined = True
  _ == _ = False

values :: Value -> [Value]
values Nil = []
values (Cons hd tl) = hd:(values tl)
values e = [e]

listToCons :: [Value] -> Value
listToCons = foldr Cons Nil

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
  show (Macro _ _ _) = "<macro>"

type EvalResult = Either EvalError Value
type Eval a = ExceptT EvalError (StateT (Env Value) IO) a

data ReadError =
    Incomplete
  | ExpectedChar Char
  | InvalidEscape Char
  | InvalidChar Char
  | BadNumber String
  | UnbalancedParens deriving (Eq, Show)

isDelimiter :: Char -> Bool
isDelimiter c = isSpace c || c `elem` "()\"',"

data NumSign = Positive | Negative deriving (Eq, Show)

signChar :: NumSign -> Char
signChar Positive = '+'
signChar Negative = '-'

data Token =
    TNumber (Maybe NumSign) String
  | TStr String
  | TSym String
  | OpenParen
  | CloseParen
  | Quote
  | Unquote deriving (Eq)

instance Show Token where
  show (TNumber Nothing s) = s
  show (TNumber (Just sign) s) = (signChar sign):s
  show (TStr s) = show s
  show (TSym s) = s
  show OpenParen = "("
  show CloseParen = ")"
  show Quote = "'"
  show Unquote = ","

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

isValidFollowingSymbolChar c = True
escapeChars = ['\\', 'n', '"']

readToken' :: PartialToken -> String -> (String, Either ReadError (Maybe Token))
readToken' None [] = ([], Right Nothing)
readToken' None ('(':cs) = (cs, Right (Just OpenParen))
readToken' None (')':cs) = (cs, Right (Just CloseParen))
readToken' None ('"':cs) = readToken' (PartialString emptyBuf NoEscape) cs
readToken' None ('\'':cs) = (cs, Right (Just Quote))
readToken' None (',':cs) = (cs, Right (Just Unquote))
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
  if  c `elem` escapeChars
    then readToken' (PartialString (appendChar c buf) NoEscape) cs
    else (s, Left $ InvalidEscape c)

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

data PartialParse =
    PartialList [Value]
  | PartialQuote
  | PartialUnquote deriving (Show)
    
type ParseState = [PartialParse]
  
data ParseOutcome =
    CompleteParse Value [Token]
  | Moar ParseState deriving (Show)

parsedValue :: ParseOutcome -> Maybe Value
parsedValue (CompleteParse v _) = Just v
parsedValue _ = Nothing

type ParseResult = Either ReadError ParseOutcome

atomTokenValue :: Token -> Value
atomTokenValue (TNumber (Just Negative) str) = Number $ negate $ read str
atomTokenValue (TNumber _ str) = Number $ read str
atomTokenValue (TStr s) = Str s
atomTokenValue (TSym s) = Symbol s
atomTokenValue _ = error "!!!"

parseFailed :: ReadError -> ParseResult
parseFailed = Left

finishList :: [Value] -> Value
finishList = listToCons . reverse

consumePartials :: ParseState -> Value -> Either ParseState Value
consumePartials [] v = Right v
consumePartials ((PartialList vs):pss) v = Left ((PartialList (v:vs)):pss)
consumePartials (PartialQuote:pss) v = consumePartials pss (Cons (Symbol "quote") (Cons v Nil))
consumePartials (PartialUnquote:pss) v = consumePartials pss (Cons (Symbol "unquote") (Cons v Nil))

appendValue :: ParseState -> Value -> Either ParseState Value
appendValue [] v = Right v
appendValue ((PartialList vs):pss) v = Left $ (PartialList (v:vs)):pss
--NOTE: top item is either quote or unquote
appendValue pss v = consumePartials pss v

parse' :: ParseState -> [Token] -> ParseResult
parse' ps [] = Right $ Moar ps
parse' ps (OpenParen:ts) = parse' ((PartialList []):ps) ts
parse' ps (Quote:ts) = parse' (PartialQuote:ps) ts
parse' ps (Unquote:ts) = parse' (PartialUnquote:ps) ts
parse' [] ts@(CloseParen:_) = parseFailed UnbalancedParens
parse' ((PartialList vs):pss) (CloseParen:ts) = case (consumePartials pss (finishList vs)) of
  Left pss -> parse' pss ts
  Right v -> Right $ CompleteParse v ts
parse' (ps:_) (CloseParen:ts) = parseFailed UnbalancedParens
parse' pss (t:ts) = case (appendValue pss (atomTokenValue t)) of
  Left pss' -> parse' pss' ts
  Right v -> Right $ CompleteParse v ts

parse :: [Token] -> ParseResult
parse = parse' []

parseNext :: ParseOutcome -> [Token] -> ParseResult
parseNext (CompleteParse _ unreadToks) newToks = parse' [] (unreadToks ++ newToks)
parseNext (Moar state) tokens = parse' state tokens

parseString :: String -> ParseResult
parseString s = case (readTokens s) of
  (Left err) -> parseFailed err
  Right tokens -> parse tokens

readAll :: String -> Either ReadError [Value]
readAll s = readTokens s >>= parseAll where
  parseAll :: [Token] -> Either ReadError [Value]
  parseAll ts = do
    state <- parse ts
    case state of
      CompleteParse v [] -> pure [v]
      CompleteParse v ts' -> fmap (v:) (parseAll ts')
      _ -> Left Incomplete
      
readStringOne :: String -> Either ReadError Value
readStringOne s = do
  vs <- readAll s
  case vs of
    [] -> Left Incomplete
    v:_ -> pure v

