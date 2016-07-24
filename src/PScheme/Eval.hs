module PScheme.Eval where

import PScheme.Reader (Expr(..))
import Data.Traversable (sequence)
import qualified Data.Map.Strict as M

data EvalError =
    UnboundSymbol String
  | OperatorRequired
  | TypeError String PValue
  | FormError String [Expr]

instance Show EvalError where
  show (UnboundSymbol sym) = "Unbound symbol: " ++ sym
  show OperatorRequired = "Operator required"
  show (TypeError ty value) = "Unexpected type for " ++ (show value) ++ ": required " ++ ty
  show (FormError msg form) = "Invalid form: " ++ msg

data PValue =
    PNumber Integer
  | PStr String
  | Fn ([PValue] -> EvalResult)
  | Special (Env -> [Expr] -> EvalResult)

instance Show PValue where
  show (PNumber i) = show i
  show (PStr s) = s
  show (Fn _) = "<function>"
  show (Special _) = "<special>"

isTruthy :: PValue -> Bool
isTruthy (PNumber 0) = False
isTruthy (PStr "") = False
isTruthy _ = True

type EvalResult = Either EvalError PValue
type EnvFrame = M.Map String PValue
type Env = [EnvFrame]

newEnv :: Env
newEnv = []

envLookup :: String -> Env -> Maybe PValue
envLookup _ [] = Nothing
envLookup k (e:es) = case (M.lookup k e) of
  m@(Just _) -> m
  Nothing -> envLookup k es

pushEnv :: EnvFrame -> Env -> Env
pushEnv = (:)

popEnv :: Env -> Env
popEnv [] = []
popEnv (_:es) = es

applyFn :: Env -> ([PValue] -> EvalResult) -> [Expr] -> EvalResult
applyFn env f es = (traverse (eval env) es) >>= (f $)

applyOp :: Env -> PValue -> [Expr] -> EvalResult
applyOp env (Fn f) es = applyFn env f es
applyOp env (Special f) es = f env es
applyOp _ _ _ = Left OperatorRequired

expectNum :: PValue -> Either EvalError Integer
expectNum (PNumber i) = Right i
expectNum v = Left $ TypeError "Number" v

arithFn :: ([Integer] -> Integer) -> [PValue] -> EvalResult
arithFn f vs = do
  nums <- traverse expectNum vs
  return $ PNumber $ f nums

plusFn :: [PValue] -> EvalResult
plusFn = arithFn sum

minusFn :: [PValue] -> EvalResult
minusFn = arithFn sub where
  sub [] = 0
  sub [i] = negate i
  sub ls = foldl1 (-) ls

ifSpecial :: Env -> [Expr] -> EvalResult
ifSpecial env [test, ifTrue, ifFalse] = do
  b <- eval env test
  eval env (if (isTruthy b) then ifTrue else ifFalse)
ifSpecial _ exprs = Left $ FormError "if form requires test ifTrue and ifFalse expressions" exprs

expectListExpr :: Expr -> Either EvalError [Expr]
expectListExpr (List es) = Right es
expectListExpr e = Left $ FormError "expected list" [e]

expectPairExpr :: Expr -> Either EvalError (Expr, Expr)
expectPairExpr e = do
  l <- expectListExpr e
  case l of
    [e1, e2] -> Right (e1, e2)
    _ -> Left $ FormError "expected pair" l

expectSymbolExpr :: Expr -> Either EvalError String
expectSymbolExpr (Symbol s) = Right s
expectSymbolExpr e = Left $ FormError "expected symbol" [e]

expectBindingPair :: Expr -> Either EvalError (String, Expr)
expectBindingPair e = do
  (nameExpr, valExpr) <- expectPairExpr e
  varName <- expectSymbolExpr nameExpr
  return (varName, valExpr)
  
letSpecial :: Env -> [Expr] -> EvalResult
letSpecial env [bindingExpr, body] = do
  bindingList <- expectListExpr bindingExpr
  bindingValues <- traverse evalBindingPair bindingList
  let ef = M.fromList bindingValues
  eval (pushEnv ef env) body
  where
    evalBindingPair :: Expr -> Either EvalError (String, PValue)
    evalBindingPair e = do
      (name, expr) <- expectBindingPair e
      v <- eval env expr
      return (name, v)
      
letSpecial _ exprs = Left $ FormError "let form requires binding list and expression to evaluate" exprs

defaultEnv :: Env
defaultEnv = [M.fromList [("+", (Fn plusFn)),
                          ("-", (Fn minusFn)),
                          ("*", (Fn $ arithFn product)),
                          ("if", (Special ifSpecial)),
                          ("let", (Special letSpecial))]]

eval :: Env -> Expr -> EvalResult
eval env expr = case expr of
  Number i -> Right $ PNumber i
  Str s -> Right $ PStr s
  Symbol s -> case (envLookup s env) of
    Just v -> Right v
    Nothing -> Left (UnboundSymbol s)
  List l -> case l of
    [] -> Left OperatorRequired
    e:es -> (eval env e) >>= (\fe -> applyOp env fe es)  
