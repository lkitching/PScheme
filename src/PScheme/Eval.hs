module PScheme.Eval where

import PScheme.Reader (Expr(..))
import Data.Traversable (sequence)
import qualified Data.Map.Strict as M

data EvalError =
    UnboundSymbol String
  | OperatorRequired
  | TypeError String PValue

instance Show EvalError where
  show (UnboundSymbol sym) = "Unbound symbol: " ++ sym
  show OperatorRequired = "Operator required"
  show (TypeError ty value) = "Unexpected type for " ++ (show value) ++ ": required " ++ ty

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
type Env = M.Map String PValue

newEnv :: Env
newEnv = M.empty

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

defaultEnv :: Env
defaultEnv = M.fromList [("+", (Fn plusFn)),
                         ("-", (Fn minusFn)),
                         ("*", (Fn $ arithFn product)),
                         ("if", (Special ifSpecial))]

eval :: Env -> Expr -> EvalResult
eval env expr = case expr of
  Number i -> Right $ PNumber i
  Str s -> Right $ PStr s
  Symbol s -> case (M.lookup s env) of
    Just v -> Right v
    Nothing -> Left (UnboundSymbol s)
  List l -> case l of
    [] -> Left OperatorRequired
    e:es -> (eval env e) >>= (\fe -> applyOp env fe es)  
