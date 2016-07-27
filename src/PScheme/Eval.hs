module PScheme.Eval where

import PScheme.Reader (Expr(..))
import Data.Traversable (sequence)
import Control.Applicative (liftA2)
import qualified Data.Map.Strict as M
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except

data EvalError =
    UnboundSymbol String
  | OperatorRequired
  | TypeError String PValue
  | FormError String [Expr]
  | ArityError Int Int

instance Show EvalError where
  show (UnboundSymbol sym) = "Unbound symbol: " ++ sym
  show OperatorRequired = "Operator required"
  show (TypeError ty value) = "Unexpected type for " ++ (show value) ++ ": required " ++ ty
  show (FormError msg form) = "Invalid form: " ++ msg
  show (ArityError expected actual) = "Wrong number of arguments (" ++ (show actual) ++ "). Expected: " ++ (show expected)

data PValue =
    PNumber Integer
  | PStr String
  | Fn ([PValue] -> EvalResult)
  | Closure Env [String] Expr
  | Special (Env -> [Expr] -> EvalResult)

instance Show PValue where
  show (PNumber i) = show i
  show (PStr s) = s
  show (Fn _) = "<function>"
  show (Closure _ _ _) = "<closure>"
  show (Special _) = "<special>"

isTruthy :: PValue -> Bool
isTruthy (PNumber 0) = False
isTruthy (PStr "") = False
isTruthy _ = True

type EvalResult = Either EvalError PValue
type EnvFrame = M.Map String PValue
type Env = [EnvFrame]

type Eval = ReaderT Env (ExceptT EvalError IO)

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
applyOp env (Closure cEnv paramNames body) argExprs = do
  args <- traverse (eval env) argExprs
  if (length paramNames) == (length args) then
    let argFrame = M.fromList (zip paramNames args)
        env' = pushEnv argFrame cEnv
    in  eval env' body
  else Left $ ArityError (length paramNames) (length args)
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

type ExprEval a = Expr -> Either EvalError a

symbolExpr :: ExprEval String
symbolExpr (Symbol s) = Right s
symbolExpr e = Left $ FormError "expected symbol" [e]

listSchema :: ExprEval [Expr]
listSchema (List es) = Right es
listSchema e = Left $ FormError "expected list" [e]

listOf :: (ExprEval a) -> (ExprEval [a])
listOf s e = (listSchema e) >>= (traverse s)

pairOf :: (ExprEval a) -> (ExprEval b) -> (ExprEval (a, b))
pairOf fs ss e = do
  l <- listSchema e
  case l of
    [fe, se] -> liftA2 (,) (fs fe) (ss se)
    _ -> Left $ FormError "expected pair" l

valueOf :: Env -> (ExprEval PValue)
valueOf = eval
  
letSpecial :: Env -> [Expr] -> EvalResult
letSpecial env [bindingExpr, body] = do
  bindingValues <- (listOf (pairOf symbolExpr (valueOf env))) bindingExpr
  let ef = M.fromList bindingValues
  eval (pushEnv ef env) body
      
letSpecial _ exprs = Left $ FormError "let form requires binding list and expression to evaluate" exprs

lambdaSpecial :: Env -> [Expr] -> EvalResult
lambdaSpecial env [params, body] = do
  paramNames <- listOf symbolExpr params
  Right $ Closure env paramNames body
lambdaSpecial _ exprs = Left $ FormError "lambda form requires parameter list followed by a body" exprs  
  
defaultEnv :: Env
defaultEnv = [M.fromList [("+", (Fn plusFn)),
                          ("-", (Fn minusFn)),
                          ("*", (Fn $ arithFn product)),
                          ("if", (Special ifSpecial)),
                          ("let", (Special letSpecial)),
                          ("lambda", (Special lambdaSpecial))]]

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

exceptT :: Monad m => Either e a -> ExceptT e m a
exceptT (Left e) = throwE e
exceptT (Right v) = return v

evalM :: Expr -> Eval PValue
evalM expr = case expr of
  Number i -> return $ PNumber i
  Str s -> return $ PStr s
  Symbol s -> do
    env <- ask
    case (envLookup s env) of
      Just v -> return v
      Nothing -> lift $ throwE (UnboundSymbol s)
  List l -> case l of
    [] -> lift $ throwE OperatorRequired
    e:es -> do
      first <- evalM e
      env <- ask
      lift $ exceptT $ applyOp env first es

runEval :: Env -> Eval a -> IO (Either EvalError a)
runEval env e = runExceptT $ runReaderT e env
      
      
      
