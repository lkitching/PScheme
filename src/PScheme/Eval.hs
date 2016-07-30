module PScheme.Eval where

import PScheme.Reader (Expr(..))
import PScheme.Env
import Data.Traversable (sequence)
import Data.Foldable (traverse_)
import Control.Applicative (liftA2)
import qualified Data.Map.Strict as M
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except

data EvalError =
    UnboundSymbol String
  | OperatorRequired
  | TypeError String PValue
  | FormError String [Expr]
  | ArityError Int Int
  | DerefUndefinedError

instance Show EvalError where
  show (UnboundSymbol sym) = "Unbound symbol: " ++ sym
  show OperatorRequired = "Operator required"
  show (TypeError ty value) = "Unexpected type for " ++ (show value) ++ ": required " ++ ty
  show (FormError msg form) = "Invalid form: " ++ msg
  show (ArityError expected actual) = "Wrong number of arguments (" ++ (show actual) ++ "). Expected: " ++ (show expected)
  show DerefUndefinedError = "Cannot evaluate undefined"

data PValue =
    PNumber Integer
  | PStr String
  | Undefined
  | Fn ([PValue] -> EvalResult)
  | Closure (Env PValue) [String] Expr
  | Special ([Expr] -> Eval PValue)

instance Show PValue where
  show (PNumber i) = show i
  show (PStr s) = s
  show Undefined = "<undefined>"
  show (Fn _) = "<function>"
  show (Closure _ _ _) = "<closure>"
  show (Special _) = "<special>"

isTruthy :: PValue -> Bool
isTruthy (PNumber 0) = False
isTruthy (PStr "") = False
isTruthy _ = True

type EvalResult = Either EvalError PValue
type Eval a = ReaderT (Env PValue) (ExceptT EvalError IO) a

applyFnM :: ([PValue] -> EvalResult) -> [Expr] -> Eval PValue
applyFnM f exprs = do
  vals <- traverse evalM exprs
  lift $ exceptT $ f vals

failEval :: EvalError -> Eval a
failEval err = lift $ exceptT $ Left err

applyOpM :: PValue -> [Expr] -> Eval PValue
applyOpM (Fn f) exprs = applyFnM f exprs
applyOpM (Special f) exprs = f exprs
applyOpM (Closure cEnv paramNames body) argExprs = do
  args <- traverse evalM argExprs
  if (length paramNames) == (length args) then do
    argFrame <- liftIO $ mapToFrame $  M.fromList (zip paramNames args)
    let env' = pushEnv argFrame cEnv
    local (const env') (evalM body)
  else failEval $ ArityError (length paramNames) (length args)  
applyOpM _ _ = failEval OperatorRequired

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

ifSpecial :: [Expr] -> Eval PValue
ifSpecial [test, ifTrue, ifFalse] = do
  b <- evalM test
  evalM (if (isTruthy b) then ifTrue else ifFalse)
ifSpecial exprs = failEval $ FormError "if form requires test ifTrue and ifFalse expressions" exprs

type ExprEval a = Expr -> Eval a

symbolExpr :: ExprEval String
symbolExpr (Symbol s) = pure s
symbolExpr e = failEval $ FormError "expected symbol" [e]

listSchema :: ExprEval [Expr]
listSchema (List es) = pure es
listSchema e = failEval $ FormError "expected list" [e]

listOf :: (ExprEval a) -> (ExprEval [a])
listOf s e = (listSchema e) >>= (traverse s)

pairOf :: (ExprEval a) -> (ExprEval b) -> (ExprEval (a, b))
pairOf fs ss e = do
  l <- listSchema e
  case l of
    [fe, se] -> liftA2 (,) (fs fe) (ss se)
    _ -> failEval $ FormError "expected pair" l

valueOf :: (ExprEval PValue)
valueOf = evalM

anyExpr :: ExprEval Expr
anyExpr = pure
  
letSpecial :: [Expr] -> Eval PValue
letSpecial [bindingExpr, body] = do
  bindingValues <-  (listOf (pairOf symbolExpr valueOf)) bindingExpr
  ef <- liftIO $ mapToFrame $ M.fromList bindingValues
  local (pushEnv ef) (evalM body)
letSpecial exprs = failEval $ FormError "let form requires binding list and expression to evaluate" exprs

evalNamed :: Env PValue -> (String, Expr) -> Eval (String, PValue)
evalNamed env (name, expr) = do
  val <- local (const env) (evalM expr)
  pure (name, val)
  
letrecSpecial :: [Expr] -> Eval PValue
letrecSpecial [bindingsExpr, body] = do
  bindingValues <- (listOf (pairOf symbolExpr anyExpr)) bindingsExpr
  --temporarily bind expressions to Unbound
  let tmpBindings = (fmap . fmap) (const Undefined) bindingValues
  tmpFrame <- liftIO $ mapToFrame $ M.fromList tmpBindings
  env <- ask
  let tmpEnv = pushEnv tmpFrame env
  vals <- traverse (evalNamed tmpEnv) bindingValues

  --re-write temp bindings
  liftIO $ traverse_ (setBinding tmpEnv) vals
  local (const tmpEnv) (evalM body)
  

lambdaSpecial :: [Expr] -> Eval PValue
lambdaSpecial [params, body] = do
  paramNames <- listOf symbolExpr params
  env <- ask
  pure $ Closure env paramNames body
lambdaSpecial exprs = failEval $ FormError "lambda form requires parameter list followed by a body" exprs  
  
defaultEnv :: IO (Env PValue)
defaultEnv = envOf $ M.fromList [("+", (Fn plusFn)),
                                 ("-", (Fn minusFn)),
                                 ("*", (Fn $ arithFn product)),
                                 ("if", (Special ifSpecial)),
                                 ("let", (Special letSpecial)),
                                 ("letrec", (Special letrecSpecial)),
                                 ("lambda", (Special lambdaSpecial))]
  

exceptT :: Monad m => Either e a -> ExceptT e m a
exceptT (Left e) = throwE e
exceptT (Right v) = return v

evalM :: Expr -> Eval PValue
evalM expr = case expr of
  Number i -> return $ PNumber i
  Str s -> return $ PStr s
  Symbol s -> do
    env <- ask
    var <- liftIO $ envLookup s env
    case var of
      Just v -> return v
      Nothing -> lift $ throwE (UnboundSymbol s)
  List l -> case l of
    [] -> lift $ throwE OperatorRequired
    e:es -> do
      first <- evalM e
      applyOpM first es

runEval :: Env PValue -> Eval a -> IO (Either EvalError a)
runEval env e = runExceptT $ runReaderT e env
