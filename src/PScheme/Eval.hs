module PScheme.Eval where

import PScheme.Reader
import PScheme.Env
import Data.Traversable (sequence)
import Data.Foldable (traverse_)
import Control.Applicative (liftA2)
import Control.Monad (foldM)
import qualified Data.Map.Strict as M
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except

isTruthy :: Value -> Bool
isTruthy (Number 0) = False
isTruthy (Str "") = False
isTruthy _ = True

applyFnM :: ([Value] -> EvalResult) -> [Value] -> Eval Value
applyFnM f exprs = do
  vals <- traverse eval exprs
  lift $ exceptT $ f vals

failEval :: EvalError -> Eval a
failEval err = lift $ exceptT $ Left err

withEnv :: Env Value -> Eval a -> Eval a
withEnv env = local (const env)

applyOpM :: Value -> [Value] -> Eval Value
applyOpM (Fn f) exprs = applyFnM f exprs
applyOpM (Special f) exprs = f exprs
applyOpM (Closure cEnv paramNames body) argExprs = do
  args <- traverse eval argExprs
  if (length paramNames) == (length args) then do
    argFrame <- liftIO $ mapToFrame $ M.fromList (zip paramNames args)
    let env' = pushEnv argFrame cEnv
    withEnv env' (eval body)
  else failEval $ ArityError (length paramNames) (length args)  
applyOpM v _ = failEval $ TypeError "applyable" v

expectNum :: Value -> Either EvalError Integer
expectNum (Number i) = Right i
expectNum v = Left $ TypeError "Number" v

arithFn :: ([Integer] -> Integer) -> [Value] -> EvalResult
arithFn f vs = do
  nums <- traverse expectNum vs
  return $ Number $ f nums

plusFn :: [Value] -> EvalResult
plusFn = arithFn sum

minusFn :: [Value] -> EvalResult
minusFn = arithFn sub where
  sub [] = 0
  sub [i] = negate i
  sub ls = foldl1 (-) ls

ifSpecial :: [Value] -> Eval Value
ifSpecial [test, ifTrue, ifFalse] = do
  b <- eval test
  eval (if (isTruthy b) then ifTrue else ifFalse)
ifSpecial exprs = failEval $ FormError "if form requires test ifTrue and ifFalse expressions" exprs

type ExprEval a = Value -> Eval a

symbolExpr :: ExprEval String
symbolExpr (Symbol s) = pure s
symbolExpr e = failEval $ FormError "expected symbol" [e]

listSchema :: ExprEval [Value]
listSchema Nil = pure []
listSchema (Cons hd tl) = pure $ consToList hd tl
listSchema e = failEval $ FormError "expected list" [e]

listOf :: (ExprEval a) -> (ExprEval [a])
listOf s e = (listSchema e) >>= (traverse s)

pairOf :: (ExprEval a) -> (ExprEval b) -> (ExprEval (a, b))
pairOf fs ss e = do
  l <- listSchema e
  case l of
    [fe, se] -> liftA2 (,) (fs fe) (ss se)
    _ -> failEval $ FormError "expected pair" l

valueOf :: (ExprEval Value)
valueOf = eval

anyExpr :: ExprEval Value
anyExpr = pure
  
letSpecial :: [Value] -> Eval Value
letSpecial [bindingExpr, body] = do
  bindingValues <-  (listOf (pairOf symbolExpr valueOf)) bindingExpr
  ef <- liftIO $ mapToFrame $ M.fromList bindingValues
  local (pushEnv ef) (eval body)
letSpecial exprs = failEval $ FormError "let form requires binding list and expression to evaluate" exprs

evalNamed :: Env Value -> (String, Value) -> Eval (String, Value)
evalNamed env (name, expr) = do
  val <- withEnv env (eval expr)
  pure (name, val)
  
letrecSpecial :: [Value] -> Eval Value
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
  withEnv tmpEnv (eval body)
  
letStarSpecial :: [Value] -> Eval Value
letStarSpecial [bindingsExpr, body] = do
  bindingValues <- (listOf (pairOf symbolExpr anyExpr)) bindingsExpr
  env <- ask
  frameMapping <- foldM (accBindings env) M.empty bindingValues
  frame <- liftIO $ mapToFrame frameMapping
  let newEnv = pushEnv frame env
  withEnv newEnv (eval body) where
    accBindings :: Env Value -> M.Map String Value -> (String, Value) -> Eval (M.Map String Value)
    accBindings baseEnv accMappings (name, expr) = do
      frame <- liftIO $ mapToFrame $ accMappings
      let env = pushEnv frame baseEnv
      val <- withEnv env (eval expr)
      return $ M.insert name val accMappings
    
letStarSpecial exprs = failEval $ FormError "let* requires binding list followed by a body" exprs

lambdaSpecial :: [Value] -> Eval Value
lambdaSpecial [params, body] = do
  paramNames <- listOf symbolExpr params
  env <- ask
  pure $ Closure env paramNames body
lambdaSpecial exprs = failEval $ FormError "lambda form requires parameter list followed by a body" exprs

quoteSpecial :: [Value] -> Eval Value
quoteSpecial [e] = pure e
quoteSpecial exprs = failEval $ FormError "expected single expression to quote." exprs

expectList :: Value -> Either EvalError [Value]
expectList v = case v of
  Nil -> pure []
  (Cons hd tl) -> pure $ consToList hd tl
  _ -> Left $ TypeError "list" v
  
carFn :: [Value] -> EvalResult
carFn [v] = do
  l <- expectList v
  case l of
    [] -> Left $ ListError $ "Empty list"
    (x:_) -> Right x
carFn vs = Left $ ArityError 1 (length vs)

listToCons :: [Value] -> Value
listToCons [] = Nil
listToCons (x:xs) = Cons x (listToCons xs)

cdrFn :: [Value] -> EvalResult
cdrFn [v] = do
  l <- expectList v
  case l of
    [] -> Left $ ListError $ "Empty list"
    (_:xs) -> Right $ listToCons xs
cdrFn vs = Left $ ArityError 1 (length vs)  
  
defaultEnv :: IO (Env Value)
defaultEnv = envOf $ M.fromList [("+", (Fn plusFn)),
                                 ("-", (Fn minusFn)),
                                 ("*", (Fn $ arithFn product)),
                                 ("if", (Special ifSpecial)),
                                 ("let", (Special letSpecial)),
                                 ("let*", (Special letStarSpecial)),
                                 ("letrec", (Special letrecSpecial)),
                                 ("lambda", (Special lambdaSpecial)),
                                 ("quote", (Special quoteSpecial)),
                                 ("list", (Fn $ pure . listToCons)),
                                 ("car", (Fn carFn)),
                                 ("cdr", (Fn cdrFn))]
  

exceptT :: Monad m => Either e a -> ExceptT e m a
exceptT (Left e) = throwE e
exceptT (Right v) = return v

eval :: Value -> Eval Value
eval expr = case expr of
  Number i -> pure expr
  Str s -> pure expr
  Symbol s -> do
    env <- ask
    var <- liftIO $ envLookup s env
    case var of
      Just v -> return v
      Nothing -> lift $ throwE (UnboundSymbol s)
  Nil -> pure expr
  (Cons hd tl) -> do
    first <- eval hd
    applyOpM first (tail $ consToList first tl)
  Undefined -> failEval DerefUndefinedError
  _ -> pure expr

runEval :: Env Value -> Eval a -> IO (Either EvalError a)
runEval env e = runExceptT $ runReaderT e env
