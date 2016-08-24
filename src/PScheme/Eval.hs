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
isTruthy Nil = False
isTruthy _ = True

applyFnM :: ([Value] -> EvalResult) -> [Value] -> Eval Value
applyFnM f exprs = do
  vals <- traverse eval exprs
  lift $ exceptT $ f vals

failEval :: EvalError -> Eval a
failEval err = lift $ exceptT $ Left err

withEnv :: Env Value -> Eval a -> Eval a
withEnv env = local (const env)

tryBindArgs :: [String] -> [Value] -> Either EvalError (M.Map String Value)
tryBindArgs syms args = bind syms args M.empty where
  bind [] [] m = pure m
  bind [s] [v] m = pure $ M.insert s v m
  bind [s] vs m = pure $ M.insert s (listToCons vs) m
  bind (_:_) [] _ = Left $ ArityError (length syms) (length args)
  bind (s:ss) (v:vs) m = bind ss vs (M.insert s v m)

paramsFrame :: [String] -> [Value] -> Eval (EnvFrame Value)
paramsFrame paramNames values = do
  argMapping <- lift $ exceptT $ tryBindArgs paramNames values
  liftIO $ mapToFrame argMapping

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
  
car :: Value -> Eval Value
car Nil = pure Nil
car (Cons h _) = pure h
car v = failEval $ TypeError "list" v

uncons :: Value -> Eval (Value, Value)
uncons Nil = pure (Nil, Nil)
uncons (Cons h tl) = pure (h, tl)
uncons v = failEval $ TypeError "list" v

ifSpecial :: Value -> Eval Value
ifSpecial v = do
  (test, t) <- uncons v
  (ifTrue, t) <- uncons t
  (ifFalse, _) <- uncons t
  b <- eval test
  eval (if (isTruthy b) then ifTrue else ifFalse)

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

anyVal :: ExprEval Value
anyVal = pure
  
letSpecial :: Value -> Eval Value
letSpecial l = do
  (bindingValues, body) <- pairOf (listOf (pairOf symbolExpr valueOf)) anyVal l
  ef <- liftIO $ mapToFrame $ M.fromList bindingValues
  local (pushEnv ef) (eval body)

evalNamed :: Env Value -> (String, Value) -> Eval (String, Value)
evalNamed env (name, expr) = do
  val <- withEnv env (eval expr)
  pure (name, val)
  
letrecSpecial :: Value -> Eval Value
letrecSpecial v = do
  (bindingValues, body) <- pairOf (listOf (pairOf symbolExpr anyVal)) anyVal v
  --temporarily bind expressions to Unbound
  let tmpBindings = (fmap . fmap) (const Undefined) bindingValues
  tmpFrame <- liftIO $ mapToFrame $ M.fromList tmpBindings
  env <- ask
  let tmpEnv = pushEnv tmpFrame env
  vals <- traverse (evalNamed tmpEnv) bindingValues

  --re-write temp bindings
  liftIO $ traverse_ (setBinding tmpEnv) vals
  withEnv tmpEnv (eval body)
  
letStarSpecial :: Value -> Eval Value
letStarSpecial v = do
  (bindingValues, body) <- pairOf (listOf (pairOf symbolExpr anyVal)) anyVal v
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

lambdaSpecial :: Value -> Eval Value
lambdaSpecial v = do
  (paramNames, body) <- pairOf (listOf symbolExpr) anyVal v
  env <- ask
  pure $ Closure env paramNames body

quoteSpecial :: Value -> Eval Value
quoteSpecial Nil = failEval $ FormError "expected single expression to quote" []
quoteSpecial (Cons v Nil) = pure v
quoteSpecial v@(Cons _ _) = failEval $ FormError "expected single expression to quote." [v]
quoteSpecial v = pure v

macroSpecial :: Value -> Eval Value
macroSpecial v = do
  (params, body) <- pairOf (listOf symbolExpr) anyVal v
  env <- ask
  pure $ Macro env params body
  
carFn :: [Value] -> EvalResult
carFn [v] = case v of
  Nil -> pure Nil
  (Cons hd _) -> pure hd
  _ -> Left $ TypeError "list" v
carFn vs = Left $ ArityError 1 (length vs)

cdrFn :: [Value] -> EvalResult
cdrFn [v] = case v of
  Nil -> pure Nil
  (Cons _ tl) -> pure tl
  _ -> Left $ TypeError "list" v
cdrFn vs = Left $ ArityError 1 (length vs)

consFn :: [Value] -> EvalResult
consFn [f, s] = Right $ Cons f s
consFn vs = Left $ ArityError 2 (length vs)
  
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
                                 ("cdr", (Fn cdrFn)),
                                 ("cons", (Fn consFn)),
                                 ("macro", (Special macroSpecial))]
  
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
    case first of
      (Special f) -> f tl
      (Fn f) -> applyFnM f (values tl)
      (Closure cEnv paramNames body) -> do
        args <- traverse eval (values tl)
        argFrame <- paramsFrame paramNames args
        let env' = pushEnv argFrame cEnv
        withEnv env' (eval body)
      (Macro cEnv paramNames body) -> do
        argFrame <- paramsFrame paramNames (values tl)
        let env' = pushEnv argFrame cEnv
        newBody <- withEnv env' (eval body)
        eval newBody
  Undefined -> failEval DerefUndefinedError
  _ -> pure expr

runEval :: Env Value -> Eval a -> IO (Either EvalError a)
runEval env e = runExceptT $ runReaderT e env
