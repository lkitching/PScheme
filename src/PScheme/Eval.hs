module PScheme.Eval where

import PScheme.Reader
import PScheme.Env
import Data.Traversable (sequence)
import Data.Foldable (traverse_)
import Control.Applicative (liftA2, (<*>))
import Control.Monad (foldM)
import qualified Data.Map.Strict as M
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except
import Data.Tuple (swap)

isTruthy :: Value -> Bool
isTruthy (Number 0) = False
isTruthy (Str "") = False
isTruthy Nil = False
isTruthy _ = True

applyFnM :: ([Value] -> EvalResult) -> [Value] -> Eval Value
applyFnM f exprs = do
  vals <- traverse eval exprs
  exceptT $ f vals

failEval :: EvalError -> Eval a
failEval = throwE

tryBindArgs :: [String] -> [Value] -> Either EvalError (M.Map String Value)
tryBindArgs syms args = bind syms args M.empty where
  bind [] [] m = pure m
  bind [s] [v] m = pure $ M.insert s v m
  bind [s] vs m = pure $ M.insert s (listToCons vs) m
  bind (_:_) [] _ = Left $ ArityError (length syms) (length args)
  bind (s:ss) (v:vs) m = bind ss vs (M.insert s v m)

paramsFrame :: [String] -> [Value] -> Eval (EnvFrame Value)
paramsFrame paramNames values = do
  argMapping <- exceptT $ tryBindArgs paramNames values
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

oneOf :: (ExprEval a) -> (ExprEval a)
oneOf s Nil = failEval $ FormError "expected value" [Nil]
oneOf s (Cons v Nil) = s v
oneOf s v@(Cons _ _) = failEval $ FormError "expected single value" [v]
oneOf s v = s v

valueOf :: (ExprEval Value)
valueOf = eval

anyVal :: ExprEval Value
anyVal = pure

anyOne :: ExprEval Value
anyOne = oneOf anyVal

local :: (Env Value -> Env Value) -> Eval a -> Eval a
local f comp = do
  oldEnv <- getEnv
  setEnv (f oldEnv)
  result <- comp
  setEnv oldEnv
  pure result

letSpecial :: Value -> Eval Value
letSpecial l = do
  (bindingValues, body) <- pairOf (listOf (pairOf symbolExpr valueOf)) anyVal l
  ef <- liftIO $ mapToFrame $ M.fromList bindingValues
  local (pushEnv ef) (eval body)

withEnv :: Env Value -> Eval a -> Eval a
withEnv newEnv = local (const newEnv)

setEnv :: Env Value -> Eval ()
setEnv = lift . put

getEnv :: Eval (Env Value)
getEnv = lift get
  
evalNamed :: Env Value -> (String, Value) -> Eval (String, Value)
evalNamed env (name, expr) = do
  val <- withEnv env (eval expr)
  pure (name, val)

setBinding :: Env a -> (String, a) -> IO ()
setBinding env (name, value) = case (findBinding name env) of
    Nothing -> return ()
    Just ref -> setRef ref value
  
letrecSpecial :: Value -> Eval Value
letrecSpecial v = do
  (bindingValues, body) <- pairOf (listOf (pairOf symbolExpr anyVal)) anyVal v
  --temporarily bind expressions to Unbound
  let tmpBindings = (fmap . fmap) (const Undefined) bindingValues
  tmpFrame <- liftIO $ mapToFrame $ M.fromList tmpBindings
  env <- getEnv
  let tmpEnv = pushEnv tmpFrame env
  vals <- traverse (evalNamed tmpEnv) bindingValues

  --re-write temp bindings
  liftIO $ traverse_ (setBinding tmpEnv) vals
  withEnv tmpEnv (eval body)
  
letStarSpecial :: Value -> Eval Value
letStarSpecial v = do
  (bindingValues, body) <- pairOf (listOf (pairOf symbolExpr anyVal)) anyVal v
  env <- getEnv
  frame <- foldM (accBindings env) newFrame bindingValues
  let newEnv = pushEnv frame env
  withEnv newEnv (eval body) where
    accBindings :: Env Value -> EnvFrame Value -> (String, Value) -> Eval (EnvFrame Value)
    accBindings baseEnv frame (name, expr) = do
      let env = pushEnv frame baseEnv
      val <- withEnv env (eval expr)
      liftIO $ addFrameBinding name val frame

lambdaSpecial :: Value -> Eval Value
lambdaSpecial v = do
  (paramNames, body) <- pairOf (listOf symbolExpr) anyVal v
  env <- getEnv
  pure $ Closure env paramNames body

unquote :: Value -> Eval Value
unquote Nil = failEval $ FormError "expected single expression to unquote" []
unquote (Cons v Nil)  = eval v
unquote (Cons v tl) = failEval $ FormError "expected single expression to unquote" [v, tl]
unquote v = eval v

quoteInner :: Value -> Eval Value
quoteInner (Cons (Symbol "unquote") l) = unquote l
quoteInner (Cons hd tl) = pure Cons <*> quoteInner hd <*> quoteInner tl
quoteInner v = pure v

quoteSpecial :: Value -> Eval Value
quoteSpecial Nil = failEval $ FormError "expected single expression to quote" []
quoteSpecial (Cons v Nil) = quoteInner v
quoteSpecial v@(Cons _ _) = failEval $ FormError "expected single expression to quote." [v]
quoteSpecial v = quoteInner v

macroSpecial :: Value -> Eval Value
macroSpecial v = do
  (params, body) <- pairOf (listOf symbolExpr) anyVal v
  env <- getEnv
  pure $ Macro env params body

evalSpecial :: Value -> Eval Value
evalSpecial args = do
  arg <- anyOne args
  --eval form in the current environment
  --eval result in top-level environment
  form <- eval arg
  topEnv <- fmap top getEnv
  withEnv topEnv (eval form)

setSpecial :: Value -> Eval Value
setSpecial args = do
  (sym, valueForm) <- (pairOf symbolExpr anyVal) args
  value <- eval valueForm
  env <- getEnv
  let mRef = findBinding sym env
  case mRef of
    Nothing -> failEval $ UnboundRef sym
    Just ref -> liftIO $ setRef ref value >> pure Nil

beginSpecial :: Value -> Eval Value
beginSpecial args = foldM (\_ f -> eval f) Nil (values args)

defineSpecial :: Value -> Eval Value
defineSpecial args = do
  (name, valueForm) <- (pairOf symbolExpr anyVal) args
  --defined ref should be in scope while evaluating the value form so
  --add undefined binding initially
  env <- getEnv
  (newEnv, ref) <- liftIO $ declare name Undefined env
  setEnv newEnv
  value <- (eval valueForm)
  --update declared binding with evaluated value
  liftIO $ setRef ref value
  --TODO: create Ref value and return that?
  pure value
  
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
                                 ("macro", (Special macroSpecial)),
                                 ("eval", (Special evalSpecial)),
                                 ("set!", (Special setSpecial)),
                                 ("begin", (Special beginSpecial)),
                                 ("define", (Special defineSpecial))]
  
exceptT :: Monad m => Either e a -> ExceptT e m a
exceptT (Left e) = throwE e
exceptT (Right v) = return v

eval :: Value -> Eval Value
eval expr = case expr of
  Number i -> pure expr
  Str s -> pure expr
  Symbol s -> do
    env <- getEnv
    var <- liftIO $ envLookup s env
    case var of
      Just v -> return v
      Nothing -> throwE (UnboundSymbol s)
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
      _ -> failEval $ FormError "Required function, macro or special form to evaluate" []
  Undefined -> failEval DerefUndefinedError
  _ -> pure expr

runEval :: Env Value -> Eval a -> IO (Either EvalError a)
runEval env e = evalStateT (runExceptT e) env

evalWithState :: Env Value -> Eval a -> IO (Env Value, Either EvalError a)
evalWithState env e = fmap swap $ runStateT (runExceptT e) env
