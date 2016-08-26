module PScheme.Env (
  EnvFrame,
  Env,
  newEnv,
  envLookup,
  pushEnv,
  popEnv,
  mapToFrame,
  envOf,
  setBinding,
  top) where

import qualified Data.Map.Strict as M
import Data.IORef

type EnvFrame a = M.Map String (IORef a)
type Env a = [EnvFrame a]

newEnv :: Env a
newEnv = []

envOf :: (M.Map String a) -> IO (Env a)
envOf m = fmap (: []) (mapToFrame m)

mapToFrame :: M.Map String a -> IO (EnvFrame a)
mapToFrame = traverse newIORef

findBinding :: String -> Env a -> Maybe (IORef a)
findBinding _ [] = Nothing
findBinding k (e:es) = case (M.lookup k e) of
  r@(Just _) -> r
  Nothing -> findBinding k es
  
envLookup :: String -> Env a -> IO (Maybe a)
envLookup k env = case (findBinding k env) of
  Nothing -> pure Nothing
  (Just ref) -> do
    v <- readIORef ref
    return (Just v)
  
setBinding :: Env a -> (String, a) -> IO ()
setBinding env (name, value) = case (findBinding name env) of
    Nothing -> return ()
    Just ref -> writeIORef ref value

pushEnv :: EnvFrame a -> Env a -> Env a
pushEnv = (:)

popEnv :: Env a -> Env a
popEnv [] = []
popEnv (_:es) = es

top :: Env a -> Env a
top [] = []
top e@[_] = e
top (f:fs) = top fs
