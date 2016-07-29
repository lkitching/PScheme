module PScheme.Env (
  EnvFrame,
  Env,
  newEnv,
  envLookup,
  pushEnv,
  popEnv,
  mapToFrame,
  envOf) where

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

envLookup :: String -> Env a -> IO (Maybe a)
envLookup _ [] = pure Nothing
envLookup k (e:es) = case (M.lookup k e) of
  (Just ref) -> fmap Just (readIORef ref)
  Nothing -> envLookup k es

pushEnv :: EnvFrame a -> Env a -> Env a
pushEnv = (:)

popEnv :: Env a -> Env a
popEnv [] = []
popEnv (_:es) = es
