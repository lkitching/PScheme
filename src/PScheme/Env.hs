module PScheme.Env (
  EnvFrame,
  Env,
  newEnv,
  envLookup,
  pushEnv,
  popEnv) where

import qualified Data.Map.Strict as M
import Data.IORef

type EnvFrame = M.Map String
type Env a = [EnvFrame a]

newEnv :: Env a
newEnv = []

mapToFrame :: M.Map String a -> EnvFrame a
mapToFrame = id

envLookup :: String -> Env a -> Maybe a
envLookup _ [] = Nothing
envLookup k (e:es) = case (M.lookup k e) of
  m@(Just _) -> m
  Nothing -> envLookup k es

pushEnv :: EnvFrame a -> Env a -> Env a
pushEnv = (:)

popEnv :: Env a -> Env a
popEnv [] = []
popEnv (_:es) = es
