module PScheme.Env (
  EnvFrame,
  Env,
  newEnv,
  declare,
  newFrame,
  envLookup,
  pushEnv,
  popEnv,
  mapToFrame,
  envOf,
  findBinding,
  addFrameBinding,
  setRef,
  top) where

import qualified Data.Map.Strict as M
import Data.IORef

type Ref = IORef
type EnvFrame a = M.Map String (Ref a)
type Env a = [EnvFrame a]

newEnv :: Env a
newEnv = []

envOf :: (M.Map String a) -> IO (Env a)
envOf m = fmap (: []) (mapToFrame m)

newFrame :: EnvFrame a
newFrame = M.empty

addFrameBinding :: String -> a -> EnvFrame a -> IO (EnvFrame a)
addFrameBinding name initValue frame = do
  ref <- newIORef initValue
  pure $ M.insert name ref frame

declare :: String -> a -> Env a -> IO (Env a, Ref a)
declare name initValue [] = do
  ref <- newIORef initValue
  pure ([M.singleton name ref], ref)
declare name initValue (frame:fs) = do
  ref <- newIORef initValue
  let env' = (M.insert name ref frame):fs
  pure (env', ref)
  
mapToFrame :: M.Map String a -> IO (EnvFrame a)
mapToFrame = traverse newIORef

findBinding :: String -> Env a -> Maybe (Ref a)
findBinding _ [] = Nothing
findBinding k (e:es) = case (M.lookup k e) of
  r@(Just _) -> r
  Nothing -> findBinding k es

setRef :: Ref a -> a -> IO ()
setRef = writeIORef  
  
envLookup :: String -> Env a -> IO (Maybe a)
envLookup k env = case (findBinding k env) of
  Nothing -> pure Nothing
  (Just ref) -> do
    v <- readIORef ref
    return (Just v)

pushEnv :: EnvFrame a -> Env a -> Env a
pushEnv = (:)

popEnv :: Env a -> Env a
popEnv [] = []
popEnv (_:es) = es

top :: Env a -> Env a
top [] = []
top e@[_] = e
top (f:fs) = top fs
