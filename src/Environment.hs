module Environment
( set
, get
, addLayer
, removeLayer
) where
import Types
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.Except

addLayer :: Monad m => Environment m -> Environment m
addLayer env =  Map.empty : env

removeLayer :: Monad m => Environment m -> Environment m
removeLayer (env : envs) = envs

set :: Monad m => String -> EnvironmentValue m -> Environment m -> Environment m
set k v env = setInArray k v env
  where setInArray k v [] = [Map.fromList[(k, v)]]
        setInArray k v (env:envs) = newEnv:envs
          where newEnv = Map.insert k v env

get :: Monad m => String -> Environment m -> Maybe (EnvironmentValue m)
get k environment = getFromArray k environment
  where getFromArray k [] = Nothing
        getFromArray k (env:envs)
          | isJust lookupValue = lookupValue
          | otherwise = getFromArray k envs
          where lookupValue = Map.lookup k env



