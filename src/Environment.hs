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

addLayer :: Environment -> Environment
addLayer env =  Map.empty : env

removeLayer :: Environment -> Environment
removeLayer (env : envs) = envs

set :: String -> Sexp -> Environment -> Environment
set k v env = setInArray k v env
  where setInArray k v [] = [Map.fromList[(k, v)]]
        setInArray k v (env:envs) = newEnv:envs
          where newEnv = Map.insert k v env

get :: String -> Environment -> Maybe Sexp
get k environment = getFromArray k environment
  where getFromArray k [] = Nothing
        getFromArray k (env:envs)
          | isJust lookupValue = lookupValue
          | otherwise = getFromArray k envs
          where lookupValue = Map.lookup k env



