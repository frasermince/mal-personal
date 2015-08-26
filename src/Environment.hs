module Environment
( set
, get
, replEnv
) where
import Types
import Data.Maybe
import qualified Data.Map as Map

set :: String -> AppliedCommand -> Environment -> Environment
set k v env = Environment {getEnvironment = setInArray k v envList}
  where envList = getEnvironment env
        setInArray k v [] = [Map.fromList[(k, v)]]
        setInArray k v (env:envs) = newEnv:envs
          where newEnv = Map.insert k v env

get :: String -> Environment -> Maybe AppliedCommand
get k environment = getFromArray k envList
  where envList = getEnvironment environment
        getFromArray k [] = Nothing
        getFromArray k (env:envs)
          | isJust lookupValue = lookupValue
          | otherwise = getFromArray k envs
          where lookupValue = Map.lookup k env

applyAction :: (Integer -> Integer -> Integer) -> AppliedCommand
applyAction f [MalNum x, MalNum y] _ = return $ MalNum $ f x y

replEnv :: Environment
replEnv = Environment{getEnvironment = [operationMap]}
  where operationMap = Map.fromList [("+", applyAction (+)), ("-", applyAction (-)), ("*", applyAction (*)), ("/", applyAction div)]

