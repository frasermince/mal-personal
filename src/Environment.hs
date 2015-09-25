module Environment
( set
, get
, replEnv
, addLayer
, removeLayer
) where
import Types
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.Except

addLayer :: Environment -> Environment
addLayer Environment { getEnvironment = (env) } = Environment { getEnvironment = Map.empty : env }

removeLayer :: Environment -> Environment
removeLayer Environment { getEnvironment = (env : envs) } =  (Environment { getEnvironment = envs })

set :: String -> Sexp -> Environment -> Environment
set k v env = (Environment {getEnvironment = newEnv})
  where newEnv = setInArray k v envList
        envList = getEnvironment env
        setInArray k v [] = [Map.fromList[(k, v)]]
        setInArray k v (env:envs) = newEnv:envs
          where newEnv = Map.insert k v env

get :: String -> Environment -> Maybe Sexp
get k environment = getFromArray k envList
  where envList = getEnvironment environment
        getFromArray k [] = Nothing
        getFromArray k (env:envs)
          | isJust lookupValue = lookupValue
          | otherwise = getFromArray k envs
          where lookupValue = Map.lookup k env

applyAction :: (Integer -> Integer -> Integer) -> AppliedCommand
applyAction f [MalNum x, MalNum y] _ = return $ MalNum $ f x y
applyAction f [_, _] _ = do throwError (MalEvalError $ "Parameters are of the wrong type")
applyAction f list _ = do throwError (MalEvalError $ "Wrong number of parameters. Expected 2 parameters but got " ++ (show $ length list))

makeMalFunction :: (Integer -> Integer -> Integer) -> Sexp
makeMalFunction f = MalFunction $ applyAction f


replEnv :: Environment
replEnv = Environment{getEnvironment = [operationMap]}
  where operationMap = Map.fromList [("+", makeMalFunction (+)), ("-", makeMalFunction (-)), ("*", makeMalFunction (*)), ("/", makeMalFunction div)]

