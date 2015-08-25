module Environment
( set
, get
, replEnv
) where
import Types
import Data.Maybe
import qualified Data.Map as Map

set :: String -> AppliedCommand -> Environment -> Environment 
set k v env = newEnvironment
  where currentEnvironment = current env
        resultMap = Map.insert k v currentEnvironment
        newEnvironment = Environment{outer = outer env, current=resultMap}


get :: String -> Environment -> Maybe AppliedCommand
get k env
  | isJust lookupValue = lookupValue
  | otherwise = do
      environmentResult <- outerEnvironment
      get k environmentResult
  where lookupValue = Map.lookup k $ current env
        outerEnvironment = outer env

applyAction :: (Integer -> Integer -> Integer) -> AppliedCommand
applyAction f [MalNum x, MalNum y] _ = MalNum $ f x y

replEnv :: Environment
replEnv = Environment{outer = Nothing, current = operationMap}
  where operationMap = Map.fromList [("+", applyAction (+)), ("-", applyAction (-)), ("*", applyAction (*)), ("/", applyAction div)]

