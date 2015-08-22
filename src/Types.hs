module Types
( Sexp(..)
) where

data Environment = Environment { outer :: Maybe Environment, current :: Map.Map k v }

set :: k -> v -> Environment -> Environment
set k v env = insert k v $ current Environment

get :: k -> Environment -> Maybe v
get k env
  | isJust lookupValue = lookupValue
  | isJust outerEnvironment = get k outerEnvironment
  | otherwise = Nothing
  where lookupValue = lookup k $ current env
        outerEnvironment = outer env

data Sexp = MalNum Integer | MalSymbol String | MalList [Sexp]
            deriving (Eq)

instance Show Sexp where
  show (MalNum x) = show x
  show (MalSymbol x) = x
  show (MalList sexps) = "(" ++ foldl convertToString "" sexps ++ ")"
    where
      convertToString "" sexp = show sexp
      convertToString accumulator sexp = accumulator ++ " " ++ show sexp

