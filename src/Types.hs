module Types
( Sexp(..)
, Environment(..)
, Command(..)
, AppliedCommand(..)
, Bindings(..)
, RunTimeError(..)
) where
import qualified Data.Map as Map
type Params = [Sexp]
type Bindings = [Sexp]
type Body = Sexp
type RunTimeError = String

data Environment = Environment { outer :: Maybe Environment, current :: Map.Map String AppliedCommand }
type AppliedCommand = Bindings -> Environment -> Sexp
type Command = Params -> Body -> AppliedCommand

data Sexp = MalNum Integer | MalSymbol String | MalList [Sexp] | MalError String
            deriving (Eq)

instance Show Sexp where
  show (MalNum x) = show x
  show (MalSymbol x) = x
  show (MalList sexps) = "(" ++ foldl convertToString "" sexps ++ ")"
    where
      convertToString "" sexp = show sexp
      convertToString accumulator sexp = accumulator ++ " " ++ show sexp

