module Types
( Sexp(..)
, Environment(..)
, Command(..)
, AppliedCommand(..)
, Bindings(..)
, RunTimeError(..)
, Eval(..)
, runEval
) where
import qualified Data.Map as Map
import Control.Monad.Writer.Lazy
import Control.Monad.Except
import Control.Monad.Identity
type Params = [Sexp]
type Bindings = [Sexp]
type Body = Sexp
type RunTimeError = String

data Environment = Environment {getEnvironment :: [Map.Map String AppliedCommand]}
type Eval = WriterT Environment (ExceptT String Identity) Sexp
runEval :: Eval -> Either String Sexp
runEval eval = runIdentity $ runExceptT result
  where result = fst <$> runWriterT eval
type AppliedCommand = Bindings -> Environment -> Eval
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

instance Monoid Environment where
  mempty = Environment {getEnvironment = []}
  mappend x y = Environment {getEnvironment = resultList}
    where resultList = reverse $ mergeLists xList yList
          xList = reverse $ getEnvironment x
          yList = reverse $ getEnvironment y
          mergeLists x [] = x
          mergeLists [] y = y
          mergeLists (x:xs) (y:ys) = (x `Map.union` y) : mergeLists xs ys

