{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Types
( Sexp(..)
, Environment(..)
, Command(..)
, AppliedCommand(..)
, Bindings(..)
, RunTimeError(..)
, Eval(..)
, MalError(..)
, runEval
, runEvalForTuple
) where
import qualified Data.Map as Map
import Control.Monad.Writer.Lazy
import Control.Monad.Except
import Control.Monad.Identity
import Text.Parsec
type Params = [Sexp]
type Bindings = [Sexp]
type Body = Sexp
type RunTimeError = String

type Environment = [Map.Map String Sexp]
type Eval = WriterT Environment (ExceptT MalError Identity) Sexp
runEval :: Eval -> Either MalError Sexp
runEval eval = runIdentity $ runExceptT result
  where result = fst <$> runWriterT eval

runEvalForTuple :: Eval -> Either MalError (Sexp, Environment)
runEvalForTuple eval = runIdentity $ runExceptT $ runWriterT eval

type AppliedCommand = Bindings -> Environment -> Eval
type Command = Params -> Body -> AppliedCommand

data Sexp = MalNum Int | MalSymbol String | MalList [Sexp] | MalFunction AppliedCommand | MalBool Bool
            deriving (Eq)

data MalError = MalParseError ParseError | MalEvalError String

instance Show Sexp where
  show (MalNum x) = show x
  show (MalSymbol x) = x
  show (MalBool x) = show x
  show (MalFunction appliedCommand) = "<FN>"
  show (MalList sexps) = "(" ++ foldl convertToString "" sexps ++ ")"
    where
      convertToString "" sexp = show sexp
      convertToString accumulator sexp = accumulator ++ " " ++ show sexp

instance {-# OVERLAPS #-} Monoid Environment where
  mempty = []
  mappend x y = reverse $ mergeLists xList yList
    where xList = reverse x
          yList = reverse y
          mergeLists x [] = x
          mergeLists [] y = y
          mergeLists (x:xs) (y:ys) = (y `Map.union` x) : mergeLists xs ys

instance Eq MalError where
  MalParseError one == MalParseError two = one == two
  MalEvalError one == MalEvalError two = one == two

instance Show MalError where
  show (MalParseError error) = show error
  show (MalEvalError error) = show error

instance Eq AppliedCommand where
  commandOne == commandTwo = runEval (commandOne bindings mempty ) == runEval (commandTwo bindings mempty)
    where bindings = [MalNum 1, MalNum 2]

--instance Show Eval where
  --show eval = show $ runEval eval
