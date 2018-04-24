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
, EnvironmentValue(..)
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

type Environment m = [Map.Map String (EnvironmentValue m)]
type Eval m = WriterT (Environment m) (ExceptT MalError m) Sexp

runEvalForTuple :: Eval IO -> IO (Either MalError (Sexp, Environment IO))
runEvalForTuple eval = runExceptT $ runWriterT eval

type AppliedCommand m = Bindings -> Environment m -> Eval m
type Command m = Params -> Body -> AppliedCommand m

data EnvironmentValue m = Value Sexp | Function (AppliedCommand m)

data Sexp = MalNum Int | MalSymbol String | MalList [Sexp] | MalBool Bool | MalString String
            deriving (Eq)

data MalError = MalParseError ParseError | MalEvalError String

instance Show Sexp where
  show (MalNum x) = show x
  show (MalSymbol x) = x
  show (MalBool x) = show x
  show (MalString string) = "\"" ++ string ++ "\""
  -- show (MalFunction appliedCommand) = "<FN>"
  show (MalList sexps) = "(" ++ foldl convertToString "" sexps ++ ")"
    where
      convertToString "" sexp = show sexp
      convertToString accumulator sexp = accumulator ++ " " ++ show sexp

instance {-# OVERLAPS #-} Monoid (Environment m) where
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

-- instance Eq AppliedCommand where
--   commandOne == commandTwo = runEval (commandOne bindings mempty ) == runEval (commandTwo bindings mempty)
--     where bindings = [MalNum 1, MalNum 2]

--instance Show Eval where
  --show eval = show $ runEval eval
