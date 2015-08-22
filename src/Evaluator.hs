module Evaluator
(evaluate) where

import qualified Data.Map as Map
import Types               (Sexp(..))


add :: Sexp -> Sexp -> Sexp
add (MalNum x) (MalNum y) = MalNum (x + y)

subtract :: Sexp -> Sexp -> Sexp
subtract (MalNum x) (MalNum y) = MalNum (x - y)

multiply :: Sexp -> Sexp -> Sexp
multiply (MalNum x) (MalNum y) = MalNum (x * y)

divide :: Sexp -> Sexp -> Sexp
divide (MalNum x) (MalNum y) = MalNum (x `div` y)

--replEnv :: [Map.Map String Sexp]
replEnv = [Map.fromList [("+", add), ("-", Evaluator.subtract), ("*", multiply), ("/", divide)]]

type RunTimeError = String

evaluate :: Sexp -> Environment -> Either RunTimeError Sexp
evaluate = Right


