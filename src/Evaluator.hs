module Evaluator
(evaluate) where

import Types               (Sexp(..), Environment(..))

type RunTimeError = String

evaluate :: Sexp -> Environment -> Either RunTimeError Sexp
evaluate command env = Right command


