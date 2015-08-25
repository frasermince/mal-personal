module Evaluator
(evaluate) where

import Types               (Sexp(..), Environment(..), RunTimeError(..))


evaluate :: Sexp -> Environment -> Either RunTimeError Sexp
evaluate command env = Right command


