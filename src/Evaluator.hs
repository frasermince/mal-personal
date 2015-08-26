module Evaluator
(evaluate) where

import Types               (Sexp(..), Environment(..), RunTimeError(..), Eval(..))


evaluate :: Sexp -> Environment -> Eval
evaluate command env = return command


