{-# LANGUAGE OverloadedStrings #-}
module Rep
( Rep.rep
--, Rep.print
) where
import Parser
import Text.Parsec
import Types (Sexp(..), Environment(..), Eval(..), runEval, MalError(..))
import Evaluator
import Data.String

rep :: Environment -> String -> Either MalError Sexp
rep env command = do sexp <- Parser.read command
                     runEval $ evaluate (sexp, env)

--print :: Eval -> String
--print = (either show show) . runEval
