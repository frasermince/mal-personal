{-# LANGUAGE OverloadedStrings #-}
module Rep
( Rep.rep
--, Rep.print
) where
import Parser (readLang)
import Text.Parsec
import Types (Sexp(..), Environment(..), Eval(..), runEvalForTuple, MalError(..))
import Evaluator
import Data.String

rep :: Environment -> String -> Either MalError (Sexp, Environment)
rep env command = do sexp <- readLang command
                     runEvalForTuple $ evaluate (sexp, env)
