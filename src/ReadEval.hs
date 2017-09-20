module ReadEval (readEval) where
import Parser (readLang)
import Types (Sexp(..), Environment(..), Eval(..), runEvalForTuple, MalError(..))
import Evaluator (evaluate)

readEval :: Environment -> String -> Either MalError (Sexp, Environment)
readEval env command = do sexp <- readLang command
                          runEvalForTuple $ evaluate (sexp, env)
