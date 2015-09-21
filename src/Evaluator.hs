module Evaluator
(evaluate) where

import Types                        (Sexp(..), Environment(..), RunTimeError(..), Eval(..), runEval, MalError(..), runEvalForTuple)
import qualified Environment as Env (get, set)
import Control.Monad.Writer.Lazy
import Control.Monad.Except


evaluate :: (Sexp, Environment) -> Eval
evaluate (MalList (MalSymbol "let*" : MalList params : expression : []), env) = do (_, newEnv) <- listen $ findNewEnv params
                                                                                   evaluate (expression, newEnv)
  where findNewEnv :: [Sexp] -> Eval
        --need to make add and remove layer commands for env
        findNewEnv [] = censor (\x -> env) (return $ MalList [])

        findNewEnv (_:[]) = do throwError $ MalEvalError "let* param bindings do not match"
        findNewEnv (MalSymbol key : val : params) = do unwrappedVal <- solvedVal
                                                       censor (Env.set key unwrappedVal) evalledCurrentEnv
          where evalledCurrentEnv = findNewEnv params
                solvedVal =  do (_, currentEnv) <- listen evalledCurrentEnv
                                evalAst (val, currentEnv)
        --findNewEnv _ = do throwError $ MalEvalError "Wrong number of params to let binding"

evaluate (MalList (MalSymbol "def!" : MalSymbol key : value : []), env) = do solvedValue <- evalAst (value, env)
                                                                             tell $ Env.set key solvedValue env
                                                                             return solvedValue
evaluate (command, env) = do tell env
                             result <- evalAst (command, env)
                             case result of
                                  MalList (MalFunction f : list) -> f list env
                                  _ -> return result

evalAst :: (Sexp, Environment) -> Eval
evalAst (MalSymbol symbol, env) = do tell env
                                     case Env.get symbol env of
                                          Nothing -> throwError (MalEvalError $ "unbound variable " ++ symbol)
                                          Just val -> return $ val

evalAst (MalList list, env) = foldr f (return $ MalList []) list
                              where f sexp accum =  do resultingSexp <- eval sexp
                                                       MalList accumulatedSexp <- accum
                                                       return $ MalList $ resultingSexp : accumulatedSexp
                                    eval sexp = do tell env
                                                   evaluate (sexp, env)
evalAst (sexp, env) = do tell env
                         return sexp
