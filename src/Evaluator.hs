module Evaluator
(evaluate) where

import Types               (Sexp(..), Environment(..), RunTimeError(..), Eval(..), runEval)
import Environment         (get)
import Control.Monad.Writer.Lazy
import Control.Monad.Except


evaluate :: (Sexp, Environment) -> Eval
evaluate (command, env) = do tell env
                             result <- evalAst (command, env)
                             case result of
                                  MalList (MalFunction f : list) -> f list env
                                  _ -> return result

evalAst :: (Sexp, Environment) -> Eval
evalAst (MalSymbol symbol, env) = do tell env
                                     case get symbol env of
                                          Nothing -> throwError ("unbound variable " ++ symbol)
                                          Just val -> return $ MalFunction val

evalAst (MalList list, env) = foldr f (return $ MalList []) list
                              where f sexp accum =  do resultingSexp <- eval sexp
                                                       MalList accumulatedSexp <- accum
                                                       return $ MalList $ resultingSexp : accumulatedSexp
                                    eval sexp = do tell env
                                                   evaluate (sexp, env)
evalAst (sexp, env) = do tell env
                         return sexp
