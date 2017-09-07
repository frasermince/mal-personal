module Evaluator
(evaluate) where

import Types                        (Sexp(..), Environment(..), RunTimeError(..), Eval(..), runEval, MalError(..), runEvalForTuple, Command(..))
import qualified Environment as Env (get, set, addLayer, removeLayer)
import Control.Monad.Writer.Lazy
import Control.Monad.Except

evaluate :: (Sexp, Environment) -> Eval
evaluate (MalList (MalSymbol "let" : MalList params : expression : []), env) = finalResult newEnv
  where newEnv = runEvalForTuple $ findNewEnv params
        finalResult env = case env of
                    (Right (_, e))    -> censor Env.removeLayer $ evaluate (expression, e)
                    (Left error) -> throwError error
        findNewEnv :: [Sexp] -> Eval
        findNewEnv [] = censor (\x -> Env.addLayer env) (return $ MalList [])

        findNewEnv (_:[]) = do throwError $ MalEvalError "let param bindings do not match"
        findNewEnv (MalSymbol key : val : params) = do unwrappedVal <- solvedVal
                                                       censor (Env.set key unwrappedVal) evalledCurrentEnv
          where evalledCurrentEnv = findNewEnv params
                solvedVal =  do (_, currentEnv) <- listen evalledCurrentEnv
                                evaluate (val, currentEnv)
        --findNewEnv _ = do throwError $ MalEvalError "Wrong number of params to let binding"

evaluate (MalList (MalSymbol "def" : MalSymbol key : value : []), env)
  = do solvedValue <- evaluate (value, env)
       tell $ Env.set key solvedValue env
       return solvedValue

                                                                            --add case statements for error handling
evaluate (MalList (MalSymbol "fn" : MalList params : functionBody : []), env)
  = return $ MalFunction $ createFunction params functionBody
  where createFunction :: Command
        createFunction params body bindings environment = evaluate (body, functionEnvironment params bindings environment)
        functionEnvironment params bindings environment = foldl setToEnv environment $ zip params bindings
        setToEnv currentEnv (MalSymbol k, v) = Env.set k v currentEnv
                        -- setToEnv currentEnv (k, v) = 

evaluate (MalList (MalSymbol "do" : params), env) = foldl foldEval initialValue params
  where initialValue :: Eval
        initialValue = do tell env
                          return $ MalList []
        foldEval :: Eval -> Sexp -> Eval
        foldEval accumulator sexp = do (_, resultingEnv) <- listen accumulator
                                       evaluate (sexp, resultingEnv)

evaluate (MalList (MalSymbol "if" : condition : positive : negative : []), env) =
  do (sexp, newEnv) <- listen $ evaluate (condition, env)
     evaluate (chooseEvaluation sexp, newEnv)
  where chooseEvaluation (MalBool True)  = positive
        chooseEvaluation (MalBool False) = negative
        chooseEvaluation _               = positive


evaluate (command, env)
  = do tell env
       result <- evalAst (command, env)
       case result of
         MalList (MalFunction f : list) -> f list env
         MalList (f : list) -> throwError $ MalEvalError $ (show f) ++ " is not a function"
         _ -> return result

  where evalAst :: (Sexp, Environment) -> Eval
        evalAst (MalSymbol symbol, env) =
          do tell env
             case Env.get symbol env of
               Nothing -> throwError (MalEvalError $ "unbound variable " ++ symbol)
               Just val -> return $ val

        evalAst (MalList list, env) =  foldr f (return $ MalList []) list
          where f sexp accum =  do tell env
                                   resultingSexp <- evaluate (sexp, env)
                                   MalList accumulatedSexp <- accum
                                   return $ MalList $ resultingSexp : accumulatedSexp

        evalAst (sexp, env) = do tell env
                                 return sexp
