module Core
( applyAction
, makeMalFunction
, replEnv
) where
import Types
import Environment as Env
import Evaluator
import Control.Monad.Except
import Control.Monad.Writer.Lazy
import Debug.Trace
import qualified Data.Map as Map

applyAction :: (Integer -> Integer -> Integer) -> AppliedCommand
applyAction f [MalNum x, MalNum y] _ = return $ MalNum $ f x y
applyAction f [_, _] _ = do throwError (MalEvalError $ "Parameters are of the wrong type")
applyAction f list _ = do throwError (MalEvalError $ "Wrong number of parameters. Expected 2 parameters but got " ++ (show $ length list))

makeMalFunction :: (Integer -> Integer -> Integer) -> Sexp
makeMalFunction f = MalFunction $ applyAction f


conditional :: AppliedCommand
conditional (condition : positive : negative : []) env = do (sexp, newEnv) <- listen $ evaluate (condition, env)
                                                            case sexp of
                                                              MalBool "true" -> evaluate (positive, newEnv)
                                                              MalBool "false" -> evaluate (negative, newEnv)
                                                              _ -> evaluate (positive, newEnv)

doExpression :: AppliedCommand
doExpression params env = foldl foldEval initialValue params
  where initialValue :: Eval
        initialValue = do tell env
                          return $ MalList []
        foldEval :: Eval -> Sexp -> Eval
        foldEval accumulator sexp = do (_, resultingEnv) <- listen accumulator
                                       evaluate (sexp, resultingEnv)


list :: AppliedCommand
list params env = return $ MalList params

replEnv :: Environment
replEnv = Environment{getEnvironment = [operationMap]}
  where operationMap = Map.fromList [ ("+", makeMalFunction (+))
                                    , ("-", makeMalFunction (-))
                                    , ("*", makeMalFunction (*))
                                    , ("/", makeMalFunction div)
                                    , ("if", MalFunction conditional)
                                    , ("do", MalFunction doExpression)
                                    , ("list", MalFunction list)
                                    ]
