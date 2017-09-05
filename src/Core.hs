module Core
( makeMalFunction
, startingEnv
) where
import Types
import Environment as Env
import Evaluator
import Control.Monad.Except
import Control.Monad.Writer.Lazy (listen, tell)
import Debug.Trace
import qualified Data.Map as Map

makeError :: AppliedCommand
makeError [_, _] _ = do throwError (MalEvalError $ "Parameters are of the wrong type")
makeError list _ = do throwError (MalEvalError $ "Wrong number of parameters. Expected 2 parameters but got " ++ (show $ length list))

applyNumberAction :: (Int -> Int -> Int) -> AppliedCommand
applyNumberAction f [MalNum x, MalNum y] _ = return $ MalNum $ f x y
applyNumberAction _ list env = makeError list env

applyBooleanAction :: (Int -> Int -> Bool) -> AppliedCommand
applyBooleanAction f [MalNum x, MalNum y] _ = return $ MalBool $ f x y
applyBooleanAction _ list env = makeError list env

makeMalFunction :: (Int -> Int -> Int) -> Sexp
makeMalFunction f = MalFunction $ applyNumberAction f

makeMalBooleanFunction :: (Int -> Int -> Bool) -> Sexp
makeMalBooleanFunction f = MalFunction $ applyBooleanAction f


conditional :: AppliedCommand
conditional (condition : positive : negative : []) env = do (sexp, newEnv) <- listen $ evaluate (condition, env)
                                                            case sexp of
                                                              MalBool True -> evaluate (positive, newEnv)
                                                              MalBool False -> evaluate (negative, newEnv)
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

isList :: AppliedCommand
isList ((MalList x) : _) env = return $ MalBool True
isList (_ : _) env = return $ MalBool False

isEmpty :: AppliedCommand
isEmpty ((MalList []) : _) env = return $ MalBool True
isEmpty ((MalList _) : _) env = return $ MalBool False
isEmpty (list : _) env = throwError $ MalEvalError $ (show list) ++ " is not a list"

count :: AppliedCommand
count ((MalList list) : _) env = return $ MalNum $ length list
count (list : _) env = throwError $ MalEvalError $ (show list) ++ " is not a list"

equality :: AppliedCommand
equality (left : right : []) env = equals left right
  where equals :: Sexp -> Sexp -> Eval
        equals (MalNum one) (MalNum two) = return $ MalBool $ one == two
        equals (MalSymbol one) (MalSymbol two) = MalBool <$> ((==) <$> Evaluator.evaluate (MalSymbol one, env) <*> Evaluator.evaluate (MalSymbol two, env))
        equals (MalBool one) (MalBool two) = return $ MalBool $ one == two
        equals (MalList one) (MalList two) = return $ MalBool $ one == two
        equals (MalFunction one) (MalFunction two) = return $ MalBool False
        equals one two = throwError $ MalEvalError $ "cannot compare " ++ show one ++ " and " ++ show two


startingEnv :: Environment
startingEnv = [operationMap]
  where operationMap = Map.fromList [ ("+", makeMalFunction (+))
                                    , ("-", makeMalFunction (-))
                                    , ("*", makeMalFunction (*))
                                    , ("/", makeMalFunction div)
                                    , ("<", makeMalBooleanFunction (<))
                                    , ("<=", makeMalBooleanFunction (<=))
                                    , (">", makeMalBooleanFunction (>))
                                    , (">=", makeMalBooleanFunction (>=))
                                    , ("if", MalFunction conditional)
                                    , ("do", MalFunction doExpression)
                                    , ("list", MalFunction list)
                                    , ("list?", MalFunction isList)
                                    , ("empty?", MalFunction isEmpty)
                                    , ("count", MalFunction count)
                                    , ("=", MalFunction equality)
                                    --, ("prn", MalFunction)
                                    ]
