module Main where

import Control.Monad
import System.IO
import Rep
import Types(runEvalForTuple, Environment(..))
import Environment(replEnv)
import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings $ loop replEnv
   where
       loop :: Environment -> InputT IO ()
       loop env = do
           minput <- getInputLine "user> "
           case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just command -> case (rep env command) of
                                 Right (s, e) -> do outputStrLn $ show s
                                                    loop e
                                 Left error -> do outputStrLn $ show error
                                                  loop env
