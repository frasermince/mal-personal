module Main where

import Control.Monad
import System.IO
import ReadEval                 (readEval)
import Types                    (runEvalForTuple, Environment(..))
import Core                     (startingEnv)
import System.Console.Haskeline (runInputT, getInputLine, defaultSettings, InputT(..), outputStrLn)

main :: IO ()
main = runInputT defaultSettings $ loop startingEnv
   where
       loop :: Environment -> InputT IO ()
       loop env = do
           minput <- getInputLine "user> "
           case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just command -> case (readEval env command) of
                                 Right (s, e) -> do outputStrLn $ show s
                                                    loop e
                                 Left error -> do outputStrLn $ show error
                                                  loop env
