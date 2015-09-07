module Main where

import Control.Monad
import System.IO
import Rep
import Environment(replEnv)
import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine "user> "
           case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just command -> do outputStrLn $ either show show $ rep replEnv command
                                  loop
