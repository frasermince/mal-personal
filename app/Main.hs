module Main where

import Control.Monad
import System.IO
import Rep
import Environment(replEnv)

main :: IO ()
main = forever prompt

prompt :: IO ()
prompt = do
  putStr "user> "
  hFlush stdout
  command <- getLine
  putStrLn $ either show show $ rep replEnv command

