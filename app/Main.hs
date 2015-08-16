module Main where

import Control.Monad
import System.IO
import Rep

main :: IO ()
main = forever prompt

prompt :: IO ()
prompt = do
  putStr "user> "
  hFlush stdout
  command <- getLine
  putStrLn $ rep command

