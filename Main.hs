module Main where

import System.Environment
import OSParser

main = do
    args <- getArgs
    case args of 
      [file] -> do
        x <- readFile file
        print (eval x)
      _ -> putStrLn "Wrong number of arguments"
