module Main where

import System.Environment
import OSParser

main = do
    args <- getArgs
    case args of 
      [tablefile, wordsfile] -> do
        table <- readFile tablefile
        words <- readFile wordsfile
        print (eval table)
        putChar '\n'
        print (eval words)
      _ -> putStrLn "Wrong number of arguments"
