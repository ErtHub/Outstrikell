module Main where

import System.Environment
import OSParser
import OSSolver
import OSWriter

main = do
    args <- getArgs
    case args of 
      [tablefile, wordsfile] -> do
        table <- readFile tablefile
        words <- readFile wordsfile
    
        printGrid (eval table)
        
        putStr "\nHidden word: " 
        putStrLn (solve (eval table) (eval words))
        
      _ -> putStrLn "Wrong number of arguments"
