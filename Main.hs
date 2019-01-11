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
        
        let solvedBoard = solve (eval table) (eval words)
        
        putStrLn "Original board: "
        printGrid (eval table)
        putStrLn "\nSolved board: "
        printGrid (split solvedBoard (length(head(eval table))))
        putStr "\nHidden word: " 
        putStrLn (getHiddenWord solvedBoard)
        
      _ -> putStrLn "Wrong number of arguments"
