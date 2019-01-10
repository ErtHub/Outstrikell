module OSWriter where

import OSSolver


printGrid [] = return ()
printGrid (g:grid) = do putStrLn ( addSpaces g)
                        printGrid grid
                        
split [] len = []
split str len = (take len str):split (drop len str) len

                          
addSpaces [] = []
addSpaces [x] = [x]
addSpaces (x:xs) = x:' ':addSpaces xs

