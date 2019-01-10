module OSWriter where

import OSSolver


printGrid [] = return ()
printGrid (g:grid) = do putStrLn ( addSpaces g)
                        printGrid grid
                          
addSpaces [] = []
addSpaces [x] = [x]
addSpaces (x:xs) = x:' ':addSpaces xs

