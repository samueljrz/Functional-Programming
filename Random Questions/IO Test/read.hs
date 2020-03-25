import System.IO  
  
main = do  
    contents <- readFile "texto.txt"  
    putStr contents  

