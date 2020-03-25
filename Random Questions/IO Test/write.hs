import System.IO  
import Data.Char  
  
main = do  
    --contents <- readFile "texto.txt"
    ws <- getLine
    writeFile "textoedit.txt" (ws)
