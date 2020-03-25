import System.Environment

rmWord [] x = []
rmWord (c:ch) xs
     | xs == c = rmWord ch xs
     | otherwise = [c] ++ rmWord ch xs

cont z [] = 1
cont z (w:ws)
     | z == w = 1 + cont z ws
     | otherwise = cont z ws

freqWord :: [[Char]] -> [([Char], Int)]
freqWord [] = []
freqWord (x:xs) = [(x, (cont x xs))] ++ freqWord (rmWord xs x)

letB [] = []
letB (x:xs)
	| length (fst x) > 3 = [x] ++ letB xs
	| otherwise = letB xs

ordB [] = []
ordB xs = [maximum xs] ++ ordB (delete' (maximum xs) xs)

delete' z xs = (takeWhile (/=z) xs) ++ (tail $ dropWhile (/=z) xs)

main = do 
   args <- getArgs       
   text <- readFile (args !! 0)
   let w = words text
   putStrLn "Todas palavras separdas por palavra e frequencia"
   putStrLn $ show (freqWord w)
   putStrLn "Palavras maiores que 3 letras em ordem alfabetica"
   putStrLn $ show (reverse(ordB(letB(freqWord w))))
