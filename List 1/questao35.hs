--  quest 2

-- funcoes de apio

--COMPLETE AQUI

-- funcao de ordenacao
--delement :: [a] -> [a]

delement z [] = []
delement z xs = (takeWhile (/=z) xs) ++ (tail $ dropWhile (/=z) xs)

--sort :: [Int] -> [Int]

sort [] = []
sort [x] = [x]
sort xs = sort def ++ [maximum xs]
	where def = delement (maximum xs) xs

 
