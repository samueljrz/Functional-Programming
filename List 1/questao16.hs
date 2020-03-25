menores' z [] = []
menores' z xs = take z (sort xs) 

delement z [] = []
delement z xs = (takeWhile (/=z) xs) ++ (tail $ dropWhile (/=z) xs)

sort [] = []
sort [x] = [x]
sort xs = sort def ++ [maximum xs]
	where def = delement (maximum xs) xs

ver z [] = 0
ver z (x:xs) = if z == x
	then 1 + ver z xs
	else ver z xs

menores z [] = []
menores 0 xs = []
menores z xs = if ver (head xs) (menores' z xs) /= 0
	then [head xs] ++ menores (z-1) (tail xs)
	else menores z ((tail xs) ++ [head xs]) 


--fazer uma lista de menores
--verificar cada elemento da lista, se o mesmo pertence a lista de menores

