rep y [] = 0
rep y (x:xs) = if y == x
	then 1 + rep y xs
	else rep y xs

unique' [] = []
unique' [x] = [x]
unique' (z:zs) = if rep z zs == 0
	then [z] ++ unique zs
	else unique zs

delement z [] = []
delement z xs = (takeWhile (/=z) xs) ++ (tail $ dropWhile (/=z) xs)

sort [] = []
sort [x] = [x]
sort xs = sort def ++ [maximum xs]
	where def = delement (maximum xs) xs

unique ms = sort (unique' ms)
