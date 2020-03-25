cont y [] = 0
cont y [x] = if x == y then 1 else 0
cont y (x:xs) = if y == x 
	then 1 + cont y xs
	else 0 + cont y xs

unico y xs = if (cont y xs) /= 1
	then False
	else True
