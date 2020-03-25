rev x = rev' (help x)

help 0 = []
help z = if z > 0
	then [mod z 10] ++ help (div z 10)
	else error"sss"
rev' [] = 0
rev' (x:xs) = x*(10^(length xs)) + rev' xs
