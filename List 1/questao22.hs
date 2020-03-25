ver x [] = 0
ver x (z:zs) = if x == z
	then 1 + ver x zs
	else ver x zs

intersec [] [] = []
intersec xs [] = []
intersec [] ys = []
intersec (x:xs) ys = if ver x ys > 0
		then [x] ++ intersec xs ys
		else intersec xs ys
