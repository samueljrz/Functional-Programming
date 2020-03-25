ver x [] = 0
ver x (z:zs) = if x == z
        then 1 + ver x zs
        else ver x zs

uniao [] [] = []
	uniao xs [] = xs
uniao [] ys = ys
uniao (x:xs) ys = if ver x ys == 0
        then [x] ++ uniao xs ys
        else uniao xs ys
