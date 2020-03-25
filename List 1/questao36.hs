compac [] = []
compac (x:xs) = tst x (ver x xs):(compac (drop (ver x xs) xs)) 

ver a [] = 0
ver a (z:zs) = if a == z 
	then 1 + ver a zs 
	else 0

tst a 0 = [a]
tst a c = [c+1] ++ [a]	

