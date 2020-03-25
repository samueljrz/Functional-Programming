aux xs = maximum xs

maior [] = 0
maior [x] = 0
maior (x:xs) 
	| (x == aux xs) = 0
	| otherwise = 1 + maior xs
