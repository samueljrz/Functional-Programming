pertence y [] = False
pertence y (x:xs) = if x == y 
		then True 
		else pertence y xs
