inserir z [] = [z]
inserir z (x:xs) = if z > x
	then [x] ++ inserir z xs
	else [z] ++ [x] ++ xs 
