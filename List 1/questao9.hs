frequencia y [] = 0
frequencia y [x] = if x == y then 1 else 0
frequencia y (x:xs) = if y == x
		then 1 + frequencia y xs
		else 0 + frequencia y xs
