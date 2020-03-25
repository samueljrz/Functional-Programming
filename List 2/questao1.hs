pair [] = 0
pair (x:xs) = if x == True
	then 1 + pair xs
	else pair xs

paridade [] = False
paridade xs = if (mod (pair xs) 2) /= 0
	then True
	else False
