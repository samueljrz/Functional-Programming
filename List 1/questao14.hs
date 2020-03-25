corpo [] = []
corpo [x] = [x]
corpo (x:xs) = if xs /= []
	then [x] ++ corpo xs
