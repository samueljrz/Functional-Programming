isSorted' [] = 0
isSorted' [x] = 0
isSorted' (x:y:xs) = if x <= y
	then 0 + isSorted' ([y] ++ xs)
	else 1 + isSorted' ([y] ++ xs)

isSorted [] = True
isSorted [x] = True
isSorted zs = if isSorted' zs > 0
	then False
	else True
