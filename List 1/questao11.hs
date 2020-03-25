maioresQue z [] = []
maioresQue z [x] = if x > z then [x] else []
maioresQue z (x:xs) = if x > z 
		then [x] ++ maioresQue z xs
		else maioresQue z xs
