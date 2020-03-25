find ch [] = ch
find ch (x:xs)
	|ch == fst x = snd x
	|otherwise = find ch xs

upper "" = ""
upper (ch:s) = (find ch list) : upper s
	where list = zip ['a'..'z'] ['A'..'Z']
