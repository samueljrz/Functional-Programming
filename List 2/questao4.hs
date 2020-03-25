swap ws x y = take x ws ++ [ws !! y] ++ aux2 (aux ws (ws !! x)) (ws !! y) ++ [ws !! x] ++ aux ws (ws !! y)

aux [] x = []
aux (z:zs) x = if z == x
	then zs
	else aux zs x

aux2 (z:zs) x = if z /= x
	then [z] ++ aux2 zs x
	else []
	
