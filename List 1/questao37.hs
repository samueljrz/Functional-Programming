splitints xs = (lado1 xs, lado2 xs)

lado1 [] = []
lado1 [x] = [x]
lado1 (x:y:xs) = [x] ++ lado1 xs

lado2 [] = []
lado2 [x] = []
lado2 (z:w:[]) = [w]
lado2 (z:w:zs) = [w] ++ lado2 zs
