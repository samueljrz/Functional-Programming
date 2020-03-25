concat' [] [] = []
concat' [] ys = ys
concat' xs [] = xs
concat' xs ys = xs ++ ys
