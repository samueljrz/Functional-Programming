selec' xs z = xs !! z

selec xs [] = []
selec xs (y:ys) = [selec' xs y] ++ selec xs ys

