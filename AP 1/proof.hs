-- NOME: SAMUEL EVANGELISTA DE AQUINO JUNIOR
-- CURSO: ENGENHARIA DE COMPUTAÇÃO
-- MATRICULA: 397618

----------
-- quest 1
----------
-- *
--lsDig :: Integral t => t -> [t]
lsDig 0 = []
lsDig x 
    | (mod x 10) >= 0 = [mod x 10] ++ lsDig (div x 10)
-- ok	
--isPanDig :: Integral t => t -> Bool
isPanDig 0 = True
isPanDig x = auxisPanDig (lsDig x)

auxisPanDig [x] = True 
auxisPanDig (x:xs)
    | elem x xs = False
    | otherwise = auxisPanDig xs
-- *
--lsPanDig :: (Eq a, Integral t, Num a) => a -> [t]
lsPanDig x = take x (aux2 (aux)) 

aux = [z | z <- [0..]]

aux2 [] = []
aux2 (x:xs)
    | isPanDig x = [x] ++ aux2 xs
    | otherwise = aux2 xs  

----------
-- quest 2
----------
-- ok
--rmFirst :: Eq a => [a] -> a -> [a]
rmFirst [] z = []
rmFirst (x:xs) z
    | x == z = xs
    | otherwise = [x] ++ rmFirst xs z

minMaxSort :: Ord t => [t] -> [t]
minMaxSort [] = []
minMaxSort [x] = [x]
minMaxSort xs = [minimum xs] ++ minMaxSort rs ++ [maximum xs]
	where rs = delement (delement xs (minimum xs)) (maximum xs)

delement xs z = takeWhile (/=z) xs ++ tail(dropWhile (/=z) xs)

----------
-- quest 3
----------
-- errado
--swap :: [a] -> Int -> Int -> [a]
swap x y ws
     | x < y = take x ws ++ [ws !! y] ++ aux3 (aux1 ws (ws !! x)) (ws !! y) ++ [ws !! x] ++ aux1 ws (ws !! y)
     |otherwise = take y ws ++ [ws !! x] ++ aux3 (aux1 ws (ws !! y)) (ws !! x) ++ [ws !! y] ++ aux1 ws (ws !! x) 

aux1 [] x = []
aux1 (z:zs) x = if z == x
	then zs
	else aux1 zs x

aux3 (z:zs) x = if z /= x
	then [z] ++ aux3 zs x
	else []

nextPerm :: Ord a => [a] -> [a]
nextPerm     _ = []

----------
-- quest 4
----------

--rmChar :: Eq t => [t] -> t -> [t]
rmChar [] x = []
rmChar (c:ch) x
     | x == c = rmChar ch x
     | otherwise = [c] ++ rmChar ch x 

--unique :: Eq t => [t] -> [t]
unique [] = []
unique (c:ch) 
     | elem c ch = unique ch
     | otherwise = c:(unique ch) 
-- * 
--freqChar :: (Eq a, Num b) => [a] -> [(a, b)]
freqChar [] = []
freqChar (z:zs) = [(z, (aux4 z zs))] ++ freqChar (rmChar zs z) 

aux4 w [] = 1
aux4 w (x:xs) 
     | w == x = 1 + aux4 w xs
     | otherwise = aux4 w xs
