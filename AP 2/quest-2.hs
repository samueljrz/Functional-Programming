----------------------------------
-- PAETE 2: PROCESAMENTO DE TEXTO
----------------------------------
--(6.0)

import System.Environment 

-- (0.5)
-- retorna substring de str que inicia
-- na posicao i e possui m caracteres
sub xs y z = (aux2 (aux1 xs y) z)

aux1 xs y = drop y xs

aux2 xs z = take z xs



-- (1.0)
-- procura na string s a string 
-- t a partir da posição j.
-- Retorna o indice onde t inicia
-- e -1 se t não pertence a s

--find :: String -> Sring -> Int -> Int
find sx tx j 
   | (aux4 tx (words(aux3 sx j))) < 0 = (aux4 tx (words(aux3 sx j)))
   | otherwise = (aux4 tx (words(aux3 sx j))) + (length(take j sx))

aux3 xs j = drop j xs

aux4 tx sx 
   | elem tx sx = (length(unwords(takeWhile (/= tx) sx)))+1
   | otherwise = 1*(-1)


-- (1.0)
-- substitui em s primeira aparicao
-- de w em s por t (a partir da posicao 
-- j caso seja possivel. Do dontearui 
-- retorna o proptia s

--replaceOne :: String -> String -> String -> Int -> String
replaceOne sx wx tx j
   | take (length sx) ((take j sx) ++ aux7 (words(aux6 sx j)) wx tx) == sx = sx
   | otherwise = ((take j sx) ++ aux7 (words(aux6 sx j)) wx tx)
 
aux6 sx j = drop j sx

aux7 sx w t 
   | elem w sx = aux5 sx w t
   | otherwise = unwords sx 

aux5 [] w t = []
aux5 (s:sx) w t
   | s == w = t ++ " " ++ (unwords sx)
   | otherwise = s ++ " " ++ aux5 sx w t


-- (2.0)
-- substitui en s todas as 
-- aparicoes de w por t, 
-- a partir da posicao j

--replace :: String -> String -> String -> Int -> String 
replace sx wx tx j 
   | take (length sx) ((take j sx) ++ aux10 (words(aux9 sx j)) wx tx) == sx = sx
   | otherwise = ((take j sx) ++ aux10 (words(aux9 sx j)) wx tx)
 
aux9 sx j = drop j sx

aux10 sx w t 
   | elem w sx = aux8 sx w t
   | otherwise = unwords sx 

aux8 [] w t = []
aux8 (s:sx) w t
   | s == w = t ++ " " ++ aux8 sx w t
   | otherwise = s ++ " " ++ aux8 sx w t

main = do
     putStrLn "ok"
     args <- getArgs
     text <- readFile (args !! 0)
     writeFile (args !! 3) (replace text (args !! 1) (args !! 2) 0) 
     









