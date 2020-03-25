list x = [x | x <- [1..x]] 

divide 1 = 1
divide n = if mod n ((list n) !! n-1) == 0
	then 1 + divide (n-1)
	else divide (n-1) 

primo 1 = False
primo n = if divide n == 2
	then True
	else False
