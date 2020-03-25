total :: [a] -> Int
total [] = 0
total xs = total(tail xs) + 1
