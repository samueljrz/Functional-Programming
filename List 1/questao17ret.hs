--alter' :: [Int] -> [a]
alter' 0 = []
alter' n = [(n*(-1))] ++ [n] ++ alter' (n-1)

--alter :: [Integer] -> [a]
alter n = reverse (alter' n)
