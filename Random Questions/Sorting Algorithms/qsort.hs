quicks [] = []
quicks (x:xs) = a ++ [x] ++ b
      where a = quicks [n | n <- xs, n <= x]
            b = quicks [n | n <- xs, n > x]
