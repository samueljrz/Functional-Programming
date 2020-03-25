qsort [] = []
qsort (x:xs) = a ++ [x] ++ b
      where a = qsort [n | n <- xs, n <= x]
            b = qsort [n | n <- xs, n > x]
