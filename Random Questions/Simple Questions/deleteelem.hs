delement xs z = (takeWhile (/=z) xs) ++ (tail $ dropWhile (/=z) xs)
