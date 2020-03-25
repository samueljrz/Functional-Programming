delete' z xs = (takeWhile (/=z) xs) ++ (tail $ dropWhile (/=z) xs)
