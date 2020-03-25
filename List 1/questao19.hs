--divide :: [a] -> [Integer] -> ([a], [a])
divide [] z = ([], [])
divide xs 0 = ([], xs)
divide xs z = (take z xs, drop ((length xs)-z) xs)
