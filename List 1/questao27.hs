rotEsq 0 xs = xs
rotEsq z xs = drop z xs ++ take z xs
