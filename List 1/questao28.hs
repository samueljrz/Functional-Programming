rotDir 0 xs = xs
rotDir n xs = drop (length xs - n) xs ++ take (length xs - n) xs
