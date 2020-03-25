reverso [] = []
reverso xs = [last xs] ++ reverso (init xs)
