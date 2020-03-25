module Title
(
    split,
    bigChar,
    join,
    title
) where 

split "" = []
split xs
    | w /= "" = [w] ++ (split r)
    | otherwise = split r
    where w = takeWhile (/=' ') xs
          z = dropWhile (/=' ') xs
          r = if length z < 2 
              then []
              else tail z

bigChar ch
    | (length t) > 0 = snd $ t !! 0
    | otherwise = ch
    where t = filter (\(a,b)->a==ch) m
          m = zip ['a'..'z'] ['A'..'Z']

join [] = ""
join (x:xs) = x ++ " " ++ join xs

title xs = join [bWort w | w <- split xs]
    where bWort w = bigChar(head w):tail w
         
