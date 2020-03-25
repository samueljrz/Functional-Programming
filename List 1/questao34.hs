sdig :: Int -> Int
sdig 0 = 0
sdig n = if n > 0 
     then (mod n 10) + sdig (div n 10)
     else error"exception"
