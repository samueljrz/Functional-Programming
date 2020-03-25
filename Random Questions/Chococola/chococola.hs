--chococola :: Int -> Int
--chococola 0 = 0
--chococola 1 = 0
--chococola 2 = 0
--chococola 3 = 1
--chococola x = (x+1) + (garrafas ((x+1)/3) (mod (x+1) 3)) + (chococola (x+1/3)) 

--garrafas :: Int -> Int -> Int
--garrafas 0 _ = 0
--garrafas x y = x + div ((mod x 3)+y) 3





chococola :: Int -> Int
chococola x = x + (garrafas x 1)

garrafas :: Int -> Int -> Int
garrafas 0 _ = 0
garrafas 1 y = (div (1 + y) 3)
garrafas 2 y = (div (2 + y) 3)
garrafas x y = (div x 3) + (div ((mod x 3)+y) 3) + (garrafas (div x 3) ((mod ((mod x 3)+y) 3)+(div ((mod x 3)+y) 3)))  
