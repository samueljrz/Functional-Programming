-- Questão 39, lista 1
base 0 _ = []
base x b = r ++ [d]
  where r = base'(div x b) b
        d = (mod x b)

-- Questão 39, lista 1(continuação)
key = ['0'..'9'] ++ ['A'..'Z']

--Questão 39, lista 1(continuação)
converte x b = [key !! q|q <- l]
     where l =  base x b