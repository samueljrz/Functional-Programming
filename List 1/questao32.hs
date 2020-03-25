isPalind [] = True
isPalind [x] = True
isPalind xs = if head xs == last xs
	then isPalind (tail(init xs))
	else False
