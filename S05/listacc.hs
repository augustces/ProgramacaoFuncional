listacc [] = []
listacc [n] = [n]
listacc (x:xs) = [x] ++ listacc ((x + head xs):(tail xs))