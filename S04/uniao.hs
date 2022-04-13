uniao [] [] = []
uniao lista [] = lista
uniao [] lista = lista
uniao l1 l2 = l1 ++ [y | y <- l2, (elem y l1) == False]