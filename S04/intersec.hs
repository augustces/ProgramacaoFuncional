intersec [] [] = []
intersec lista [] = []
intersec [] lista = []
intersec l1 l2 = [y | y <- l1, (elem y l2) == True]