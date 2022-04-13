sublist inicio fim lista 
    | (inicio >= 0 && fim >= 0) = take (fim - inicio) (drop inicio lista)
    | (inicio >= 0 && fim < 0) = take ((length lista) + fim - inicio) (drop inicio lista)
    | (inicio < 0 && fim >= 0) = take (fim - ((length lista) + inicio) ) (drop (length lista + inicio) lista)
    | otherwise = take ((length lista) + fim - ((length lista) + inicio) ) (drop (length lista + inicio) lista)