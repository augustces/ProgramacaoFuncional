
elemento 0 xs = head xs
elemento ind xs 
    | ind > 0 = xs !! ind
    | ind < 0 = xs !! (length xs + ind)

-- recursão
elementoR 0 xs = head xs
elementoR indi (element:lista) = elementoR (condicao) lista
    where
        condicao
            | indi > 0 = (indi - 1)
            | otherwise = (length (lista) + indi)