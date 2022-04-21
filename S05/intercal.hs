intercal xs ys 
    | (length xs == 0) = ys
    | (length ys == 0) = xs
    | otherwise = [(head xs)] ++ [head ys] ++ intercal (tail xs) (tail ys)

intercal' xs ys = if caso1 then calc1 else calc2
    where
        caso1 = (length xs > length ys)
        calc1 = foldl condicao [] (zip xs (ys ++ [-1,-1..]))
        calc2 = foldl condicao [] (zip (xs ++ [-1,-1..]) ys)
        condicao vet (a,b) 
            | (a /= (-1) && b /= (-1)) = vet ++ [a] ++ [b]
            | (a /= (-1) && b == (-1)) = vet ++ [a]
            | (a == (-1) && b /= (-1)) = vet ++ [b]
            | otherwise = vet
            