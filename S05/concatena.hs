concatena xs ys = foldl conc ys (reverse xs)
    where conc vet x = (x:vet)