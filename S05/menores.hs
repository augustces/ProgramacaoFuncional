menores 0 xs = []
menores 1 xs = [minimum xs]
menores n xs 
    | n >= (length xs) = xs
    | otherwise = menores n [x | x <- xs, x /= maximum (xs)]