frequencia n [] = 0
frequencia n (x:xs)
    | (x == n) = 1 + frequencia n xs
    | otherwise = frequencia n xs