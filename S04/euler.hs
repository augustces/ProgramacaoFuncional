euler1 0 = 0
euler1 1 = 0
euler1 n
    | (mod (n - 1) 5 == 0 || mod (n - 1) 3 == 0) = (n - 1) + euler1 (n - 1)
    | otherwise = 0 + euler1 (n - 1)