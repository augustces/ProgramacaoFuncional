deletee n [] = []
deletee n (x:xs) 
    | n == x = xs
    | otherwise = [x] ++ deletee n xs