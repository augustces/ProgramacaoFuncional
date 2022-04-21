maioresQue' n [] = []
maioresQue' n (x:xs) 
    | x > n = [x] ++ maioresQue' n xs
    | otherwise = maioresQue' n xs
maioresQue n xs = [x | x <- xs, n < x]