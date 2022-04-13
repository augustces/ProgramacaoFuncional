maior [x] = x
maior (x:xs) 
    | x > last xs = maior( init (x:xs) )
    | otherwise = maior( xs)