expoentes base divisor = length $ filtro $ takeWhile (\(a,b) -> a /= 0) $ iterate fn (base, mod base divisor)
    where 
        fn (a,b) = (div a divisor, (div a divisor) `mod` divisor)
        filtro [] = []
        filtro ((a,b):xs)
            | b == 1 = filtro xs
            | otherwise = (a,b) : filtro xs 

main = do
    print $ expoentes 7 2
    print $ expoentes 4 2
    print $ expoentes 8 2
    print $ expoentes 24 2
    print $ expoentes 1024 2
    print $ expoentes 150 5