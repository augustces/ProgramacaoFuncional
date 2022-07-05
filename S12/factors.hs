expoentes base divisor = length $ filtro $ takeWhile (\(a,b) -> a /= 0 && b == 0) $ iterate fn (base, mod base divisor)
    where 
        fn (a,b) = (div a divisor, (div a divisor) `mod` divisor)
        filtro [] = []
        filtro ((a,b):xs)
            | b == 1 = filtro xs
            | otherwise = (a,b) : filtro xs 

ehPrimo n
    | n < 1 = False 
    | n == 1 = True
    | otherwise = foldl (\acc x ->  acc && mod n x /= 0 )True [2.. n - 1]

factors n = fatores n xs
    where 
        fatores a [] = [(a, 1)]
        fatores a (y:ys)
            | a <= 1 = []
            | auxiliar a y > 0 =  (y,auxiliar a y) : fatores (div a (y ^ auxiliar a y)) ys
            | otherwise = fatores a ys
        auxiliar atual fator = expoentes atual fator
        xs = [x | x <- [2..n], ehPrimo x]

main = do
    print $ factors 36 -- == [(2,2),(3,2)]
    print $ factors 50 -- == [(2,1),(5,2)]
    print $ factors 78 -- == [(2,1),(3,1),(13,1)]
    print $ factors 60 -- == [(2,2),(3,1),(5,1)]
    print $ factors 3361743 -- == [(3,4),(7,3),(11,2)]