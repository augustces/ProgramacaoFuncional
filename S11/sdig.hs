sdig n 
    | n == 0 = 0
    | otherwise = mod n 10 + sdig ( div n 10)

main = do
    print $ sdig 4132 -- == 10
    print $ sdig 328464584658 -- == 63