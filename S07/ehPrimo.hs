ehPrimo n
    | n < 1 = False 
    | n == 1 = True
    | otherwise = foldl (\acc x ->  acc && mod n x /= 0 )True [2.. n - 1]

main = do
    print $ ehPrimo 1 -- == True
    print $ ehPrimo 2 -- == True
    print $ ehPrimo 10 -- == False
    print $ ehPrimo 13 -- == True