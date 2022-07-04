ehPrimo n
    | n < 1 = False 
    | n == 1 = True
    | otherwise = foldl (\acc x ->  acc && mod n x /= 0 )True [2.. n - 1]

function (a', b') x
    | ehPrimo a' && ehPrimo b' = (a', b')
    | mod a' x == 0 && mod b' x == 0 = function (div a' x, div b' x) x
    | otherwise = function (a',b') (x + 1)

reduce (a, b) = function (a, b) 2

main = do 
    print $ reduce (10,2) -- == (5,1)
    print $ reduce (15,6) -- == (5,2)
    print $ reduce (30,12) -- == (5,2)
    print $ reduce (120,48) -- == (5,2)
    print $ reduce (50,100) -- == (1,2)