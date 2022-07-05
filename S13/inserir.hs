inserir x [] = [x]
inserir x (y:ys)
    | x <= y = [x] ++ (y:ys)
    | otherwise = [y] ++ inserir x ys

main = do
    print $ inserir 3 [2,7,12] -- == [2,3,7,12]
    print $ inserir 1 [2,7,12] -- == [1,2,7,12]
    print $ inserir 10 [2,7,12] -- == [2,7,10,12]
    print $ inserir 15 [2,7,12] -- == [2,7,12,15]