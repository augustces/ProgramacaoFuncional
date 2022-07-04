mynub [] = []
mynub (x:xs)  = mynub' (x:xs) []

mynub' [] _ = []
mynub' (x:xs) ys
    | null ys = [x] ++ mynub' xs [x]
    | elem x ys = mynub' xs ys
    | otherwise = [x] ++ mynub' xs (ys ++ [x])

main = do
    print $ mynub [1,1,1] -- == [1]
    print $ mynub [2,1,2,1,1,1,1,2] -- == [2,1]
    print $ mynub [2,1,2,1,1,1,1,2,3] -- == [2,1,3]
    print $ mynub [1,2,5,2,5,7,2,5] -- == [1,2,5,7]