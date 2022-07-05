--MERGE
mymerge xs ys = unir xs ys

main = do
    print $ mymerge [1,3] [7,7,9] -- == [1,3,7,7,9]
    print $ mymerge [7,7,9] [1,3] -- == [1,3,7,7,9]
    print $ mymerge [1,3,5] [4,4,6,7] -- == [1,3,4,4,5,6,7]
    print $ mymerge [4,4,5,6,7] [1,3] -- == [1,3,4,4,5,6,7]

-- ALGORITMO MERGESORT
unir [] ys = ys
unir xs [] = xs
unir (x:xs) (y:ys)
    | x <= y = [x] ++ unir xs (y:ys)
    | otherwise = [y] ++ unir (x:xs) ys

mergesort [x] = [x]
mergesort xs = unir (mergesort left) (mergesort right)
    where
        left = take (div (length xs) 2) xs
        right = drop (div (length xs) 2) xs
