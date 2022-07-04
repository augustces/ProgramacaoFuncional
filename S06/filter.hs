myfilter _ [] = []
myfilter f (x:xs)
    | f x = [x] ++ myfilter f xs
    | otherwise = myfilter f xs

main = do
    print $ myfilter (>5) [0..10] -- == [6,7,8,9,10]
    print $ myfilter (odd) [0..10] -- == [1,3,5,7,9]