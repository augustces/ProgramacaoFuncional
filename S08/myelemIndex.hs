myelemIndex n xs = myEI n xs 0

myEI _ [] _ = Nothing
myEI n (x:xs) index
    | n == x = Just index
    | otherwise = myEI n xs (index + 1)

main = do
    print $ myelemIndex 4 [1,2,6,4,3] -- == Just 3
    print $ myelemIndex 5 [1,2,6,4,3] -- == Nothing
    print $ myelemIndex 4 [] -- == Nothing
    print $ myelemIndex 1 [1,2,6,4,3] -- == Just 0