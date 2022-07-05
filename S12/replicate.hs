myreplicate x n
    | x == 0 = []
    | otherwise = myreplicate (x - 1) n ++ [n]

main = do
    print $ myreplicate 4 0 -- == [0, 0, 0, 0]
    print $ myreplicate 2 True -- == [True, True]
    print $ myreplicate 3 "banana" -- == ["banana", "banana", "banana"]