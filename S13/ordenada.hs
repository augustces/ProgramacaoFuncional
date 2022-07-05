ordenada [] = False
ordenada (x:xs) = test x xs
    where 
        test _ [] = True
        test h (y:ys)
            | h <= y = True && test y ys
            | otherwise = False

main = do
    print $ ordenada [1,2,3,5] -- == True
    print $ ordenada [1,0,2,5] -- == False
    print $ ordenada [1,2,3,2] -- == False
    print $ ordenada [1,2,2,2,5] -- == True