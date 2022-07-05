compac [] = []
compac (x:xs) = compactar x xs 1
    where 
        compactar h [] 1 = [[h]]
        compactar h [] rep = [[h, rep]]
        compactar h (y:ys) rep
            | h == y = compactar y ys (rep + 1)
            | rep > 1 = [[h, rep]] ++ compactar y ys 1
            | otherwise = [[h]] ++ compactar y ys 1

main = do
    print $ compac [] -- == []
    print $ compac [1,1,1] -- == [[1,3]]
    print $ compac [1,2,3] -- == [[1],[2],[3]]
    print $ compac [2,2,2,3,4,4,2,9,5,2,4,5,5,5] -- == [[2,3],[3],[4,2],[2],[9],[5],[2],[4],[5,3]]