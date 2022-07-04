mytails [] = [[]]
mytails (x:xs) = [(x:xs)] ++ mytails xs

main = do
    print $ mytails [1] -- == [[1],[]]
    print $ mytails [1,3,5] -- == [[1,3,5],[3,5],[5],[]] 
    print $ mytails [5,3,4] -- == [[5,3,4],[3,4],[4],[]]
