filterMaybe = foldl function []
    where function vet x = case x of
                            Just y -> vet ++ [y]
                            Nothing -> vet

main = do -- filterMaybe
    print $ filterMaybe [Just 5,Nothing,Just 7,Nothing] -- == [5, 7]
    print $ filterMaybe [Just 9, Just 56,Just 11, Just 16] -- == [5, 7]

countNothing = foldl function 0
    where function acc x = case x of
                            Nothing -> acc + 1
                            Just y -> acc

main2 = do -- countNothing
    print $ countNothing [Just 5,Nothing,Just 7,Nothing] -- == 2
    print $ countNothing [Just 5, Just 4, Just 7, Just 12] -- == 0


