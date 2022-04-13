contagem :: [Bool] -> Int
contagem [] = 0 
contagem (x:xs) 
    | (x == False) = 1 + contagem xs
    | otherwise = 0 + contagem xs
paridade [] = False
paridade lista
    | (mod (contagem lista) 2) == 0 = True
    | otherwise = False