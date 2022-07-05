ordenada [] = False
ordenada (x:xs) = test x xs
    where 
        test _ [] = True
        test h (y:ys)
            | h <= y = True && test y ys
            | otherwise = False

qsort ys
    | ordenada ys = ys
    | otherwise = qsort' ys pivo
    where pivo = minimum ys

qsort' [] _ = []
qsort' xs pivo = [pivo] ++ qsort menor ++ qsort maior
    where
        maior = foldl (\acc y -> if pivo < y then acc ++ [y] else acc) [] xs
        menor = foldl (\acc y -> if pivo > y then acc ++ [y] else acc) [] xs

main = do
    print $ qsort [7,3,5,7,8,4] -- == [3,4,5,7,7,8]
    print $ qsort [7,3,5,7,8,4,4] -- == [3,4,4,5,7,7,8]
    print $ qsort [7,3,5,7,8,4,5] -- == [3,4,5,5,7,7,8]