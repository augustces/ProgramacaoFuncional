import Data.List (unfoldr, transpose)
-- gerador 1
a1 = gerador 0
    where gerador n 
            | n > 0 = [n] ++ gerador (n * (-1))
            | otherwise = [n] ++ gerador (n * (-1) + 1)

c1 = concat $ unfoldr fn 0
    where fn n = if n == 0
                    then Just ([0], n + 1)
                    else Just ([n] ++ [n * (-1)], n +1)

-- gerador 2
a2 = gerador 1
    where 
        gerador n
            | odd n = [n] ++ gerador (n + 1)
            | otherwise = [n * (-1)] ++ gerador (n + 1)

c2 = unfoldr fn 1
    where fn n = if odd n
                    then Just (n, n + 1)
                    else Just (n * (-1), n + 1)

-- gerador 3
a3 = gerador 0
    where gerador n = [2^n] ++ gerador (n + 1)

c3 = unfoldr fn 1
    where fn n = if n < 0
            then Nothing 
            else Just (n, n*2)
d3 = iterate (*2) 1

-- gerador 4
a4 n
    | n == 1 = [1]
    | otherwise = [n] ++ a4 (div n 2)

c4 n = unfoldr fn n
    where fn n = if n == 0 
            then Nothing 
            else Just (n, div n 2)