import Data.List (unfoldr)
-- recursão 
separaR x
    | div x 10 > 0 = separaR (div x 10) ++ [mod x 10]
    | div x 10 == 0 = [x]
    | otherwise = []

-- Unfoldr
separaU 0 = [0]
separaU n = reverse $ unfoldr fn n
    where fn x = if x /= 0
                then Just (mod x 10, div x 10)
                else Nothing 


main1 = do
    print $ separaR 0 -- == [0]
    print $ separaR 123 -- == [1,2,3]
    print $ separaR 51234 -- == [5,1,2,3,4]

main2 = do
    print $ separaU 0 -- == [0]
    print $ separaU 123 -- == [1,2,3]
    print $ separaU 51234 -- == [5,1,2,3,4]