-- By Augusto César
-- exists
exists y xs = foldl function False xs
    where function boolean x = boolean || (if x == y then True else False)

--neib
neib xs ind lim = snd $ foldl function (0, []) xs
    where 
        function (acc, vet) x = if lim >= ind then (func1 acc vet x) else (func2 acc vet x)
        func1 acc vet x = if acc <= (ind + lim) then (acc + 1, vet ++ [x]) else (acc + 1, vet)
        func2 acc vet x = if acc >= (ind - lim) && acc <= (ind + lim) then (acc + 1, vet ++ [x]) else (acc + 1, vet)

--dig2char
dig2char :: Int -> [Char]
dig2char dig
    | dig == 0 = []
    | otherwise = dig2char ((div dig 10)) ++ [toEnum ((mod dig 10 + 48))]

--set
set :: String -> Int -> Int -> String
set xs index value 
    | index == 0 = [toEnum (value + 48)] ++ tail xs
    | index == (length xs - 1) = init xs ++ [toEnum (value + 48)]
    | otherwise = take index xs ++ [toEnum (value + 48)] ++ drop (index + 1) xs

--fit
fit :: (String, Int) ->  Int -> [Int] -> [Bool]
fit (xs, lim) index value = foldl function [] value
    where function vet v = vet ++ [foldl (\x y -> x && if (fromEnum y - 48) == v then False else True) True (neib xs index lim)]

--getHoles
getHoles :: String -> [Int]
getHoles xs = fst $ foldl function ([], 0) xs
    where function (vetPos, index) x = if x == '.' then (vetPos ++ [index], index + 1) else (vetPos, index + 1)

-- Testes
neibTest :: IO ()
neibTest = do
    print $ neib "abcdef.." 0 2 
    print $ neib "abc.def"  3 1 
    print $ neib "abc.def"  3 2 
    print $ neib "abc.def"  1 2 
    print $ neib "abc.def"  5 3 

setTest :: IO ()
setTest = do
    print $ set "12345" 0 9 
    print $ set "12345" 1 9 
    print $ set "12345" 4 9

fitTest :: IO ()
fitTest = do 
    print $ fit ("12.345", 1) 2 [1,2,3,4,5] 
    print $ fit ("12.345", 2) 2 [1,2,3,4,5] 
    print $ fit ("12.345", 3) 2 [1,2,3,4,5] 
    print $ fit ("12345.", 4) 5 [1,2,3,4,5] 

getHolesTest :: IO ()
getHolesTest = do
    print $ getHoles "12.3.."
    print $ getHoles "12.3.4"
    print $ getHoles "...3.4"
