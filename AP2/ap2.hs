import Data.Maybe
import Data.List
-- By Augusto César

-- exists
exists :: Eq a => a -> [a] -> Bool
exists y xs = foldl function False xs
    where function boolean x = boolean || ( x == y)

--neib
neib :: [a] -> Int -> Int -> [a]
neib xs ind lim = snd $ foldl function (0, []) xs
    where 
        function (acc, vet) x = if lim >= ind then func1 acc vet x else func2 acc vet x
        func1 acc vet x = if acc <= (ind + lim) then (acc + 1, vet ++ [x]) else (acc + 1, vet)
        func2 acc vet x = if acc >= (ind - lim) && acc <= (ind + lim) then (acc + 1, vet ++ [x]) else (acc + 1, vet)

--dig2char
dig2char :: Int -> Char
dig2char dig = toEnum (dig + 48)

--set
set :: String -> Int -> Int -> String
set xs index value 
    | index == 0 = [dig2char value] ++ tail xs
    | index == (length xs - 1) = init xs ++ [dig2char value]
    | otherwise = take index xs ++ [dig2char value] ++ drop (index + 1) xs

--fit
fit :: (String, Int) ->  Int -> Int -> Bool
fit (xs, lim) index value = foldl (\x y -> x && (fromEnum y - 48) /= value) True (neib xs index lim)

--getHoles
getHoles :: String -> [Int]
getHoles xs = fst $ foldl function ([], 0) xs
    where function (vetPos, index) x = if x == '.' then (vetPos ++ [index], index + 1) else (vetPos, index + 1)

--solve
solve :: (String, Int) -> [Int] -> Int -> Maybe String
solve (xs, lim) holes hindex 
    | hindex == length holes = Just xs
    | possible == [] = solve (xs, lim) holes (hindex - 1)
    | otherwise = solve(set xs (holes !! hindex) (head possible), lim) holes (hindex + 1)
    where
        possible = [ y | y <- [0..lim], fit (xs, lim) (holes !! hindex) y]

--mainSolver
mainSolver :: String -> Int -> String
mainSolver xs lim = fromJust $ solve (xs, lim) (getHoles xs) 0

--main
main :: IO ()
main = do
    xs <- getLine
    lim <- readLn :: IO Int
    putStrLn $ mainSolver xs lim

{- Testes -}

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
    print $ map (fit ("12.345", 1) 2) [1,2,3,4,5] 
    print $ map (fit ("12.345", 2) 2) [1,2,3,4,5] 
    print $ map (fit ("12.345", 3) 2) [1,2,3,4,5] 
    print $ map (fit ("12345.", 4) 5) [1,2,3,4,5] 

getHolesTest :: IO ()
getHolesTest = do
    print $ getHoles "12.3.."
    print $ getHoles "12.3.4"
    print $ getHoles "...3.4"

mainTest :: IO ()
mainTest = do
    print $ mainSolver "01.2." 3 == "01320"
    print $ mainSolver ".0..231..5" 5 == "1045231045"
    print $ mainSolver "2..0..............3..........." 3 == "213021302130213021302130213021"
    print $ mainSolver "0..32..41." 5 == "0413250413"
    print $ mainSolver "9....7.620.5318....." 9 == "95318746209531874620"