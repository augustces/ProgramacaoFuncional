import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Distribution.Simple.Utils (xargs)
toInt :: String -> Int
toInt x = read x :: Int

distancia xs = foldl condicao [] xs
    where condicao acc ((a,b),c)
            | a < 10 || b < 10 = acc
            | otherwise = acc ++ [(abs (a - b), c)]

tuplas xs = zip (separar xs) [0..]
    where
        separar [] = []
        separar (x:xs) = (a x, b x) : separar xs
        numeros x = words x
        a x = toInt $ head $ numeros x
        b x = toInt $ last $ numeros x

processa :: [String] -> String 
processa xs 
    | null ys = "sem ganhador"
    | otherwise = show $ snd $ foldl function (head ys) ys
    where 
        function (a,b) (c,d) = if c < a then (c,d) else (a,b)
        ys = distancia $ tuplas xs

main :: IO ()
main = do
  print $ processa ["8 11", "10 15"] -- 1
  print $ processa ["9 12", "11 13", "10 11"] -- 2 
  print $ processa ["12 15", "16 14", "10 9"] -- 1
  print $ processa ["12 15", "20 23", "10 9", "35 35"] -- 3
  print $ processa ["10 8", "9 13"] -- sem ganhador
  print $ processa ["8 9", "12 7"] -- sem ganhador
  print $ processa ["10 9", "15 19"] -- 1
  print $ processa ["9 8", "9 12", "12 15", "18 19"] -- 3