import Data.Maybe (fromMaybe)
import Data.List (intercalate)
titulo :: String -> [Char]
titulo str = intercalate " " (corrigir $ words str)

corrigir :: [String] -> [String]
corrigir [] = []
corrigir (x:xs) =[foldl function [caixaAlta $ head x] (tail x)] ++ corrigir xs
    where
        function vet y = vet ++ [caixaBaixa y]
        caixaAlta c = fromMaybe c $ lookup c (zip ['a'..'z'] ['A'..'Z']) 
        caixaBaixa c = fromMaybe c $ lookup c (zip ['A'..'Z'] ['a'..'z']) 

main = do
    print $ titulo "FuLaNo bElTrAnO silva" -- == "Fulano Beltrano Silva"