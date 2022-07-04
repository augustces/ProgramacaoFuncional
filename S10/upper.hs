upper :: Foldable t => t Char -> [Char]
upper = foldl function [] 
    where 
        function strFinal c = case lookup c tuplas of
                                Nothing -> strFinal ++ [c]
                                Just y -> strFinal ++ [y]
        tuplas = zip ['a'..'z'] ['A'..'Z']

main = do
    print $ upper "abc" -- == "ABC"
    print $ upper "a Casa Caiu" -- == "A CASA CAIU"
    print $ upper "tenho 45 ABCs" -- == "TENHO 45 ABCS"