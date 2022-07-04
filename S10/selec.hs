selec str = foldl function []
    where function acc x = if x < length str then acc ++ [str !! x] else acc

main = do
    print $ selec "abcdef" [0,3,2,3] -- == "adcd"