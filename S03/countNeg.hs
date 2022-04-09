main = do
    a <- readLn :: IO [Integer]
    print $ countNeg a

countNeg :: [Integer] -> Integer

countNeg [] = 0
countNeg (x:xs) = countNeg xs + if x < 0 
                    then 1 
                    else 0 

    
