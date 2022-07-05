rev n = reverter n 0
    where 
        reverter value y 
            | value == 0 = y
            | otherwise = reverter (div value 10) (function y + mod value 10)
        function x = x * 10

main = do 
    print $ rev 1923 -- == 3291
    print $ rev 123 -- == 321
    print $ rev 39402 -- == 20493
    print $ rev 5 -- == 5