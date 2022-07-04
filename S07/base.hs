base n b
    | div n b > 0 = base (div n b) b ++ [text !! (mod n b)]
    | n < b && n /= 0 = [text !! n]
    | otherwise = ""
        where
            text = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"

main = do
    print $ base 25 10 -- == "25"
    print $ base 17 2 -- == "10001"
    print $ base 26 16 -- == "1A"
    print $ base 26012 36 -- == "K2K"