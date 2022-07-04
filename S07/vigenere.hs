import Data.Maybe (fromMaybe)
import Data.List (elemIndex)
vigenere text key = foldl function [] (zipper text key)
    where 
        function xs (a,b) = xs ++ [fromMaybe ' ' $ int2char (char2int a + char2int b)]
        char2int c = fromMaybe 0 (elemIndex c ['A'..'Z'])
        int2char i = if i > 25 then lookup (i - 26) f else lookup i f
        f = zip [0..] ['A'..'Z']


zipper text key = zip text (foldl function key [1..(length text)])
    where function k _ = key ++ k

main = do
    print $ vigenere "ATACARBASESUL" "LIMAO" -- == "LBMCOCJMSSDCX"
    print $ vigenere "ABACATE" "A" -- == "ABACATE" 
    print $ vigenere "ABACATE" "B" -- == "BCBDBUF" 
    print $ vigenere "ABACATE" "AB" -- == "ACADAUE" 