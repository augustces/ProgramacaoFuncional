swap [] inicio fim = []
swap [x] inicio fim = [x]
swap xs inicio fim = condicao ++ [xs!!fim] ++ (take (fim - inicio - 1) (drop (inicio + 1) xs)) ++ [xs!!inicio] ++ drop (fim + 1) xs
    where
        condicao
            | inicio == 0 = []
            | otherwise = (take inicio xs)
