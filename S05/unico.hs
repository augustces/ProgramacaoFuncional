unico n xs = condicao
    where 
        condicao = if sum (count) == 1 then True else False 
            where  
                count = [1 | y <- xs, n == y]


-- usando filter
unique x [] = False 
unique x lista = if (length (filter (==x) lista) == 1) then True else False