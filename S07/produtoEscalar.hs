produtoEscalar [] _ = 0
produtoEscalar xs ys = prodEscalar $ zip xs ys
    where 
        prodEscalar [] = 0
        prodEscalar ((a,b):cs) = (a * b) + prodEscalar cs

main = do
    print $ produtoEscalar [1] [1] -- == 1
    print $ produtoEscalar [1,2,3] [1,1,1] -- == 6
    print $ produtoEscalar [1,2,3] [4,5,6] -- == 32
    print $ produtoEscalar [1,2,3,7] [4,5,6,2] -- == 46