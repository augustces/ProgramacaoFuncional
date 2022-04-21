quad n = foldl qPerf False [1..(n / 2)]
    where qPerf cond x = cond || (n == x*x)