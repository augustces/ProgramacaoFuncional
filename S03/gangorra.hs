calculo x y = x * y
gangorra a b c d =
    if (calculo a b) == (calculo c d) then 0
    else if (calculo a b) > (calculo c d) then (-1)
    else 1