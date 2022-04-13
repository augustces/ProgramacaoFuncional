splitints function xs = (filter function xs, filter nFunction xs)
    where
        nFunction x = not (function x)
