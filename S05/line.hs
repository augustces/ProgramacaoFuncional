line 0 = []
line n = [(function n)..(function n + n - 1)]
    where function x = sum [1..(x - 1)] + 1