triangle 0 = []
triangle n = triangle (n - 1) ++ [line n]
    where 
        function x = sum [1..(x - 1)] + 1
        line 0 = []
        line n = [(function n)..(function n + n - 1)]