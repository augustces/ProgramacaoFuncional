removerMaior [n] = []
removerMaior xs = [x | x <- xs, x /= maximum (xs)]