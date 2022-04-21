rotEsq 0 vet = vet
rotEsq n (v:vet) = rotEsq (n - 1) (vet ++ [v])