rotDir 0 vet = vet
rotDir n vet = rotDir (n - 1) ([last vet] ++ init vet)