divide xs 0 = ([], xs)
divide xs n = ((take n xs), drop n xs)