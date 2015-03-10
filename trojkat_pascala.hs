silnia :: Integer -> Integer
silnia 0 = 1
silnia 1 = 1
silnia x = x * silnia (x - 1)

trojkatPascala :: Integer -> Integer -> Integer
trojkatPascala _ 0 = 1
trojkatPascala r c = silnia r `div` (silnia c * silnia (r-c))
