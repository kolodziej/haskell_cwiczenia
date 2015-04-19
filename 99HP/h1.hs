funk :: (Eq a) => [a] -> [(a, a)]
funk [] = []
funk [x] = [(x, x)]
funk (x:y:xs) = (x, y) : funk xs
