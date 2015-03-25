elementAt :: [a] -> Int -> a
elementAt [] _ = error "Empty list"
elementAt [x] 1 = x
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n - 1)

elementAt' :: [a] -> Int -> a
elementAt' [] _ = error "Empty list"
elementAt' [x] 0 = x
elementAt' (x:xs) 0 = x
elementAt' (x:xs) n = elementAt' xs (n - 1)
