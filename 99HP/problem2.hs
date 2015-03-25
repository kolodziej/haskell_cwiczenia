lastButOne :: [a] -> a
lastButOne [] = error "Empty list"
lastButOne [x] = error "One element"
lastButOne (x:xs)
	| length xs == 1 = x
	| otherwise = lastButOne xs
