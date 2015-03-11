binsearch :: (Ord a) => [a] -> a -> Int -> Int -> Bool
binsearch [] _ _ _ = False
binsearch a val b e
	| a!!middle > val = binsearch a val b (middle-1)
	| a!!middle < val = binsearch a val (middle+1) e
	| a!!middle == val = True
	where
	middle = b + ((e-b) `div` 2)
