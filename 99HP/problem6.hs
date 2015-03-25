isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome x = ((reverse x) == x)
