znajdz_spacje :: [Char] -> Int -> Int
znajdz_spacje a n
	| head a == ' ' = n
	| otherwise = znajdz_spacje (tail a) (n+1)

pobierz_imie :: [Char] -> [Char]
pobierz_imie str = take (znajdz_spacje str 0) str

pobierz_nazwisko :: [Char] -> [Char]
pobierz_nazwisko str = drop ((znajdz_spacje str 0) + 1) str

main = do
	putStrLn "Hello, what's your name?"
	name <- getLine
	putStrLn $ "Your first name is " ++ (pobierz_imie name) ++ " and surname is " ++ (pobierz_nazwisko name)
