rev :: [Int]->[Int]
rev [] = []
rev x = rev1 x []

rev1 :: [Int]->[Int]->[Int]
rev1 [] y = y
rev1 (x:xs) y = rev1 xs (x:y)
