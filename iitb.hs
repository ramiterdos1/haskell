--2

errno :: [Char]->Int
errno x = wbps x 0
  where
  wbps :: [Char] -> Int -> Int
  wbps [] x =x
  wbps (x:xs) n
     | x=='(' = wbps xs (n+1)
     | n>0 && x==')' = wbps xs (n-1)
     | otherwise = wbps xs (n+1)
--3

powers :: Int->[Int]
powers n= [ n^i | i <- [0..] ]

--14summands
--summands :: Int -> [[Int]]
--summands  


--3

where_smallest :: (Int -> Int) -> Int -> Int -> Int 

where_smallest f a b
     | a>b       = findmin (map f [b..a])  + b
     | a==b      = a
     | otherwise = findmin (map f [a..b])  + a

findmin :: [Int]->Int
findmin []  = 0
findmin [x]  = 1
findmin (x:xs) = fmin xs x 0 0

fmin :: [Int]->Int->Int->Int->Int
fmin [] x y n = y
fmin (y:ys) x z n
   | x<y       = fmin ys x z (n+1)
   | otherwise = fmin ys y (n+1) (n+1)

--5
hof a b c [] = b
hof a b c (x:xs) = c (a x) ( hof a b c xs)

--ps1 -> 11.Set
data Elem a = Elem a Bool deriving (Eq,Show)
data Set a = Set [Elem a ] deriving (Eq,Show)

insert :: a -> Set a -> Set a
insert b (Set s) = Set ((Elem b True) : s)
