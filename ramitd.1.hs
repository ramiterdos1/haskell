import Data.Char
--Q1
leastTwo :: Int -> Int -> Int -> Int

leastTwo a b c = (a + b + c) - max_ a b c

max_ :: Int -> Int -> Int -> Int
max_ a b c 
  | a>=b && a>=c = a
  | b>=a && b>=c = b
  | otherwise    = c 

--Q2
descending :: [Int] -> Bool

descending [] = True
descending [x] = True
--descending (x:y:[]) = x>=y
descending (x:y:ys) = (x>=y) && (descending (y:ys))

--Q3
repli :: [Int] -> Int -> [Int]

repli _ 0 = []
repli [] _ = []
repli (x:xs) n = take n ( map (* x) [1,1..]) ++ repli xs n

--Q4
charsToNum :: [Char] -> Int
charToDigit :: Char -> Int
charToDigit x
  | x>='0' && x<='9' = ord x - ord '0'
  | otherwise        = 0

charsToNum [] = 0
charsToNum (x:xs) = (charToDigit x)*( product (take (length xs) [10,10..])) + charsToNum xs

--Q5
pack :: [Int] -> [[Int]]

pack [] = []
pack (x:xs) = pack_ [x] xs

pack_ :: [Int] -> [Int] -> [[Int]] 
pack_ x [] = [x]
pack_ x (y:ys) 
  | y==(head x) = pack_ (y:x) ys
  | otherwise   = x:(pack_ [y] ys) 

pck :: [Int] -> [[Int]]

pck [] = [] 
pck [x] = [[x]]
pck (x:y:xs) = if x==y then [x:head (pck (y:xs))]++tail (pck (y:xs)) else ([[x]] ++ pck (y:xs))

