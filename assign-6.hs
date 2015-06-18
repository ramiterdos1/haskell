import Char

str2int :: Char -> Int
str2int x = ord (x) - ord( '0')

parseexpr :: String -> [(Int,String)] -> ((Int,String),[(Int,String)])
parseexpr p m
	|length p == 1 = ((str2int (p!!0),p),((str2int (p!!0)),p):m)
	|(find p m 0)/=(-1)= (m!!(find p m 0),m)
	|otherwise = ((maxm l),((maxm l):((minm l):m)) )
	where
	   l=[((operator (fst (fst (parseexpr (take n p) m))) (p !! n) (fst (fst (parseexpr (drop (n+1) p) m)))),"("++snd (fst (parseexpr (take n p) m))++[p!!n]++snd (fst (parseexpr (drop (n+1) p) m))++")")| n <- [1,3..((length p)-2)]]

operator :: Int -> Char -> Int -> Int 
operator x m y
            |(m == '+') = x+y
            |(m == '*') = x*y
            |(m == '-') = x-y

maxval :: String->(Int,String)
maxval expr=fst (parseexpr expr [])

find :: String -> [(Int,String)]->Int->Int
find p m i
	|m==[] = -1
	|snd (head m)==p = i
	|otherwise = find p (tail m) i+1

maxm :: [(Int,String)]->(Int,String)
maxm []=(0,"0")
maxm (l:[])=l
maxm (l:ls)| fst l>fst g = l
	|otherwise = g
		where g=maxm ls

minm :: [(Int,String)]->(Int,String)
minm []=(0,"0")
minm (l:[])=l
minm (l:ls)| fst l<fst g = l
	|otherwise = g
		where g=minm ls
