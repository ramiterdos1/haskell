import Data.Char
f :: Int -> Int -> String

f n x 
   | x==0 =
       if n==1
       then  "One"
       else  if n==2
             then l
             else if n==3
                  then "Three"
                  else z
   | otherwise = "too"
                    where
                       z="ko"
                       l="Two"
 
--f 5 0 ="f"
z [] = 0
z ('p':x:xs) = ord (x) - ord '0'
z (x:xs) = z xs

data Z a b = C a | E b
      deriving Show
data S a = S [a]
      deriving Show
get_a :: S a -> [a]
get_a (S a) = a

data Stack a = Stack [a] deriving (Ord,Eq,Show)
push :: a -> Stack a -> Stack a
push x (Stack ls) = Stack (x:ls)
pop :: Stack a -> (a, Stack a)
pop (Stack (x:ls)) = (x, Stack ls)
pop_till ::(Ord a) => Stack a->a->[a]->([a],Stack a)
pop_till (Stack (x:ls)) y ans = if (x==y) then (ans, Stack ls) else pop_till (Stack ls) y (x:ans)
isempty :: Stack a -> Bool
isempty (Stack []) = True
isempty _          = False
empty :: Stack a
empty = Stack [] 
br :: String -> Int ->[String]
br [] _ = [] 
br x n 
--        | length(x) == 6 = ['(':(x++")")]
--        | length(x)==7 = ['(':(x++")"),'!':'(':(tail(x)++")")] 
        | length(x) == 3 =['(':(x++")")]
        | length(x) ==2 =  [x]
        | n>=length(x)-1 = []  
        | x!!n=='&' = map (\y->'(':(y++")")) ([z++y|z<- (map (\y->y++"&&") (br (take n x) 0)),y<-((br (drop (n+2) x) 0))])++( br x (n+2))
        | x!!n== '|'= map (\y->'(':(y++")")) ([z++y|z<- (map (\y->y++"||") (br (take n x) 0)),y<-((br (drop (n+2) x) 0))])++( br x (n+2))
        | x!!0=='!' = map (\y->'(':(y++")")) (map ((:) '!') (br (drop (n+1) x) 0))++( br x (n+1))
        | otherwise = br x (n+1)

