import Data.Char
import Data.Array
import Data.List
chk x =  ((ord x >= ord ('A')) && (ord x <=ord 'Z')) || ((ord x >= ord ('a')) && (ord x <=ord 'z')) 
chk_ x = ((ord x >= ord 'A') && (ord x <=ord 'Z')) || ((ord x >= ord ('a')) && (ord x <=ord 'z')) || ((ord x >=ord '0') && (ord x <=ord '9') )
keywrds [] = False
keywrds (x:xs) =chk x && foldr (&&) True (map chk_ xs)

mywords :: String-> [String]
mywords [] = []
mywords l = (takeWhile (\x->x/=' ') l) : mywords (dropWhile (\x->x==' ') (dropWhile (\x-> x/=' ') l))


remainders n = takeWhile (\x->x/=n) (map (\(x,i)->mod x (10^i)) [(n,i)| i<-[1..]])

numberparts n = takeWhile (\x->x/=0) (map (\(x,i)->div x (10^i)) [(n,i)| i<-[0..]])

rotate [] = []
rotate (x:xs) = xs++[x]

rotate_times x 0 = x
rotate_times x n = rotate_times (rotate x) (n-1)

keywords [] = []
keywords x = filter (\x->keywrds x == True) (mywords x)

nextab [] =0
nextab ('a':'b':xs) =1+nextab xs
nextab (x:xs) = nextab xs


perm [] = [[]]
perm x=[y:ys| y<-x, ys<-perm( delete y x)] 

{-
nextPermutation []=[]
nextPermutation (x:y:xs)
  -}

data OTree a= Node a [OTree a]

inorder:: OTree a -> [a]
inorder (Node a []) = [a] 
inorder (Node a (x:xs)) = inorder(x) ++ [a] ++ concat (map inorder xs)

dep :: OTree a -> Int

dep (Node a []) =1
dep (Node a l) = 1 + maximum (map dep l)
depth :: OTree a -> [Int]
depth (Node a l) = dep (Node a l) : map dep l


depths :: OTree a ->Int->[Int]
depths (Node a []) x = [x] 
depths (Node a l) x = concat (map (\y->depths y (x+1)) l )

{-
values :: [String]->String->[String]
values (x) [y] = [y:z|z<-x]
values (x:xs) (y:ys) = values [y
-}
{-
minsum:: [Int]-> (Int,[Int])

minsum x = if (length x) < 3 then min3 x else min_ (listArray (1,length x) x) (z+1) [z] where z= min3 (take 3 x)
min_ :: Array Int Int-> Int -> [Int]-> (Int,[Int])
min_ [] a l = (a,l)
min_  t a x = if 
-}
min3 :: [Int] ->Int-> (Int,Int)
min3 [x,y,z] a
     | z<=x && z<=y = (z,a+2)
     | y<=z && y<=x = (y,a+1)
     | otherwise = (x,a) 
minA :: [Int]->Int->[(Int,Int)]
minA [x,y] a =  []  
minA (x:l) a = (min3 (take 3 (x:l)) a) : minA l (a+1)
rev = foldl (flip (:)) []
minsum :: [Int]->(Int,[Int])
minsum l = minsum_ (tail z) x [y]
    where 
          z = minA l 1
          (x,y) = head z
minsum_:: [(Int,Int)]->Int->[Int]->(Int,[Int])
minsum_  [(x,y)] sum l = if (y/=head l) then (sum+x, rev (y:l)) else (sum, rev l) 
minsum_  ((x,y):z) a l 
        | (y-(head l))>=3 = minsum_ z (a+x) (y:l)
        | otherwise = minsum_ z a l

minout ls = firstZero 0 
   where
        m = length ls 
        f x y=y
        myArray = accumArray f 0 (0,m) [(i,1) | i <- ls, 0 <= i, i <= m]
        firstZero :: Int -> Int
        firstZero i
            | (myArray!i == 0) = i
            | otherwise = firstZero  (i+1)

takenum:: Int-> IO Int
takenum a= do
           putStrLn "Enter number!"
           n<-readLn :: IO Int
           return (a+n)

readlist :: [Int] -> IO [Int]
readlist l = do
                inp <- (readLn :: IO Int)
                if (inp == -1)
                 then (return l)
                 else readlist (inp:l)

readwords:: [String]->IO [String]
readwords l =do
               inp<-getLine
               if(inp=="") then return l
               else readwords (inp:l)
