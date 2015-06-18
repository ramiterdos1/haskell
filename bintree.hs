data BinTr a = Nil | BinTr (BinTr a) a (BinTr a) 
    deriving (Show, Eq, Ord)

insert ::Ord a=> BinTr a->a -> BinTr a

insert Nil y = BinTr Nil y Nil
insert (BinTr tl x tr) y 
    | y<x = BinTr (insert tl y) x tr
    |otherwise = BinTr tl x (insert tr y)

createTree ::Ord a=> [a] -> BinTr a
createTree []= Nil
createTree x=foldl insert Nil x

search  :: Ord b=>BinTr b -> b -> Bool

search (BinTr tl x tr) a 
     | x==a = True
     | a<x = if tl/=Nil then search tl a else False
     | otherwise = if tr/=Nil then search tr a else False

delete ::Ord a=>BinTr a -> a -> BinTr a
delete Nil x = Nil
delete (BinTr Nil x Nil) a = Nil 
delete (BinTr (BinTr tl a1 tr) a Nil) x 
     | a==x = BinTr tl a1 tr
     | otherwise =BinTr (delete (BinTr tl a1 tr) x) a Nil

     
delete (BinTr tl x tr) a 
  | a<x = BinTr (delete tl a) x tr
  | a>x = BinTr tl x (delete tr a)
  | a==x = BinTr tl z1 z2
        where 
            z1 = findMin tr
            z2 = delete tr z1
findMin ::Ord a=> BinTr a -> a
findMin (BinTr Nil a tr) = a
findMin (BinTr tl a tr) = findMin tl 


retRoot :: BinTr a -> a
--retRoot Nil = Nil
retRoot (BinTr tl x tr) = x

retLeft :: BinTr a -> BinTr a
retLeft Nil = Nil
retLeft (BinTr tl x tr) = tl

retRight :: BinTr a -> BinTr a
retRight Nil = Nil
retRight (BinTr tl x tr) = tr

height :: BinTr a -> Int
height Nil = 0
height (BinTr tl x tr) 
  | htl <= htr = htr+1
  | otherwise = htl+1
  where 
    htl = height tl
    htr = height tr

inorder :: BinTr a -> [a]
inorder Nil =[]
inorder (BinTr tl x tr) = (inorder tl) ++ (x: inorder tr)

