f :: [char]->Int

f [] = 0
f x:xs = (x-'0')+ (f xs)

g::char->Int
g '0' = 0
g x=g (x-1) +1

