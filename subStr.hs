f_ (xs,ys) 0 0 = (xs,ys)
f_ ((x:xs),ys) 0 j = f_ (xs, (x:ys)) 0 (j-1)
f_ ((x:xs),ys) i j = f_ (xs,ys) (i-1) (j-1) 

f xs i j = reverse( snd (f_ (xs,[])(i-1) j))
