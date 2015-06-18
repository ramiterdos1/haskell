f xs = (g xs (10^((length xs)-1)))
g ys 1 = (read (head ys):: Float)
g ys d = (read (head ys):: Float)*d + (g (tail ys) (d/10))
