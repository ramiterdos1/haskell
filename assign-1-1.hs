leastTwo :: Int -> Int -> Int -> Int
leastTwo x y z | ((z >= x)&&(z >= y)) = x + y
               | ((x >= y)&&(x >= z)) = y + z
               | ((y >= x)&&(y >= z)) = z + x
