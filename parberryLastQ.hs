f1 :: Integer->Integer->Integer
f1 x n
   | x<10 = n + x^2
   | otherwise = f1 (div x 10) (n+ (mod x 10)^2)

f2 :: Integer->Integer-> [Integer] 
f2 x 0 = [(f1 x 0)]
f2 x n = z : f2 z (n-1)
     where z = f1 x 0      
