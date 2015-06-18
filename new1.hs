f:: Int->[[Int]]
f n = f_ n 0
      where
      f_ n y
          | n==0 =[[1]]
          | y==n && n>0 = [[]]
          | y==0 =  [1]: f_ n (y+1)
          | otherwise =( 1:y: [product [(y-x+1)..y] `div` (product [1..x]) | x <- [2..y] ]) : (f_ n (y+1))

pr1 [[]] = return ()
pr1 (x:xs) =
             do
               pr2 x
               putStr "\n"
               pr1 xs
pr2 [] = return ()
pr2 (x:xs) =  
                 do 
                    putStr (show x ++ " ")      
                    pr2 xs
main = do
     
        n <- readLn :: IO Int
        pr1 (f n)
