f :: Integer ->Integer-> Integer ->Integer
f n x 0 = x
f n x n1
  | (n1 `mod` 10 /=0) && (n `mod` (n1 `mod` 10)) == 0 = f n (x+1) (n1 `div` 10)
  | otherwise                  = f n x (n1 `div`10)

f1 :: Int -> IO ()
f1 0 = return () 
f1 t = do
        n <- readLn :: IO Integer
        print (f n 0 n)
        f1 (t-1)
main = do
t<- readLn :: IO Int

