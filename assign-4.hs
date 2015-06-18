import Char 
     
data Expr = Value Int|
            Add Expr Expr|
            Sub Expr Expr|
            Mult Expr Expr           

instance Show Expr where  
         show (Value x) = (show x)
         show (Add x y) = "("++(show x)++"+"++(show y)++")"
         show (Sub x y) = "("++(show x)++"-"++(show y)++")"
         show (Mult x y) = "("++(show x)++"*"++(show y)++")"

integer :: Char -> Int
integer x = ord (x) - (ord '0')
 
parseExpr :: String -> [Expr]
parseExpr (p:[])= [(Value (integer p))]
parseExpr p
         = [(operator (p!!n) x y)|n <- [1..((length p)-2)],(mod n 2 == 1),x <- parseExpr (take n p),y <- parseExpr (drop (n+1) p)]
  where
   operator :: Char -> Expr -> Expr -> Expr
   operator m x y|(m == '+') = Add x y
                 |(m == '-') = Sub x y
                 |(m == '*') = Mult x y  


remdup :: [Int] -> [Int]
remdup []=[]
remdup (x:xs)|(elem x xs) = remdup xs
             |otherwise = [x]++(remdup xs) 


evalExpr :: String -> [Int]
evalExpr p = remdup[evalterm x|x <- parseExpr p]
   where 
    evalterm :: Expr -> Int
    evalterm (Value x) = x
    evalterm (Add x y) = (evalterm x) + (evalterm y)
    evalterm (Sub x y) = (evalterm x) - (evalterm y)
    evalterm (Mult x y) = (evalterm x) * (evalterm y)
        
          




