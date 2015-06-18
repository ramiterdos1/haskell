alt :: [Int]->Bool
alt [x] = True
alt [x,y]|(x>y)= True
         |otherwise = False
alt [x,y,z]|(x>y)&&(y<z)=True
           |otherwise =False
alt (x:y:z:xs)|(x > y)&&(z > y)&&((alt (z:xs)))=True
              |otherwise = False 
alt1 :: [Int] -> Bool
alt1 [x]=True
alt1 [x,y]|(x<y)=True
          |otherwise=False
alt1 [x,y,z]|(y>x)&&(y>z)=True
            |otherwise =False
alt1 (x:y:z:xs)|(y > z)&&(y > x)&&(alt1 (z:xs))=True
               |otherwise = False
alternating :: [Int] -> Bool
alternating [] = True
alternating l |((alt l) || (alt1 l)) = True
	      | otherwise = False
