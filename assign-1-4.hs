descending :: [Int] -> Bool
descending [] = True
descending [x]= True
descending [x,y] | (y <= x) = True
                 | otherwise = False
descending (x:y:xs)|((x >= y) && (descending (y:xs))) = True
                   |otherwise = False
