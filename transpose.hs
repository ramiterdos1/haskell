transpose :: [[a]]->[[a]]

transpose l = applyEach z l
   where 
     z = [map (!!i)| i<-[0..((length l)-1)]]
     applyeach [] l =[]
     applyEach (x:xs) l = x l : applyEach xs l 
