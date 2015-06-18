data Duallist a b = Nil | Lista a (Duallist a b) | Listb b (Duallist a b)

instance (Show a, Show b) => Show (Duallist a b) where
	show l = (show "[") ++ (extract l) where
		extract (Lista x Nil) = (show x) ++ (show "]")
		extract (Listb x Nil) = (show x) ++ (show "]")
		extract (Lista x l) = (show x) ++ (show ",") ++ (extract l)
		extract (Listb x l) = (show x) ++ (show ",") ++ (extract l) 

list1 :: Duallist Int Bool 
list1 = Lista 1 (Listb True (Listb False (Lista 9 Nil)))
