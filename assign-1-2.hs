largestDivisor::Int->Int
largestDivisor n
        |(n==0) =error "There are infinite divisors"
        |(n>0)&&(divisors n == []) =1
	|n<0 =error "No. is Negetive"
        |otherwise           = div n  (head (divisors n))
	    where
	       divisors::Int->[Int] 
	       divisors n = [d | d <- [2..(n-1)], mod n d == 0]
		                     
