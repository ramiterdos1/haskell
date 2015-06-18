import Data.List
import Data.Char
type Valuation = [Bool]
type TruthTableEntry = ([Bool],Bool)
type TruthTable = [TruthTableEntry]

data Expr = Index Int | And Expr Expr | Or Expr Expr | Neg Expr | Null
   deriving (Ord,Eq)

--evalExpr :: String -> [TruthTable]
show1 :: Expr -> String
show1 (And a1 a2) = "(" ++ show1 (a1) ++ "&&" ++ show1 (a2) ++ ")" 
show1 (Or a1 a2) = "(" ++ show1 (a1) ++ "||" ++ show1 (a2) ++ ")"
show1 (Neg a) = "(!" ++ show1 (a) ++ ")"
show1 (Index a) = "p" ++ show a

instance Show Expr where
  show (And a1 a2) = show1 (a1) ++ "&&" ++ show1 (a2) 
  show (Or a1 a2) = show1 (a1) ++ "||" ++ show1 (a2)
  show (Neg a) = "!" ++ show1 (a) 
  show (Index a) = "p" ++ show a
    
no_Literals :: Expr->Int
no_Literals (Index a) = 1
no_Literals (And a1 a2) = no_Literals (a1) + no_Literals(a2)
no_Literals (Or a1 a2) = no_Literals (a1) + no_Literals(a2)
no_Literals (Neg a) = no_Literals (a)

no_of_Lits :: String -> Int
no_of_Lits x = length(filter (== 'p') x)

oring :: [Int]->[Int]->[Int]
oring [] x = x
oring x [] = x
oring (a:as) (b:bs)= if (a==0 && b==0) then 0:(oring as bs) else 1: (oring as bs) 

--[enter 000000]
no_Diff_Lits :: Expr->[Int]->[Int]
no_Diff_Lits (Index a) x = take (a-1) x ++ [1] ++ drop a x 
no_Diff_Lits (And a1 a2) x = oring (no_Diff_Lits (a1) x) (no_Diff_Lits (a2) x)
no_Diff_Lits (Or a1 a2) x = oring (no_Diff_Lits a1 x) (no_Diff_Lits a2 x)
no_Diff_Lits (Neg a) x = no_Diff_Lits a x

data Stack a = Stack [a] deriving (Eq,Show,Ord)
push :: a -> Stack a -> Stack a
push x (Stack ls) = Stack (x:ls)
pop :: Stack a -> (a, Stack a)
pop (Stack (x:ls)) = (x, Stack ls)
pop_till :: (Ord a) => Stack a->a->[a]->([a],Stack a)
pop_till (Stack (x:ls)) y ans = if (x==y) then (ans, Stack ls) else pop_till (Stack ls) y (x:ans)
isempty :: Stack a -> Bool
isempty (Stack []) = True
isempty _          = False
empty :: Stack a
empty = Stack []  
  
parseExpr :: String -> [Expr]
parseExpr x = map expr_2_Tree (nub $ filter (\y->no_of_Lits x== no_of_Lits y) (gen_exprs x)) 	

data Z a b = C a | E b deriving (Eq,Show,Ord)
get_C :: Z Char b-> Int 
get_C (C a) = ord a - ord '0'
get_E :: Z a b -> b
get_E (E e) = e

--Start with '('s.. always!
expr_2_Tree :: String ->Expr
expr_2_Tree x = expr_2_tree x (empty) 
  where 
      expr_2_tree :: String->Stack (Z Char Expr)->Expr
      expr_2_tree [] st = get_E(fst (pop st))
      expr_2_tree  (x:xs) st = expr_2_tree  xs (tr x st)
        where 
            tr :: Char -> Stack (Z Char Expr) -> Stack (Z Char Expr)
            tr ')' st = push (E (work_on x)) y 
                 where
                    (x,y)= pop_till st (C '(') []
                    work_on :: [Z Char Expr] -> Expr
                    work_on x
                        | length(x) == 6 =
                                 if (x!!2==C '&')
                                 then (And (Index z1) (Index z2)) 
                                 else if(x!!2==C '|')
                                      then (Or (Index z1) (Index z2))	      
                                      else Null            
                        | length(x) == 5 =
                                      if (x!!0 == C 'p')
                                      then  if (x!!2 == C '&')
                                            then (And (Index z1) k)
                                            else (Or (Index z1) k) 
                                      else  if (x!!2==C '&')
                                            then (And k2 (Index z4))
                                            else (Or k2 (Index z4))
                        | length(x) == 4 =
                                        if(x!!1== C '|')
                                        then (Or k2 k3)
                                        else (And k2 k3)
                        | length(x) == 3 = (Neg (Index i1)) 	
                        | length(x) == 2 = (Neg k4) 
                        | otherwise = Null 
                                where
                                  z1=get_C(x!!1)
                                  z2=get_C(x!!5)
                                  i1 = get_C(x!!2)
                                  k =get_E(x!!4)	
                                  z4=get_C(x!!4)
                                  k2 =get_E(x!!0)		
                                  k3 = get_E(x!!3)
                                  k4= get_E(x!!1)
            tr x st = push (C x) st

gen_exprs :: String -> [String]
gen_exprs x = br x 0

br :: String -> Int ->[String]
br [] _ = [] 
br x n 
--        | length(x) == 6 = ['(':(x++")")]
--        | length(x)==7 = ['(':(x++")"),'!':'(':(tail(x)++")")] 
        | length(x) == 3 =['(':(x++")")]
        | length(x) ==2 =  [x]
        | n>=length(x)-1 = []  
        | x!!n=='&' = map (\y->'(':(y++")")) ([z++y|z<- (map (\y->y++"&&") (br (take n x) 0)),y<-((br (drop (n+2) x) 0))])++( br x (n+2))
        | x!!n== '|'= map (\y->'(':(y++")")) ([z++y|z<- (map (\y->y++"||") (br (take n x) 0)),y<-((br (drop (n+2) x) 0))])++( br x (n+2))
        | x!!0=='!' = map (\y->'(':(y++")")) (map ((:) '!') (br (drop (n+1) x) 0))++( br x (n+1))
        | otherwise = br x (n+1)
--bracket :: String ->Int->[String]
--bracket x n
--       | length(x) == 6 || length(x)==7 || length(x) == 3 =['(':(x++')')]
--       | length(x)>7 &&  
        
truth_expr :: Expr->Valuation->[Int]->Bool
truth_expr (Index i) v m= v!!(m!!(i-1)-1)
truth_expr (And e1 e2) v m= (truth_expr e1 v m) && (truth_expr e2 v m)
truth_expr (Or e1 e2) v m= (truth_expr e1 v m) || (truth_expr e2 v m) 
truth_expr (Neg i) v m= not(truth_expr i v m)

mapping :: [Int]->Int->[Int] 
mapping [] _ = []
mapping (x:xs) a = (x+a):mapping xs (a+x)
t_vals:: Expr->Valuation->Bool
t_vals e v= truth_expr e v (mapping (no_Diff_Lits e [0,0,0,0,0,0]) 0)   
							            											
generate_TT :: Int -> [[Bool]]																
generate_TT 0 = [[]]
generate_TT n = (map ((:) False) (generate_TT (n-1))) ++ (map ((:) True) (generate_TT (n-1)))

diff_lits :: Expr->Int 	
diff_lits e =length (filter (==1) (no_Diff_Lits e [0,0,0,0,0,0]))															  	 
eval_for_all:: Expr->TruthTable
eval_for_all e = [(x,(t_vals e x)) | x<-generate_TT (diff_lits e)]

evalExpr :: String -> [TruthTable]
evalExpr [] = []
evalExpr x = nub (map (eval_for_all) (parseExpr x))