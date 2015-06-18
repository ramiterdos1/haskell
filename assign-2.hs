type Coefficient=Int
type Exponent=Int
type Polynomial=[(Coefficient,Exponent)]
addpoly::Polynomial->Polynomial->Polynomial
addpoly a []=a
addpoly [] b=b
addpoly ((x,y):xs) ((p,q):qs)|(y==q)=nozero(((x+p),y):(addpoly xs qs))
                             |(y>q)=nozero((x,y):(addpoly xs ((p,q):qs)))
                             |(y<q)=nozero((p,q):(addpoly ((x,y):xs) qs))  

mult::Polynomial->Polynomial->Polynomial
mult a []=[]
mult [] b=[]
mult ((x,y):xs) ((p,q):qs)=(x*p,y+q):(mult ((x,y):xs) (qs))
multpoly::Polynomial->Polynomial->Polynomial
multpoly x []=[]
multpoly [] y=[]
multpoly ((x,y):xs) ((p,q):qs)=nozero (addpoly m n)
  where m=multpoly xs ((p,q):qs)
        n=mult ((x,y):xs) ((p,q):qs)
nozero::Polynomial->Polynomial
nozero []=[]
nozero [(a,b)]| a==0=[]
              | otherwise=[(a,b)]
nozero ((a,b):xs)| a==0=nozero xs
                 | otherwise=(a,b):nozero xs
