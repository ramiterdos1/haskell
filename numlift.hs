lift::(b->b->b) -> (a->b)->(a->b)->a->b
lift oper f g x = oper (f x) (g x)

instance (Num b) => Eq (a->b) where
    _ == _ = False

instance (Num b) => Ord (a->b) where
    _ < _ = False

instance (Num b) => Show (a->b) where
    show f = "Cannot display functions"


instance (Num b) => Num (a->b) where
    (+) = lift (+)
    (-) = lift (-)
    (*) = lift (*)
    abs f x = abs (f x)
    signum f x = signum (f x)
    negate f x = negate (f x)
    fromInteger n = always n
                      where
                        always::(Num b)=>Integer->a->b
                        always m y = (fromInteger m)
