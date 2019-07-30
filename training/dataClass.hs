module DataClass where 
   
data MaybeNull a = None | Some a deriving (Show,  Ord)

data TripleEq = True | False | Unknown

instance (Eq m) => Eq (MaybeNull m) where 
    Some t == Some m = t ==m 
    _ == _ = Prelude.False