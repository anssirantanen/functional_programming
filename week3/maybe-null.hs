

data MaybeNull a = Null | Some a deriving(Show)
instance (Eq m) =>Eq (MaybeNull m) where
    Some x == Some y = x == y  
    Null == Null = False
    _ == _ = False
