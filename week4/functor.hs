module MaybeNull (MaybeNull(..), ThreeQuality(..))
where


data MaybeNull a = Null | Some a deriving(Show)
data ThreeQuality = True | False | Unknown deriving(Show)
class TEq a where  
    (==) :: a -> a -> ThreeQuality  
    (/=) :: a -> a -> ThreeQuality  

instance (Eq m) => TEq (MaybeNull m) where
    Some x == Some y = boolToEq(x Prelude.== y)
    _ == _ = Unknown
    Some x /= Some y = boolToEq(x Prelude.== y)
    _ /= _ = Unknown

boolToEq b 
    | Prelude.True = MaybeNull.True
    | Prelude.False = MaybeNull.False

instance Functor MaybeNull where
    fmap f (Some a) = Some (f a)
    fmap f Null = Null

testMeqT () = Some 1 MaybeNull.== Some 1
testMeqF () = Some 1 MaybeNull.== Some 2
testMeqU () = Null MaybeNull.== Some 1

testS () = fmap (*2) (Some 2)
testN () = fmap (*2) (Null)