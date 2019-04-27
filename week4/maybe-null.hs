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
    | b Prelude.== Prelude.True = MaybeNull.True
    | b Prelude.== Prelude.False = MaybeNull.False

testMeqT () = Some 1 MaybeNull.== Some 1
testMeqF () = Some 1 MaybeNull.== Some 2
testMeqU () = Null MaybeNull.== Some 1