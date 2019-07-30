module DBNull where 
data DBNull a = NULL | Some a deriving (Show)

data TriBool = True | False | Unknown deriving (Show)

class TriEq a where 
    (==):: a -> a -> TriBool
    (/=):: a -> a -> TriBool
instance (Eq a) => TriEq (DBNull a) where
    Some x == Some y =boolToTri( x Prelude.== y)
    NULL == NULL = DBNull.False
    _ == _ = Unknown
    Some x /= Some y = boolToTri( x Prelude.== y)
    _ /= _ = Unknown
 
boolToTri :: Prelude.Bool -> DBNull.TriBool
boolToTri a
 | a Prelude.== Prelude.True = DBNull.True
 | a Prelude.== Prelude.False = DBNull.False

