
data KeyVal = KeyVal Int [Int] deriving(Show, Eq)
instance Ord KeyVal where
    compare (KeyVal a1 _) (KeyVal a2 _) =compare a1 a2
extractVal (KeyVal _ v) = v
extractKey (KeyVal x _) = x
data Tree  = Empty  | Node KeyVal (Tree) (Tree) deriving (Show)

insert:: KeyVal -> Tree ->(KeyVal -> KeyVal -> KeyVal)-> Tree 
insert e Empty _ = Node e Empty Empty
insert e (Node ne left right) f
    | e == ne  = Node (f e ne) left right
    | e < ne = Node ne (insert e left f) right
    | e > ne = Node ne left (insert e right f)
search:: Tree -> Int -> Maybe [Int]
search Empty _ = Nothing
search(Node  v left rigth) x 
    | x == extractKey v = Just (extractVal v)
    | x  < extractKey v = search left x 
    | x  > extractKey v = search left x

-- f for on conflict insert
onConflit:: KeyVal -> KeyVal -> KeyVal
onConflit (KeyVal a val1) (KeyVal b val2) =
 (KeyVal a (val1++val2))  
test:: () -> Maybe [Int]
test () = 
    search (Node (KeyVal 1 [1,2,3]) Empty Empty) 1