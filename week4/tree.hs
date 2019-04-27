main =  
    do
    treeOps (return (Nil:: (Tree String)))

treeOps:: IO (Tree String) -> IO (Tree String)
treeOps oldTree = 
    do
        choiseStr <- ask() 
        let choice =  castToInt choiseStr
        t <- case choice of
            1 -> 
               addElem oldTree
            2 ->
                (search oldTree)
        treeOps (return t)

ask () = 
    do   
        putStrLn "1. add tree elem"
        putStrLn "2. search tree elem"
        getLine 

castToInt str = read (str) :: Int        

addElem oldTree =
    do 
        tree <- oldTree
        putStrLn "give stored value"
        value <- getLine
        return (insert tree value)
 
search treeIO =
    do 
        tree <- treeIO
        putStrLn "give search value"
        value <- getLine
        let found = contains tree value
        print ("contains")
        print (found)
        treeIO
data Tree a = Nil | Node (Tree a) a (Tree a) deriving Show
    
empty :: (Ord a) => Tree a -> Bool
empty Nil = True
empty  _  = False
    
contains :: (Ord a) => (Tree a) -> a -> Bool
contains Nil _ = False
contains (Node t1 v t2) x 
        | x == v = True
        | x  < v = contains t1 x 
        | x  > v = contains t2 x
    
insert :: (Ord a) => Tree a -> a -> Tree a
insert Nil x = Node Nil x Nil
insert (Node t1 v t2) x 
    | v == x = Node t1 v t2
    | v  < x = Node t1 v (insert t2 x)
    | v  > x = Node (insert t1 x) v t2