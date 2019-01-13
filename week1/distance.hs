distance:: (Int,Int,Int) -> (Int,Int,Int) -> Int -> Int
distance f t l 
    | f == t = l
    | otherwise = distance (nextDate f) t (l+1)

nextDate:: (Int,Int,Int) -> (Int, Int,Int)
nextDate (y,m,d)
    | d+1 > 31 = case m+1 > 12 of
        True -> (y+1,1,1)
        False -> (y,m+1,1)
    | otherwise =  (y, m, d+1)