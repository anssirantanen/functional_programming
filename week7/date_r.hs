--day numbers
m30 = [4,6,8,9,11] 
m31 = [1,3,5,7,8,10,12]
--leap year rules
leapYear y
 | mod y 4 /= 0 = False
 | mod y 100 /= 0 = True
 | mod y 400 /= 0 = False
 | otherwise = True
 --date check for increment
correctDate :: (Int, Int, Int) -> Bool
correctDate (d,m,y)
 | y == 0 = False
 | leapYear y && m == 2 && 0 < d && d < 30 = True
 | not (leapYear y) && m == 2 && 0 < d && d < 29 = True
 | elem m m30 && 0 < d && d < 31 = True
 | elem m m31 && 0 < d && d < 32 = True
 | otherwise = False


nextDate :: (Int, Int, Int) -> (Int, Int, Int)
nextDate (d,m,y)
 | not (correctDate(d,m,y)) = (0,0,0) --on error just return
 | m == 12 && d == 31 = (1,1,y+1)
 | elem m m31 && d == 31 = (1,m+1,y)
 | elem m m30 && d == 30 = (1,m+1,y)
 | leapYear y && m == 2 && d == 29 = (1,3,y)
 | not (leapYear y) && m == 2 && d == 28 = (1,3,y)
 | otherwise = (d+1,m,y)

dateDistance :: (Int, Int, Int) -> (Int, Int, Int, Int) -> Int
dateDistance (d,m,y) (d2,m2,y2,n)
 | d == d2 && m == m2 && y == y2 = n
 | otherwise = dateDistance (nextDate (d,m,y)) (d2,m2,y2,n+1)