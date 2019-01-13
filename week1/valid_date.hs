validDate:: (Int,Int,Int) -> Bool
validDate (y,m,d) = validMonth m && validDay d



validMonth:: Int -> Bool
validMonth m = m<= 12 && m>= 1

validDay :: Int -> Bool 
validDay d = d<= 31 && d>= 1