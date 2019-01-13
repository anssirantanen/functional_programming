{-
Write a list comprehension expression that makes a list of all dates of year 2019 using tuples (year, month, day), so the first one is
(2019,1,1) and the last is (2019,12,31)
-}

generateDates :: [(Int,Int,Int)]
generateDates = [(y,m,d) | y <- [2018]::[Int],
                              m <- [1..12],
                              d <- [1..31]]
