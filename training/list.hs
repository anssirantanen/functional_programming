module List where

generateDays ::  [(Integer,Integer,Integer)]
generateDays =
    [(x,y,2000)| x <- [1..31], y <- [1..12]]

product str= 
    [x : y : [] | x<- str, y <- str]