module Dates(    
    Date(..),
    correctDate,
    nextDate,
    leapYear
)where

data Date = Date{
            year :: Int,
            month :: Int,
            day :: Int
            } deriving(Show,Eq,Ord)

leapYear y 
  | mod y 400 == 0 = True
  | mod y 100 == 0 = False
  | mod y 4 == 0 = True
  | otherwise = False

correctDate :: Date -> Bool
correctDate (Date 0 _ _)= False
correctDate date
 | elem (month date) [1,3,5,7,8,10,12] && elem (day date) [1..31] = True
 | elem (month date) [4,6,9,11] && elem (day date) [1..30] = True
 | (month date) == 2 && elem (day date) [1..28] = True
 | leapYear (year date) &&  (month date)==2 && (day date)==29 = True
 | otherwise = False

nextDate :: Date -> Date
nextDate date
 | correctDate date{day=((day date)+1)} = date{day=((day date)+1)}
 | correctDate date{month=((month date)+1), day=1} = date{month=((month date)+1), day=1}
 | (year date)==(-1) = Date{year=1,month=1,day=1}
 | otherwise = Date{year=((year date)+1),month=1,day=1}

nextDate':: Date -> Date
nextDate' date
 | correctDate incrDay = incrDay
 | correctDate incrMonth = incrMonth
 | (year date)==(-1) = Date{year=1,month=1,day=1}
 | otherwise = Date{year=((year date)+1),day=1,month=1}
 where incrDay = date{day =((day date)+1)}
       incrMonth = date{month=((month date)+1),day=1}


dateDistance date1 date2 
 | not (correctDate date1) = error "incorrect first date"
 | not (correctDate date2) = error "incorrect second date"
 | date1 == date2 = 0
 | date1 < date2 = addDate1Until0 date1 date2 
 | date1 > date2 = addDate1Until0 date2 date1 

addDate1Until0 date1 date2
 | date1 == date2 = 0
 | otherwise = 1 + (addDate1Until0 (nextDate date1) date2)